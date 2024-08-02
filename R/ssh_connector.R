#' SSH connector
#'
#' This connector allows to read data from SSH remote server during `teal` session.
#'
#' @inheritParams teal.data::teal_data
#' @param data (`teal_data`) object. App developer can pass `data` as `teal_data` object with
#'  some initial data.
#' @param host (`character`) an ssh server string of the form `hostname[:port]`.
#' An `ipv6` hostname should be wrapped in brackets like this: `[2001:db8::1:80]`.
#' @param paths (named `list` of file paths) the paths to the files containing the
#' data on the remote server. The names of the list will define the data names
#' that will be used to store the data on the `teal.data` object.
#' @param read_expression (quoted `call` or `expression`) a call that will be
#' used by the connector to read the contents of the file.
#' This `call` can be created with [quote()], [substitute()] or [expression()] functions.
#' Note that the `path` variable is defined in the connector code and should be
#' used in the expression.
#' It defaults to `utils::read.csv(file = path, header = TRUE)`.
#'
#' When complex expression are being used, we recommend to use [expression()] with
#' with each line being a separate argument. See example below.
#' @param title (`shiny.tag`) a title for the connector.`
#'
#' @return object of class `teal_data_module`.
#'
#' @examples
#' library(teal)
#' x <- ssh_connector(
#'   paths = list(ADSL = "/path/to/ADSL.csv", ADTTE = "/path/to/ADTTE.csv")
#' )
#'
#' app <- init(data = x, modules = list(example_module()))
#'
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
#'
#' # Example with host defined explicitly
#' x <- ssh_connector(
#'   paths = list(ADSL = "/path/to/ADSL.csv", ADTTE = "/path/to/ADTTE.csv"),
#'   host = "localhost",
#'   read_expression = quote(utils::read.csv(file = path, header = TRUE))
#' )
#'
#' app <- init(data = x, modules = list(example_module()))
#'
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
#'
#' # Example with complex expression
#' x <- ssh_connector(
#'   paths = list(ADSL = "/path/to/ADSL.csv", ADTTE = "/path/to/ADTTE.csv"),
#'   read_expression = expression(
#'     read_custom = function(file) utils::read.csv(file = file, header = TRUE),
#'     read_custom(path)
#'   )
#' )
#'
#' app <- init(data = x, modules = list(example_module()))
#'
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
#' @export
ssh_connector <- function(data = teal.data::teal_data(),
                          join_keys = teal.data::join_keys(),
                          host = NULL,
                          paths = list(),
                          read_expression = quote(utils::read.csv(file = path, header = TRUE)),
                          title = shiny::h2(shiny::code("teal"), " - Access data from SSH")) {
  checkmate::assert_class(data, "teal_data")
  checkmate::assert_class(join_keys, "join_keys")
  checkmate::test_class(read_expression, "call")
  checkmate::assert_string(host, null.ok = TRUE)
  checkmate::assert_list(paths, names = "named", min.len = 1, types = "character")
  checkmate::assert_class(title, "shiny.tag")

  dataset_code <- mapply(
    function(path, dataname) {
      read_expression <- c(read_expression, expression()) # Trick to allow splicing on bquote
      bquote(
        # Downloads file to a temporary directory and allows inclusion
        # of code in "Show R Code" without @linksto
        .(as.name(dataname)) <- withr::with_tempdir({
          path <- .(path)
          downloaded <- ssh::scp_download(
            ssh_session,
            path,
            to = "."
          )
          path <- basename(path) # read from local directory
          ..(read_expression)
        }),
        splice = TRUE
      )
    },
    path = unname(paths),
    dataname = names(paths),
    SIMPLIFY = FALSE
  )
  names(dataset_code) <- names(paths)

  tdm <- teal::teal_data_module(
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shinyjs::useShinyjs(),
        shiny::singleton(
          shiny::tags$head(
            shiny::includeCSS(system.file("css/error.css", package = "teal.connectors.ssh"))
          )
        ),
        title,
        ssh_connect_ui(ns(ssh_module_name()), host = host),
        shiny::div(
          shiny::tags$h5(
            shiny::tags$i(class = "fa-solid fa-circle-info"),
            "Name of datasets and respective remote path:"
          ),
          shiny::tags$ul(
            lapply(names(paths), function(dataname) {
              shiny::tags$li(
                shiny::tags$code(dataname),
                shiny::tags$span(":", .noWS = "before"),
                shiny::tags$code(paths[[dataname]])
              )
            })
          )
        ),
        shiny::actionButton(ns("submit"), label = "Load data"),
        shiny::uiOutput(ns("error_helper"))
      )
    },
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        # When app developer defines the host, it cannot be changed by the user
        if (!is.null(host)) shinyjs::disable(id = shiny::NS(ssh_module_name(), "host"))

        # Establish the connection
        ssh_tdata <- shiny::eventReactive(input$submit, {
          within(
            data,
            {
              ssh_args <- teal.connector.ssh::ssh_authenticator(host = host)
              ssh_session <- ssh_args$session
            },
            host = host
          )
        })

        # Read data for every path
        tdata <- shiny::reactive({
          new_tdata <- ssh_tdata()

          failed_files <- list()
          if (!checkmate::test_class(new_tdata, "qenv.error")) {
            failed_tdata <- NULL
            for (code_ix in seq_along(dataset_code)) {
              if (is.null(failed_tdata)) {
                new_tdata <- teal.code::eval_code(new_tdata, dataset_code[[code_ix]])
              } else {
                # Faster way of checking if files exist without having to download
                new_tdata <- teal.code::eval_code(
                  new_tdata,
                  substitute(
                    {
                      # Throw error if file does not exist in remote host
                      stopifnot(
                        0 == ssh::ssh_exec_wait(ssh_session, command = sprintf("[[ -f %s ]]", path))
                      )
                    },
                    list(path = paths[[code_ix]])
                  )
                )
              }
              if (checkmate::test_class(new_tdata, "qenv.error")) {
                failed_tdata <- failed_tdata %||% new_tdata # Keep first occurrence of error
                failed_files[[names(dataset_code)[[code_ix]]]] <- paths[[code_ix]]
                # Allow other files to be downloaded and check if they exist
                new_tdata <- ssh_tdata()
              }
            }
            new_tdata <- failed_tdata %||% new_tdata
          }

          if (checkmate::test_class(new_tdata, "teal_data")) {
            teal.data::datanames(new_tdata) <- union(teal.data::datanames(new_tdata), names(paths))
            teal.data::join_keys(new_tdata) <- c(teal.data::join_keys(data), join_keys)
          }

          list(tdata = new_tdata, failed_files = failed_files)
        })

        # Disconnect from SSH session
        final_tdata <- shiny::reactive({
          if (checkmate::test_class(tdata()$tdata, "qenv.error")) {
            # Force ssh session to close if there are errors reading files
            within(ssh_tdata(), ssh::ssh_disconnect(ssh_session))
            tdata()$tdata
          } else {
            within(
              tdata()$tdata,
              {
                ssh::ssh_disconnect(ssh_session)
                rm(ssh_session)
              }
            )
          }
        })

        # Display error helper
        output$error_helper <- shiny::renderUI({
          shiny::req(tdata())
          if (checkmate::test_class(tdata()$tdata, "qenv.error")) {
            error_ui(
              failed_files = tdata()$failed_files,
              connection_error = checkmate::test_class(ssh_tdata(), "qenv.error")
            )
          }
        })

        final_tdata
      })
    }
  )
}

#' UI for connector
#'
#' @param host (`character(1)`) optional host definition that forces this
#' option on the user. If it is not defined, then user can define themselves.
#'
#' @return a [shiny::tagList] with UI definition.
#'
#' @keywords internal
ssh_connect_ui <- function(id, host = NULL) {
  checkmate::assert_string(id)
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::tags$div(
            class = "auth_container",
            shiny::tags$h5("SSH authentication"),
            shiny::textInput(
              ns("host"),
              "Host",
              value = host %||% "",
              placeholder = "hostname or hostname:port"
            ),
            shiny::textInput(ns("user"), "Username"),
            shiny::passwordInput(ns("password"), "Password"),
          )
        ),
        shiny::column(
          width = 6,
          shinyjs::hidden(
            shiny::tags$div(
              id = ns("connection_info_container"),
              shiny::tags$h4(
                shiny::actionLink(
                  inputId = ns("expand_info"),
                  title = "Click to see connection info",
                  shiny::tagList(
                    shiny::tags$i(class = "fa fa-plus-square-o", "aria-hidden" = "true"),
                    shiny::span("SSH connection info")
                  )
                )
              ),
              shinyjs::hidden(shiny::tableOutput(ns("connection_info")))
            )
          )
        )
      )
    )
  )
}

#' Authentication to SSH via Shiny module (within teal) or manually
#'
#' Authentication function that is used as part of a [teal::teal_data_module()] server to
#' perform SSH authentication in a `teal` application.
#'
#' When used outside a Shiny or `teal` application it will ask the user for the credentials and
#' authenticate using [ssh::ssh_connect()].
#'
#' @param host (`character(1)`) Optional host address for SSH connection.
#'
#' @return A list with a session object from [ssh::ssh_connect()].
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ssh_authenticator()
#' }
ssh_authenticator <- function(host = NULL) {
  checkmate::assert_string(host, null.ok = TRUE)

  session <- if (shiny::isRunning()) {
    shiny::moduleServer(ssh_module_name(), function(input, output, session) {
      res <- list(
        session = ssh::ssh_connect(
          sprintf("%s@%s", input$user, host %||% input$host),
          passwd = input$password
        )
      )
      shinyjs::show("connection_info_container")
      session_info <- ssh::ssh_session_info(res$session)

      shiny::observeEvent(input$expand_info, {
        shinyjs::show("connection_info")
        shinyjs::disable("expand_info")
      })

      # Caching result of session info as it will be disconnected after files are downloaded
      output$connection_info <- shiny::renderTable(
        {
          session_info_df <- as.data.frame(t(unlist(session_info)), stringsAsFactors = FALSE)
          colnames(session_info_df) <- NULL

          # Transpose the data frame to convert column names to row names
          transposed_info <- t(session_info_df)
          rownames(transposed_info) <- names(session_info)

          # Convert to data frame and remove column names
          data_frame_info <- as.data.frame(transposed_info, stringsAsFactors = FALSE)
          colnames(data_frame_info) <- NULL

          data_frame_info
        },
        rownames = TRUE
      )

      res
    })
  } else {
    host <- host %||% askpass::askpass("Host")
    user <- askpass::askpass("Username")
    password <- askpass::askpass("Password")
    list(
      session = ssh::ssh_connect(
        sprintf("%s@%s", user, host),
        passwd = password
      )
    )
  }
}

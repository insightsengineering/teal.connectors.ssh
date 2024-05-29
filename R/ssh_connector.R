#' SSH connector
#'
#' This connector allows to read data from SSH remote server during `teal` session.

#' @inheritParams teal.data::teal_data
#' @param data (`teal_data`) object. App developer can pass `data` as `teal_data` object with
#'  some initial data.
#' @param host (`character`) an ssh server string of the form `hostname[:port]`.
#' An `ipv6` hostname should be wrapped in brackets like this: `⁠[2001:db8::1]:80`⁠.
#' @param paths (named `list` of file paths) the paths to the files containing the
#' data on the remote server. The names of the list will define the data names
#' that will be used to store the data on the `teal.data` object.
#' @param read_expression (quoted `expression`) an expression that will be used by the connector
#' to read the contents of the file.
#' Note that the `path` variable is defined in the connector code and should be
#' used in the expression.
#' It defaults to `utils::read.csv(file = path, header = TRUE)`.
#' @examples
#' library(teal)
#' x <- ssh_connector(
#'   paths = list(ADSL = "/path/to/ADSL.csv", ADTTE = "/path/to/ADTTE.csv")
#' )
#'
#' app <- init(
#'   data = x,
#'   modules = list(example_module())
#' )
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
#' app <- init(
#'   data = x,
#'   modules = list(example_module())
#' )
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
#' @export
ssh_connector <- function(data = teal.data::teal_data(),
                          join_keys = teal.data::join_keys(),
                          host = NULL,
                          paths = list(),
                          read_expression = quote(utils::read.csv(file = path, header = TRUE))) {
  checkmate::assert_class(data, "teal_data")
  checkmate::assert_class(join_keys, "join_keys")
  # checkmate::assert_(read_function)
  checkmate::assert_string(host, null.ok = TRUE)
  checkmate::assert_list(paths, names = "named", min.len = 1, types = "character")

  dataset_code <- mapply(
    function(path, dataname) {
      substitute(
        {
          # Downloads file to a temporary directory and allows inclusion
          # of code in "Show R Code" without @linksto
          dataname <- withr::with_tempdir({
            path <- substitute_path
            downloaded <- ssh::scp_download(
              ssh_session,
              path,
              to = "."
            )

            path <- basename(path) # read from local directory
            read_expression
          })
        },
        list(
          substitute_path = path,
          dataname = str2lang(dataname),
          read_expression = read_expression
        )
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
              ssh_args <- ssh_authenticator(host = host)
              ssh_session <- ssh_args$session
            },
            host = host
          )
        })

        # Read data for every path
        tdata <- shiny::reactive({
          new_tdata <- ssh_tdata()

          failed_files <- list()
          if (!checkmate::test_class(ssh_tdata, "qenv.error")) {
            failed_tdata <- NULL
            for (code_ix in seq_along(dataset_code)) {
              new_tdata <- teal.code::eval_code(new_tdata, dataset_code[[code_ix]])
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
            teal.data::datanames(new_tdata) <- setdiff(
              union(teal.data::datanames(new_tdata), names(paths)),
              "ssh_session"
            )
            teal.data::join_keys(new_tdata) <- join_keys
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

error_ui <- function(failed_files = c(), connection_error = FALSE) {
  checkmate::assert_list(failed_files, min.len = 0, names = "named", types = "character")
  checkmate::assert_flag(connection_error)

  shiny::tags$div(
    class = "message-container",
    shiny::tags$h3("Sorry, the data retrieval from SSH was unsuccessful..."),
    shiny::tags$p("Here are some troubleshooting tips for this issue:"),
    shiny::tags$ul(
      if (connection_error) {
        shiny::tags$li("Double check that the host and credentials are correct.")
      } else {
        shiny::tags$li("", style = "display: none;")
      },
      if (!connection_error && length(failed_files) > 0) {
        shiny::tags$li(
          shiny::tags$span("The following files could not be read: "),
          shiny::tags$ul(
            lapply(
              names(failed_files),
              function(dataname) {
                shiny::tags$li(
                  shiny::tags$span(failed_files[[dataname]]),
                  shiny::tags$i(
                    "(dataname: ",
                    shiny::tags$code(dataname, .noWS = "outside"),
                    ")"
                  )
                )
              }
            )
          )
        )
      } else {
        shiny::tags$li("", style = "display: none;")
      },
      shiny::tags$li("Contact the app developer if error persists.")
    ),
    shiny::tags$br(),
    shiny::tags$p("Here's more info about the error message:")
  )
}

#' UI for connector
#'
#' @param id (`character(1)`) the id string to be used in UI element namespace.
#' @param host (`character(1)`) optional host definition that forces this
#' option on the user. If it is not defined, then user can define themselves.
#' @param paths (named `list` of `character(1)`) remote path definition.
#'
#' @return a [shiny::tagList] with UI definition.
#'
#' @keywords internal
ssh_connect_ui <- function(id, host = NULL) {
  checkmate::assert_string(id)
  ns <- shiny::NS(id)

  shiny::tagList(
    fluidPage(
      fluidRow(
        column(
          width = 6,
          shiny::tags$div(
            class = "auth_container",
            shiny::tags$h5("SSH authentication"),
            shiny::textInput(ns("host"), "Host", value = host %||% "", placeholder = "hostname or hostname:port"),
            shiny::textInput(ns("user"), "Username"),
            shiny::passwordInput(ns("password"), "Password"),
          )
        ),
        column(
          width = 6,
          tableOutput(ns("connection_info"))
        )
      )
    )
  )

}

#' Authentication to SSH via Shiny module (within teal) or manually
#'
#' @description
#' It should be used as part of a [teal::teal_data_module()] server to
#' perform SSH authentication in a teal application.
#'
#' This also allows to authenticate with the reproducible generated by a teal application.
#'
#' @param host (`character(1)`) Optional host address for SSH connection.
#'
#' @return A list with a session object from [ssh::ssh_connect()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ssh_authenticator()
#' }
ssh_authenticator <- function(host = NULL) {
  checkmate::assert_string(host, null.ok = TRUE)

  session <- if (shiny::isRunning()) {
    shiny::moduleServer(ssh_module_name(), function(input, output, session) {
      list(
        session = ssh::ssh_connect(
          sprintf("%s@%s", input$user, host %||% input$host),
          passwd = input$password
        )
      )
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

# Helper function to get module name
ssh_module_name <- function() {
  id <- trimws(getOption("teal.connectors.ssh.module.name", "cred"))
  checkmate::assert_string(id)
  id
}

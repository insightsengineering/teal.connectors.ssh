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
#' @param read_function (`function`) a function definition that will be used by the connector
#' to read the contents of the file.
#' It defaults to `utils::read.csv`
#' @param ... arguments that are passed to `read_function`.
#' @examples
#' library(teal)
#' x <- ssh_connector(
#'   paths = list(ADSL = "/path/to/ADSL.csv", ADTTE = "/path/to/ADTTE.csv"),
#'   header = TRUE
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
#'   header = TRUE
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
                          read_function = function(path, ...) utils::read.csv(file = path, ...),
                          ...) {
  checkmate::assert_class(data, "teal_data")
  checkmate::assert_class(join_keys, "join_keys")
  checkmate::assert_function(read_function)
  checkmate::assert_string(host, null.ok = TRUE)
  checkmate::assert_list(paths, names = "named", min.len = 1, types = "character")

  dots <- rlang::list2(...)

  dataset_code <- mapply(
    function(path, dataname) {
      substitute(
        {
          # Downloads file to a temporary directory and allows inclusion
          # of code in "Show R Code" without @linksto
          dataname <- withr::with_tempdir({
            downloaded <- ssh::scp_download(
              ssh_session,
              path,
              to = "."
            )

            do.call(
              read_function,
              append(list(basename(path)), dots)
            )
          })
        },
        list(
          path = path,
          dataname = str2lang(dataname),
          dots = dots
        )
      )
    },
    path = unname(paths),
    dataname = names(paths),
    SIMPLIFY = FALSE
  )

  code <- as.expression(
    c(
      substitute(
        # When using `env` parameters with `as.name("utils::read.csv")` it will
        # surround the substituted expression with backticks. Defining `read_function`
        # in the reproducible call is a workaround for this problem.
        read_function <- source, list(source = read_function)
      ),
      dataset_code
    )
  )

  tdm <- teal::teal_data_module(
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shinyjs::useShinyjs(),
        ssh_connect_ui(ns("cred"), default_host = host),
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
        shinyjs::disabled(shiny::actionButton(ns("submit"), label = "Load data"))
      )
    },
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        # When app developer defines the host, it cannot be changed
        if (!is.null(host)) shinyjs::disable(id = "cred-host")

        ssh_args <- ssh_connect_srv(id = "cred", default_host = host)

        tdata <- shiny::eventReactive(ssh_args(), {
          new_tdata <- teal.code::concat(data, teal_data(ssh_session = ssh_args()$session))
          # Read data for every path
          new_tdata <- teal.code::eval_code(new_tdata, code)

          if (inherits(new_tdata, "qenv.error")) {
            shinyjs::disable("submit")
          }

          new_tdata <- within(

            new_tdata,
            {
              ssh::ssh_disconnect(ssh_session)
              rm(ssh_session)
            }
          )

          if (checkmate::test_class(new_tdata, "teal_data")) {
            shinyjs::enable("submit")
            teal.data::datanames(new_tdata) <-setdiff(union(teal.data::datanames(new_tdata), names(paths)), "ssh_session")
            teal.data::join_keys(new_tdata) <- join_keys
          }

          new_tdata
        })
        tdata
      })
    }
  )
}

#' UI for connector
#'
#' @param id (`character(1)`) The id string to be used in UI element namespace.
#' @param default_host (`character(1)`) optional host definition that forces this
#' option on the user. If it is not defined, then user can define themselves.
#' @param paths (named `list` of `character(1)`) remote path definition.
#'
#' @return a [shiny::tagList] with UI definition.
#'
#' @keywords internal
ssh_connect_ui <- function(id, default_host = NULL) {
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
            shiny::textInput(ns("host"), "Host", value = default_host %||% "", placeholder = "hostname or hostname:port"),
            shiny::textInput(ns("user"), "Username"),
            shiny::passwordInput(ns("password"), "Password"),
            shiny::actionButton(ns("connect"), label = "Connect")
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

#' Server logic for SSH connection
#'
#' This function handles the server-side logic for establishing an SSH connection.
#'
#' @param id (`character(1)`) The id string used in the UI element namespace.
#' @param default_host (`character(1)`) The default SSH host if specified.
#'
#' @return a list with a session object from [ssh::ssh_connect()].
#' @export
ssh_connect_srv <- function(id = "cred", default_host = NULL) {
  checkmate::assert_string(id)

  shiny::moduleServer(id, function(input, output, session) {
    ssh_args <- shiny::eventReactive(input$connect, {
      ssh_authenticator(id = id, user = input$user, password = input$password,host =  input$host)
    })

    output$connection_info <- shiny::renderTable({
      req(ssh_args())
      session_info <- ssh::ssh_session_info(ssh_args()$session)

      session_info_df <- as.data.frame(t(unlist(session_info)), stringsAsFactors = FALSE)
      colnames(session_info_df) <- NULL

      # Transpose the data frame to convert column names to row names
      transposed_info <- t(session_info_df)
      rownames(transposed_info) <- names(session_info)

      # Convert to data frame and remove column names
      data_frame_info <- as.data.frame(transposed_info, stringsAsFactors = FALSE)
      colnames(data_frame_info) <- NULL

      data_frame_info
    }, rownames = TRUE)

    ssh_args
  })
}

#' Authentication to SSH via Shiny module (within teal) or manually
#'
#' @description
#' It should be used as part of a [teal::teal_data_module()] server to
#' perform SSH authentication in a teal application.
#'
#' This also allows to authenticate with the reproducible generated by a teal application.
#'
#' @param id (`character(1)`) An ID string that corresponds with the ID used to call the module's UI function.
#' Defaults to `"cred"`.
#' @inheritParams ssh_connect_ui
#'
#' @return A list with a session object from [ssh::ssh_connect()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ssh_authenticator("demo")
#' }
ssh_authenticator <- function(user, password, host) {
  checkmate::assert_string(user)
  checkmate::assert_string(password)
  checkmate::assert_string(host)
  list(
    session = ssh::ssh_connect(
      sprintf("%s@%s", user, host),
      passwd = password
    )
  )
}

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

  code <- as.expression(dataset_code)

  tdm <- teal::teal_data_module(
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        shinyjs::useShinyjs(),
        ssh_connect_ui(ns("cred"), default_host = host, paths = paths),
        shiny::actionButton(ns("submit"), label = "Load data")
      )
    },
    server = function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        # When app developer defines the host, it cannot be changed
        if (!is.null(host)) shinyjs::disable(id = "cred-host")

        tdata <- shiny::eventReactive(input$submit, {
          new_tdata <- within(
            data,
            {
              ssh_args <- ssh_authenticator(id = "cred", default_host = host)
              ssh_session <- ssh_args$session
            },
            host = host
          )
          # Read data for every path
          new_tdata <- teal.code::eval_code(new_tdata, code)

          new_tdata <- within(
            new_tdata,
            {
              ssh::ssh_disconnect(ssh_session)
              rm(ssh_session)
            }
          )

          if (checkmate::test_class(new_tdata, "teal_data")) {
            teal.data::datanames(new_tdata) <- union(teal.data::datanames(new_tdata), names(paths))
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
ssh_connect_ui <- function(id, default_host = NULL, paths) {
  checkmate::assert_string(id)
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      style = "padding-left: 1em; border-left: solid 1px gray;",
      shiny::tags$h5("SSH authentication"),
      shiny::textInput(ns("host"), "Host", value = default_host %||% "", placeholder = "hostname or hostname:port"),
      shiny::textInput(ns("user"), "Username"),
      shiny::passwordInput(ns("password"), "Password"),
    ),
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
#' @param id (`character(1)`) An ID string that corresponds with the ID used to call the module's UI function.
#' Defaults to `"cred"`. This parameter is only used when a shiny application is running.
#' @inheritParams ssh_connect_ui
#'
#' @return A list with a session object from [ssh::ssh_connect()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ssh_authenticator()
#' }
ssh_authenticator <- function(default_host = NULL, id = "cred") {
  checkmate::assert_string(id)
  session <- if (shiny::isRunning()) {
    shiny::moduleServer(id, function(input, output, session) {
      list(
        session = ssh::ssh_connect(
          sprintf("%s@%s", input$user, input$host),
          passwd = input$password
        )
      )
    })
  } else {
    host <- default_host %||% askpass::askpass("Host")
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

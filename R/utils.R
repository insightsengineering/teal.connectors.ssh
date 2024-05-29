# Helper function to get module name
ssh_module_name <- function() {
  id <- trimws(getOption("teal.connectors.ssh.module.name", "cred"))
  checkmate::assert_string(id)
  id
}

# Creates a helful error UI to guide the user towards a solution
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
                  shiny::tags$code(failed_files[[dataname]]),
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

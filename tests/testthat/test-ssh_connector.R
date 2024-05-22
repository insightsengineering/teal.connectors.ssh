testthat::test_that("ssh_connector fails when data parameter is not `teal_data`", {
  testthat::expect_error(
    ssh_connector(data = list()), "Must inherit from class 'teal_data'"
  )
})

testthat::test_that("ssh_connector fails when no path is specificed", {
  testthat::expect_error(ssh_connector(), "Assertion on 'paths' failed")
})

testthat::test_that("ssh_connector requires join_keys to be join_keys", {
  testthat::expect_error(ssh_connector(join_keys = list()), "join_keys")
})

testthat::test_that("ssh_connector returns a 'teal_data_module' with ui and server", {
  out <- ssh_connector(paths = list(a = "/path/to/file"))
  checkmate::expect_class(out, "teal_data_module")
  checkmate::expect_list(out, names = "named", len = 2)
  checkmate::expect_function(out$server, args = "id")
  checkmate::expect_function(out$ui, args = "id")
})

testthat::test_that("ssh_connector$server returns 'teal_data' after clicking submit button", {
  # Necessary to shiny::isRunning() = FALSE
  # https://github.com/rstudio/shiny/issues/3597
  init_current_app_state <- getFromNamespace("initCurrentAppState", "shiny")
  clear_current_app_state <- getFromNamespace("clearCurrentAppState", "shiny")

  init_current_app_state("testing")
  withr::defer(clear_current_app_state())

  iris_path <- withr::local_tempfile()
  mtcars_path <- withr::local_tempfile()

  write.csv(iris, file = iris_path)
  write.csv(mtcars, file = mtcars_path)

  connector <- ssh_connector(
    paths = list(dataset1 = iris_path, dataset2 = mtcars_path),
    header = TRUE,
    row.names = 1,
    stringsAsFactors = TRUE
  )

  shiny::testServer(
    connector$server,
    args = list(id = "test"),
    expr = {
      # mock {ssh} functions to evaluate code
      testthat::with_mocked_bindings(
        ssh_connect = function(...) list(),
        ssh_disconnect = function(...) NULL,
        scp_download = function(session, files, to) {
          lapply(
            files,
            function(from) file.copy(from, file.path(to, basename(from)))
          )
        },
        .package = "ssh",
        code = {
          session$setInputs(submit = TRUE)
          testthat::expect_s4_class(tdata(), "teal_data")

          code <- teal.code::get_code(tdata())

          params_str <- "header = TRUE, row.names = 1, stringsAsFactors = TRUE"

          testthat::expect_equal(
            trimws(strsplit(code, "\n")[[1]]), # remove indentaton and whitespace
            c(
              "ssh_args <- ssh_authenticator(id = \"cred\", default_host = NULL)",
              "ssh_session <- ssh_args$session",
              "read_function <- function (path, ...)",
              "utils::read.csv(file = path, ...)",
              "dataset1 <- withr::with_tempdir({",
              sprintf("downloaded <- ssh::scp_download(ssh_session, \"%s\", to = \".\")", iris_path),
              sprintf("do.call(read_function, append(list(basename(\"%s\")), list(%s)))", iris_path, params_str),
              "})",
              "dataset2 <- withr::with_tempdir({",
              sprintf("downloaded <- ssh::scp_download(ssh_session, \"%s\", to = \".\")", mtcars_path),
              sprintf(
                "do.call(read_function, append(list(basename(\"%s\")), list(%s)))", mtcars_path, params_str
              ),
              "})",
              "ssh::ssh_disconnect(ssh_session)",
              "rm(ssh_session)"
            ),
            fixed = TRUE
          )

          testthat::expect_equal(teal.code::get_var(tdata(), "dataset1"), iris)
          testthat::expect_equal(teal.code::get_var(tdata(), "dataset2"), mtcars)
        }
      )
    }
  )
})

testthat::test_that("ssh_connector$server returns 'teal_data' from ", {
  # Necessary to shiny::isRunning() = FALSE
  # https://github.com/rstudio/shiny/issues/3597
  init_current_app_state <- getFromNamespace("initCurrentAppState", "shiny")
  clear_current_app_state <- getFromNamespace("clearCurrentAppState", "shiny")

  init_current_app_state("testing")
  withr::defer(clear_current_app_state())

  iris_path <- withr::local_tempfile()

  read_fun <- function(path, ...) {
    dat <- as.data.frame(read.dcf(path), ...)
    for (x in c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")) {
      dat[[x]] <- as.numeric(dat[[x]])
    }
    dat$Species <- as.factor(dat$Species)
    dat
  }

  write.dcf(iris, file = iris_path)

  connector <- ssh_connector(paths = list(dataset1 = iris_path), read_function = read_fun)

  shiny::testServer(
    connector$server,
    args = list(id = "test"),
    expr = {
      # mock {ssh} functions to evaluate code
      testthat::with_mocked_bindings(
        ssh_connect = function(...) list(),
        ssh_disconnect = function(...) NULL,
        scp_download = function(session, files, to) {
          lapply(
            files,
            function(from) file.copy(from, file.path(to, basename(from)))
          )
        },
        .package = "ssh",
        code = {
          session$setInputs(submit = TRUE)
          testthat::expect_s4_class(tdata(), "teal_data")

          testthat::expect_equal(teal.code::get_var(tdata(), "dataset1"), iris)
        }
      )
    }
  )
})

# `teal.connectors.ssh`: `teal` connector to access data via SSH

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering/teal.connectors.ssh/actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering/teal.connectors.ssh/actions/workflows/check.yaml)
[![Docs 📚](https://github.com/insightsengineering/teal.connectors.ssh/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal.connectors.ssh/)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/teal.connectors.ssh/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/teal.connectors.ssh/main/coverage-report/)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/teal.connectors.ssh?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/insightsengineering/teal.connectors.ssh?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/teal.connectors.ssh)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/teal.connectors.ssh)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/teal.connectors.ssh)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/teal.connectors.ssh)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/teal.connectors.ssh)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/teal.connectors.ssh)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/teal.connectors.ssh/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/teal.connectors.ssh/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/teal.connectors.ssh?color=red\&label=open%20issues)](https://github.com/insightsengineering/teal.connectors.ssh/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

The `teal.connectors.ssh` package allows SSH data to be pulled into `teal` applications.
The app developer can be used as a [`teal_data_module`](https://insightsengineering.github.io/teal/latest-tag/articles/data-as-shiny-module.html) in your `teal` application.

## Key features

- Define one or more paths to the data in the remote server.
- The data names that will be associated with the data.
- Function to use to read the data. with `utils::read.csv` as the default.
  - Extra arguments on `ssh_connector` call will be passed to this function.
- _(optionally)_ The host of the remote server.
  - If not defined, the user must input it manually.

For an example as to how this works see the example app in the help page: `help(ssh_connector)`.

## Installation

```r
install.packages("teal.connectors.ssh", repos = c("https://insightsengineering.r-universe.dev", getOption("repos"))))

# install.packages("pak")
pak::pak("insightsengineering/teal.connectors.ssh@*release")
```

Alternatively, you might also use the development version.

```r
install.packages("teal.connectors.ssh", repos = c("https://pharmaverse.r-universe.dev", getOption("repos")))

# install.packages("pak")
pak::pak("insightsengineering/teal.connectors.ssh")
```

## Example

```r
library(teal.connectors.ssh)
x <- ssh_connector(
  paths = list(ADSL = "/path/to/ADSL.csv", ADTTE = "/path/to/ADTTE.csv"),
  host = "example.com:22"
)
app <- teal::init(data = x, modules = list(teal::example_module()))
shiny::shinyApp(app$ui, app$server)
```

![Example of sample application](man/figures/example-ui.png)

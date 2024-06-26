% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssh_connector.R
\name{ssh_authenticator}
\alias{ssh_authenticator}
\title{Authentication to SSH via Shiny module (within teal) or manually}
\usage{
ssh_authenticator(host = NULL)
}
\arguments{
\item{host}{(\code{character(1)}) Optional host address for SSH connection.}
}
\value{
A list with a session object from \code{\link[ssh:ssh]{ssh::ssh_connect()}}.
}
\description{
Authentication function that is used as part of a \code{\link[teal:teal_data_module]{teal::teal_data_module()}} server to
perform SSH authentication in a \code{teal} application.
}
\details{
When used outside a Shiny or \code{teal} application it will ask the user for the credentials and
authenticate using \code{\link[ssh:ssh]{ssh::ssh_connect()}}.
}
\examples{
if (interactive()) {
  ssh_authenticator()
}
}

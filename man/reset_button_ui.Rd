% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/button_retry.R
\name{reset_button_ui}
\alias{reset_button_ui}
\title{Tutorial reset button}
\usage{
reset_button_ui(id, label = "Retry Exam")
}
\arguments{
\item{id}{ID matching ui with server}

\item{label}{Label to appear on the submit grade button}
}
\description{
Reset the entire exam to allow another attempt.
The number of attempts and how often the retry option is available can be defined in
the \code{tutorialExtras_setup()} function.
The tutorial must be available in a package for this button to work as the function requires
a name and package specification.

Shiny ui and server logic for the lock computation.

Note that when including these functions in a learnr Rmd document it is necessary that
the server function, \code{reset_server()}, be included in an R chunk where \code{context="server"} as
they interact with the underlying Shiny functionality. Conversely, the ui function,
\code{reset_button_ui()}, must \emph{not} be included in an R chunk with a \code{context}.
}

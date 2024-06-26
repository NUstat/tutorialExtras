% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/description.R
\name{text_ui}
\alias{text_ui}
\title{Text and description storage}
\usage{
text_ui(id = NULL)
}
\arguments{
\item{id}{ID matching ui with server}
}
\description{
Add text descriptions to exercises.
Including a ui/server for the question text will allow
the text to be printed in the downloaded exam html.

In order for the text to appear with the exercise when downloading,
the id for the text_ui and text_server must match the
name of the exercise code chunk. Otherwise, it will print at the
end of the html document.
}

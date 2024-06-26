% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_setup.R
\name{tutorialExtras_setup}
\alias{tutorialExtras_setup}
\title{Tutorial Extras Setup}
\usage{
tutorialExtras_setup(
  is_exam = FALSE,
  max_attempt = Inf,
  max_retry = Inf,
  retry_cooldown = 24
)
}
\arguments{
\item{is_exam}{defaults to FALSE. If the tutorial is an exam with a lock button set to TRUE.}

\item{max_attempt}{stop allowing submits if max_attempt is reached.}

\item{max_retry}{stop allowing re-attempts when max_retry is reached.}

\item{retry_cooldown}{length of time after lock is pressed until retry becomes available (in hours). Default = 24}
}
\description{
Define if the tutorial is an exam, if the exam has a max_retry and cooldown, and if a tutorial has
a max_attempt limit on quiz questions.
}


# ISDSfunctions

The ISDSfunctions package contains a variety of functions to help enhance learnr tutorials. The ISDSfunctions package is free and offered to support instructors who want to teach R.

# Installation

Install the latest version of ISDSfunctions from GitHub.

devtools::install_github("NUstat/ISDSfunctions", dependencies = TRUE)

Currently not available on CRAN.


# Features of ISDSfunctions

 - 3 new question types including: fill in the blank, radio drop-down, and word bank.
 - tutorial grading and printing options
 - exam tutorials with randomized shuffle and randomized question selection
 - exam tutorials with reset options geared towards proficiency exams
 - exam tutorials with grading and print options


# List of tutorials

The following tutorials demonstrate how to use the functions within this package.

- exam_tutorial_example
- grade_tutorial_example
- new_question_types

Tutorials can be run by typing the following line in the R console: `learnr::run_tutorial("new_question_types", package = "ISDSfunctions")`. Alternatively, in Version 1.3 onwards after having executed `library(ISDSfunctions)`, a list of tutorials appears in a tutorial tab (by default it will be in the upper-right pane).

# Acknowledgments

This work was made possible through funding from the Alumnae of Northwestern University grant and the Open Educational Resources (OER) grant.

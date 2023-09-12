
# tutorialExtras

The tutorialExtras package contains a variety of functions to help enhance learnr tutorials. The tutorialExtras package is free and offered to support instructors who want to teach R. These functions are currently being implemented in the following packages: isdsTutorials and r4dsQuizzes.

# Installation

Install the latest version of tutorialExtras from GitHub.

remotes::install_github("NUstat/tutorialExtras", dependencies = TRUE)

Currently not available on CRAN.


# Features of tutorialExtras

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

Tutorials can be run by typing the following line in the R console: `learnr::run_tutorial("new_question_types", package = "tutorialExtras")`. Alternatively, in Version 1.3 onwards after having executed `library(tutorialExtras)`, a list of tutorials appears in a tutorial tab (by default it will be in the upper-right pane).

# Acknowledgments

This work was produced with support from Northwestern University Libraries, with funding from Northwestern University’s 
Affordable Instructional Resources initiative, and funding from the Open Educational Resources (OER) grant.

# Citations

Aden-Buie G, Chen D, Grolemund G, Rossell Hayes A, Schloerke B (2023). gradethis: Automated Feedback for Student Exercises in 'learnr' Tutorials. https://pkgs.rstudio.com/gradethis/, https://rstudio.github.io/learnr/, https://github.com/rstudio/gradethis.

Aden-Buie G, Schloerke B, Allaire J, Rossell Hayes A (2023). learnr: Interactive Tutorials for R. https://rstudio.github.io/learnr/, https://github.com/rstudio/learnr.

de Vries A, Schloerke B, Russell K (2023). sortable: Drag-and-Drop in 'shiny' Apps with 'SortableJS'. R package version 0.5.0, https://rstudio.github.io/sortable/.

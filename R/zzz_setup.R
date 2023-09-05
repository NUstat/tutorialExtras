# load isds_setup on load to prevent errors
# user can define the options in setup
.onLoad <- function(libname, pkgname) {
  isds_setup()
}

#' @title ISDS setup variables
#'
#' @description
#' Define if the tutorial is an exam and if there is a max_attempt limit on quiz questions
#' 
#' @param isds_exam defaults to FALSE. If the tutorial is an exam with a lock button set to TRUE.
#' @param max_attempt stop allowing submits if max_attempt is reached.
#' @param max_retry stop allowing re-attempts when max_retry is reached.
#' @param retry_cooldown length of time after lock is pressed until retry becomes available (in hours). Default = 24
#'
#' @export
isds_setup <- function(isds_exam = FALSE, max_attempt = Inf, max_retry = Inf, retry_cooldown = 24){
  # hacky less than optimal solution
  
  # error checking
  # ensure max_attempt is either inf or an integer
  if (max_attempt != Inf) {
    if(max_attempt%%1 != 0){
      stop("max_attempt must be either an integer or Inf.")
    }
  }
  
  # ensure max_retry is either inf or an integer
  if (max_retry != Inf) {
    if(max_retry%%1 != 0){
      stop("max_retry must be either an integer or Inf.")
    }
  }
  
  # ensure retry_cooldown is either 0 or an integer
  if(retry_cooldown%%1 != 0){
    stop("retry_cooldown must be an integer.")
  }
  
  
  # setting global variables accessible to override_exercise and override_quiz
  isds_exam <<- isds_exam
  max_attempt <<- max_attempt
  max_retry <<- max_retry
  retry_cooldown <<- retry_cooldown
  
  # storage must be local for reset option to work
  options(tutorial.storage = "local")
  
  # create a temp directory that stores "attempt" and "time"
  tmp_dir <- tempdir()
  end_dir <- ifelse(!is.na(stringi::stri_locate_last_fixed(tmp_dir, "/")[,1]),
                    stringi::stri_locate_last_fixed(tmp_dir, "/")[,1],
                    stringi::stri_locate_last_fixed(tmp_dir, "\\")[,1])
  # global object so we can save time
  mod_dir <<- stringr::str_sub(tmp_dir, end = end_dir)
  
  # get time if it exists
  # this prevents time from being reset if "start over" is pressed
  
  if(file.exists(paste0(mod_dir,"time.RData"))){
    load(file = paste0(mod_dir,"time.RData"))
  }
  start_time <<- ifelse(exists("start_time"), start_time, 0)
  
  # get attempt if it exists
  if(file.exists(paste0(mod_dir,"attempt.RData"))){
    load(file = paste0(mod_dir,"attempt.RData"))
  }
  
  attempt <<- ifelse(exists("attempt"), attempt, 1)
  
  # need to set.seed that changes every "reattempt"
  init.seed <<- Sys.info()["user"]
  
  TeachingDemos::char2seed(paste0(init.seed, attempt))
  
  rand_int <<- stats::runif(1, 0.01, 10)
  
}
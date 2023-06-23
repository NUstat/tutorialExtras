
# Add a custom exercise grader


# Does not work
# empty ui for server to match and set the seed
#' @title Initializing seed server
#' @param id ID matching ui with server
#' @export
exam_seed_ui <- function(id){
  print("ui")

}

# server for initializing seed
#' @title Initializing seed server
#' @param id ID matching ui with server
#' @export
exam_seed_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session = getDefaultReactiveDomain()) {
      observe({
        #observeEvent(
        #  req(session$userData$learnr_state() == "restored"),{
        session = getDefaultReactiveDomain()
        ns <- getDefaultReactiveDomain()$ns
        print("Seed triggered")
        user_id <- learnr:::get_tutorial_info()$user_id
        attempt = 1
        #attempt <- learnr:::get_object(session,  NS("seed", id = "seed"))$data$seed
        #print(attempt)

        #print(paste0(tutorial_info$user_id, attempt))

        TeachingDemos::char2seed(paste0(user_id, attempt))
        #newseed <<- paste0(tutorial_info$user_id, attempt)
      })
    })
}
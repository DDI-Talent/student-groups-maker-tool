library(R6)
# install.packages("magrittr")
library(magrittr)
library(tibble)
library(dplyr)
library(shiny)
library(bslib)

# TODO: should this me dependancy injection instead?
source('./pairs_engine/matrix_operations.R')
source('./pairs_engine/pairs_operations.R')
source('./pairs_engine/history_operations.R')
source('./pairs_engine/fitness.R')
source('./pairs_engine/scenarios_operations.R')



PairMaker <- R6::R6Class("PairMaker",
                         public = list(
                           history = NA,
                           filename = NA,
                           most_recent_candidate = NA,
                           count_debug = 0,
                           present_ = c(),
                           initialize = function(history = history_operations$empty_history(), filename = NA) {
                             self$history <- history
                             self$filename <- filename
                             self$most_recent_candidate <- reactiveVal(NA)
                           },
                           load_history_from_file = function(fileLocation, fileNameString){
                             self$filename <- fileNameString
                             self$history <- history_operations$load_history_from_file(fileLocation)
                           },
                           save_history_to_file = function(new_filename = self$filename){
                             history_operations$save_history_in_file(self$history, new_filename)
                           },
                           get_all_potential_attendees = function(){
                             print("get_all_potential_attendees")
                             print(self$history)
                             self$history$person %>% unique() %>% sort()
                           },
                           innitialise_class = function(attendees_string){
                             #TODO comaSeparatedNames strsplit?
                             attendees_vector <- history_operations$attendeesFromString(attendees_string)
                             self$history <- scenarios_operations$run_one_lab(
                               attendees_vector,
                               group_size = 1,
                               history_until_now = history_operations$empty_history(),
                               session_name = "ClassSetup",
                               how_many_alternatives = 1
                             )
                             print("innitialise_class1")
                             print(self$history)
                             self$filename <- "NewClass.csv"
                           },
                           make_pairs_for_session = function(attendees, session_name, group_size = 2, how_many_alternatives = 10){
                             print("make_pairs_for_session")
                             print(attendees)
                             newMostRecentCandidate <- scenarios_operations$run_one_lab(
                               attendees,
                               group_size = group_size,
                               history_until_now = self$history,
                               session_name = session_name,
                               how_many_alternatives = how_many_alternatives
                             )
                             self$most_recent_candidate(newMostRecentCandidate)
                             # self$most_recent_candidate() # can we get away without returning this?
                           },
                           approve_most_recent_groups_candidate = function(){
                             history_with_new_froups <- history_operations$combine_histories( self$most_recent_candidate(), self$history)
                             self$history <- history_with_new_froups
                           },
                           getMostRecentCandidate = function(simplified = FALSE){
                             if (simplified){
                               self$most_recent_candidate() %>% select(c("person","group_id"))
                             }else{
                               self$most_recent_candidate()
                             }
                           }
                         ),
                         # Use active to change variables from outside without get/set reactivenes. like public vars.
                         active = list(
                           present = function(value) {
                             if (missing(value)) return(present_)
                             else self$present_ <- value
                           }
                         )
)

library(R6)
# install.packages("magrittr")
library(magrittr)
library(tibble)
library(dplyr)
library(shiny)
library(bslib)

# private & pure functions come from here:
source('./pairs_engine/matrix_operations.R')
source('./pairs_engine/pairs_operations.R')
source('./pairs_engine/history_operations.R')
source('./pairs_engine/scenarios_operations.R')
source('./pairs_engine/fitness_operations.R')

Pair_maker <- R6::R6Class("Pair_maker",
                         public = list(

                          # VARIABLES
                           history = NA,
                           filename = NA,
                           most_recent_candidate = NA,
                           count_debug = 0,
                           present_ = c(),

                           # CONSTRUCTOR
                           initialize = function(history = history_operations$empty_history(), filename = NA) {
                             self$history <- history
                             self$filename <- filename
                             self$most_recent_candidate <- reactiveVal(NA)
                           },

                           # FUNCTIONS
                           load_history_from_file = function(file_location, file_name_string){
                             self$filename <- file_name_string
                             self$history <- history_operations$load_history_from_file(file_location)
                           },
                           save_history_to_file = function(new_filename = self$filename){
                             history_operations$save_history_in_file(self$history, new_filename)
                           },
                           get_all_potential_attendees = function(){
                             self$history$person %>% unique() %>% sort()
                           },
                           add_students = function(attendees_string){
                             attendees_vector <- history_operations$attendees_from_string(attendees_string)
                             fake_session_to_add_students <- scenarios_operations$run_one_session(
                               attendees_vector,
                               group_size = 1,
                               history_until_now = history_operations$empty_history(),
                               session_name = history_operations$session_name_with_format("ClassSetup"),
                               how_many_alternatives = 1
                             )
                             self$history <- history_operations$combine_histories(self$history, fake_session_to_add_students)
                             self$filename <- if(is.na(self$filename)) "new_class.csv" else self$filename

                           },
                           make_pairs_for_session = function(attendees, session_name, group_size = 2, how_many_alternatives = 10){
                             new_most_recent_candidate <- scenarios_operations$run_one_session(
                               attendees,
                               group_size = group_size,
                               history_until_now = self$history,
                               session_name = session_name,
                               how_many_alternatives = how_many_alternatives
                             )
                             self$most_recent_candidate(new_most_recent_candidate)
                           },
                           approve_most_recent_groups_candidate = function(){
                             history_with_new_froups <- history_operations$combine_histories( self$most_recent_candidate(), self$history)
                             self$history <- history_with_new_froups
                           },
                           get_most_recent_candidate = function(simplified = FALSE){
                             if (simplified){
                               self$most_recent_candidate() %>% select(c("person","group_id"))
                             }else{
                               self$most_recent_candidate()
                             }

                           }
                         ),
                         private = list(
                         ),
                         # Use active to change variables from outside without get/set reactivenes. like public vars.
                         # (can't remember if we use this at the end)
                         active = list(
                           present = function(value) {
                             if (missing(value)) return(present_)
                             else self$present_ <- value
                           }
                         )
)

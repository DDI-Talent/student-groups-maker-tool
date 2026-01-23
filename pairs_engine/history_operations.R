history_operations <- list(
  combine_histories = function(...){
    bind_rows(...)
  },
  empty_history = function(){
    empty_history_new <- tibble(group_size = numeric(),
                                  session = character(),
                                  fitness = numeric(),
                                  person = character())
    empty_history_new
  },
  new_history_for_class = function(attendees, session_name = "SetupClassroom"){
    history_for_class <- tibble(group_size = 1,
                                session = session_name,
                                fitness = 1,
                                person = attendees)
    history_for_class
  },
  attendees_from_string = function(attendees_string){
    #   split by , and then remove whitespaces around
    attendees_string |> strsplit(",") |> unlist() |> trimws() |> unique()
  },

  load_history_from_file = function(file_name, fallback_to_empty = TRUE){
    #   TODO: trycatch file not existing. or maybe missing some columns?
    loaded_history <- read.csv(file_name)
    if (fallback_to_empty == TRUE && nrow(loaded_history) == 0){
      loaded_history <- empty_history()
    }
    loaded_history
  },
  save_history_in_file = function(history_to_save, filename){
    write.csv(history_to_save, filename, row.names = FALSE)
  },
  session_name_with_format = function(session_name_plain){
    lab_name_unique <- paste0(session_name_plain,"_" ,sample(letters,1) )
    lab_name_unique
  }

)

# TODO: cleanup tests
# cands1 <- make_pairs_long(people = letters[1:5], group_size = 2)
# cands2 <- make_pairs_long(people = letters[1:5], group_size = 2)
# combine_histories(cands1,cands2)


# load_history_from_file("efi_text_history22.csv")
# load_history_from_file("efi_text_history2.csv")
# empty_history()


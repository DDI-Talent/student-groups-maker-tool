
# calculate differnt penalties of pairs candidate
fitness <- function(pairs_candidate, pairs_history = history_operations$empty_history()) {
  pairs_history_including_this <- history_operations$combine_histories(pairs_history, pairs_candidate)
  unique_sessions <- pairs_history_including_this |> distinct(session) |> pull(session)
  unique_people <- pairs_history_including_this |> distinct(person) |> pull(person)
  ## TODO: penalise for people being together again!
  history_matrix <- matrix_operations$make_empty_matrix(unique_people)
  for (this_session in unique_sessions){
    week_as_matrix <- pairs_history_including_this |>
      filter(session == this_session) |>
      matrix_operations$pairs_as_matrix(unique_people)
    history_matrix <- history_matrix + week_as_matrix
    # TODO: I think this might not work, if some students are missing on some weeks?
    # becuase history is always assumed to be the matrix of the same size?`
  }
  penalty_for_people_being_together_again <- sd(history_matrix)

  # DONE: penalise for individual students being in large pairs

  # redo groups sizes (where did they come from previously?? oh well)

  pairs_history_including_this <- pairs_history_including_this |> group_by(session, group_id) %>%
    mutate(group_size = n()) %>%
    ungroup()


  # browser()

  # get sum of ll group sizes for each person
  persons_group_sizes_total <- pairs_history_including_this |>
    group_by(person) |>
    summarise(penalty_group_size=sum(group_size))

  # penalise for sd of those groupsize sums
  penalty_for_same_people_in_large_groups <- persons_group_sizes_total|>
    summarise(total_penalty = sd(penalty_group_size))
  # print("penalties")
  # print(paste(penalty_for_same_people_in_large_groups, penalty_for_people_being_together_again))
  total_penalty <- penalty_for_same_people_in_large_groups + penalty_for_people_being_together_again
  round(total_penalty, 2)
}
#
# test_fitness <- function(){
#   test1 <- structure(list(person = c("a", "b", "d", "c", "e"),
#                           week = c("1","1", "1", "1", "1"),
#                           group_id = c(1, 1, 1, 2, 2),
#                           group_size = c(3, 3, 3, 2, 2))
#   )
#   test2 <- structure(list(person = c("a", "b", "c", "d", "e"),
#                           week = c("2","2", "2", "2", "2"),
#                           group_id = c(1, 1, 1, 2, 2),
#                           group_size = c(3, 3, 3, 2, 2))
#   )
#   test3 <- structure(list(person = c("a", "d", "e", "b", "c"),
#                           week = c("3","3", "3", "3", "3"),
#                           group_id = c(1, 1, 1, 2, 2),
#                           group_size = c(3, 3, 3, 2, 2))
#   )
#   test_good <- structure(list(person = c("c", "d", "e", "b", "a"),
#                               week = c("4g","4g", "4g", "4g", "4g"),
#                               group_id = c(1, 1, 1, 2, 2),
#                               group_size = c(3, 3, 3, 2, 2))
#   )
#   test_bad <- structure(list(person = c("a", "b", "c", "d", "e"),
#                              week = c("4b","4b", "4b", "4b", "4b"),
#                              group_id = c(1, 1, 1, 2, 2),
#                              group_size = c(3, 3, 3, 2, 2))
#   )
#   print("test_good")
#   fitness(pairs_candidate = test_good, pairs_history = combine_histories(test1, test2,test3))
#   print("test_bad")
#   fitness(pairs_candidate = test_bad, pairs_history = combine_histories(test1, test2,test3))
# }
#
# test_fitness()

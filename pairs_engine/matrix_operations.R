matrix_operations <- list(
  make_empty_matrix = function(persons){
    unique_people <- sort(persons)
    matrix_of_zeros <- matrix(0:0, nrow = length(unique_people),
                           ncol = length(unique_people),
                           dimnames = list(unique_people,
                                           unique_people))
    matrix_of_zeros
  },
  pairs_as_matrix = function(pairs, all_unique_people){
    empty_matrix <- matrix_operations$make_empty_matrix(persons = all_unique_people)
    # empty_matrix <- make_empty_matrix(persons = pairs$person)
    group_ids <- pairs |>
      distinct(group_id) |>
      pull(group_id)
    group_ids

    for (group_id_name in group_ids) {
      people_in_pair <- pairs |>
        filter(group_id == group_id_name) |>
        pull(person)
  # browser()
      # if pair has 1 person, double them.
      if (length(people_in_pair) == 1){
        people_in_pair = c(people_in_pair, people_in_pair)
      }
      subpairs_in_group <- combn(people_in_pair, m=2)

      for (group_id_name in 1:dim(subpairs_in_group)[2]) {
        subpair <- subpairs_in_group[,group_id_name]
        empty_matrix[subpair[1], subpair[2]] <- 1
        empty_matrix[subpair[2], subpair[1]] <- 1
      }
    }
    empty_matrix
  }
)
# pairs_test <- tibble( person = c("a", "b", "c", "d", "e"),
#                       week = c("2","2", "2", "2", "2"),
#                       group_id = c(1, 1, 1, 2, 2),
#                       group_size = c(3, 3, 3, 2, 2)
# )
# pairs_as_matrix(pairs_test)

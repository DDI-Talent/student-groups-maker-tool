pairs_operations <- list(
  make_pairs_long = function(people, group_size = 2, session_name = paste0(sample(10000, 1))) {
    pool_idx <- seq_along(people)
    number_of_groups <- length(people) %/% group_size
    group_names <- rep(seq_len(number_of_groups), times = group_size + 100)
    group_names <- group_names[pool_idx] #trim to size of students

    people_shuffled <- split(sample(people), group_names) # shuffles here

    tibble(person = people_shuffled) |>
      dplyr::mutate(session = session_name)  |>
      dplyr::mutate(group_id = unique(group_names)) |>
      tidyr::unnest(person) |>
      dplyr::mutate(group_size = n(), .by = group_id)
  }
)
# make_pairs_long(people = letters[1:5], group_size = 2)

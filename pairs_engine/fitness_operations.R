fitness_operations <- list(
  # calculate different penalties of pairs candidate
  fitness = function(pairs_candidate, pairs_history = history_operations$empty_history()) {
    pairs_history_including_this <- history_operations$combine_histories(pairs_history, pairs_candidate)
    unique_sessions <- pairs_history_including_this |> distinct(session) |> pull(session)
    unique_people <- pairs_history_including_this |> distinct(person) |> pull(person)

    history_matrix <- matrix_operations$make_empty_matrix(unique_people)
    for (this_session in unique_sessions){
      week_as_matrix <- pairs_history_including_this |>
        filter(session == this_session) |>
        matrix_operations$pairs_as_matrix(unique_people)
      history_matrix <- history_matrix + week_as_matrix
    }
    penalty_for_people_being_together_again <- sd(history_matrix)

    # penalise for individual students being in large pairs
    pairs_history_including_this <- pairs_history_including_this |> group_by(session, group_id) %>%
      mutate(group_size = n()) %>%
      ungroup()

    # get sum of all group sizes for each person
    persons_group_sizes_total <- pairs_history_including_this |>
      group_by(person) |>
      summarise(penalty_group_size=sum(group_size))

    # penalise for sd of those groupsize sums
    penalty_for_same_people_in_large_groups <- persons_group_sizes_total|>
      summarise(total_penalty = sd(penalty_group_size))

    total_penalty <- penalty_for_same_people_in_large_groups + penalty_for_people_being_together_again
    round(total_penalty, 2)
  }
)

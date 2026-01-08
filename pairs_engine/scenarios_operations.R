scenarios_operations <- list(
  run_one_lab = function( people, group_size, history_until_now, session_name, how_many_alternatives = 50 ){
    # print("run_one_lab 1")
    # print(people)
    cands_this_week <- scenarios_operations$find_fittest_pairs(people = people,
                                          group_size = group_size,
                                          session_name = session_name,
                                          how_many_alternatives = how_many_alternatives,
                                          history_until_now=history_until_now)
    # print("run_one_lab 2")

    cands_this_week
    # history_until_now


    # print(paste("fitness for week:",which_week))
    # print(cands_this_week$fitness[1])
    # NICE OPTIONAL: see history
    # print(history_until_now, n = 1000)
    # history_until_now
    # |> select(-c('fitness', "group_size"))
  },
  run_scenario = function( people, group_size, how_many_sessions, how_many_alternatives  ){
    history_until_now <- history_operations$empty_history()
    session_name <- 'Test'
    for (which_session in 1:how_many_sessions) {
      cands_this_session <- find_fittest_pairs(people = people,
                                            group_size = group_size,
                                            session_name = paste0(session_name,which_session),
                                            how_many_alternatives = how_many_alternatives,
                                            history_until_now=history_until_now)
      history_until_now <- history_operations$combine_histories(cands_this_session, history_until_now)
      print(paste("fitness for week:",which_session))
      print(cands_this_session$fitness[1])
    }
    # NICE OPTIONAL: see history
    # print(history_until_now, n = 1000)
    history_until_now
  },
  find_fittest_pairs = function(people, group_size = 2, session_name = "session_", how_many_alternatives = 10, history_until_now){
    # print("find_fittest_pairs 1")

    parallell_universes <- history_operations$empty_history()
    print("find_fittest_pairs 2")
    time_now_string <- format(Sys.time(), "%Y-%m-%d--%H:%M:%S")

    for (alternative in 1:how_many_alternatives) {
      # print("alternative")
      # print(alternative)
      # print(people)
      cands_for_session <- pairs_operations$make_pairs_long(people, group_size = group_size, session_name = session_name)
      fitness_of_this_cand <- fitness(cands_for_session, history_until_now)
      cands_for_session <- cands_for_session |>
        mutate(fitness = fitness_of_this_cand$total_penalty) |>
        mutate(alternative = alternative) |>
        mutate(time = time_now_string)
      parallell_universes <- history_operations$combine_histories(parallell_universes, cands_for_session)
    }
    # find fittest week in parallell_universes
    alternative_to_keep <- parallell_universes |>
      slice_min(fitness) |>
      slice(1) |>
      pull(alternative) |>
      first()

    # print(paste("fittest",alternative_to_keep))

    # keep stuff from this week, and return it
    parallell_universes |>
      filter(alternative == alternative_to_keep) |>
      select(-c(alternative))
  }
)


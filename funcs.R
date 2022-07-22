# stage analysis functions

pull_ids_from_tourney_df <- function(tournaments_df, start_dt, end_dt) {
  
  tournaments_df %>%
    dplyr::filter(!is.na(numAttendees)) %>%
    dplyr::filter(start_date >= start_dt) %>%
    dplyr::filter(start_date <= end_dt) %>%
    dplyr::select(id) %>% 
    pull() %>%
    return()
  
}

## input list of tournament id, return every game within every event in tournament
create_games_df_from_ids_list <- function(ids_list) {
  raw_tbl <- ids_list %>%
    lapply(pull_game_info_from_pages)
  
  return(raw_tbl)
}

remove_noncompliant_tournaments_from_list <-
  function(tourneys_list) {
    tourneys_list %>%
      purrr::keep(is_tibble) %>%
      purrr::keep(function(x)
        ncol(x) == 23) %>%
      purrr::keep(function(x)
        ! "stage" %in% names(x)) %>%
      return()
  }

combine_pages_from_object <- function(tournament_object) {
  if (length(tournament_object) == 0) {
    return(NULL)
  }
  
  tournament_id <- tournament_object %>% .[[1]] %>% .[['id']]
  
  print(paste0("Current ID: ", tournament_id))
  
  unnested_df <- tryCatch(
    tournament_object %>%
      lapply(as.tibble, .name_repair = "unique") %>%
      bind_rows() %>%
      unnest(everything(), names_repair = "unique") %>%
      unnest(everything(), names_repair = "unique") %>%
      unnest(everything(), names_repair = "unique") %>%
      unnest(everything(), names_repair = "unique") %>%
      unnest(everything(), names_repair = "unique") %>%
      unnest(everything(), names_repair = "unique") %>%
      bind_rows(),
    error = function(e) {
      message(paste0("Error at ", tournament_id))
    }
  )
  
  return(unnested_df)
}



discard_noncompliant_dataframes <- function(tournament_df) {
  if (is.null(tournament_df)) {
    return(NULL)
  }
  
  print(distinct(tournament_df %>% dplyr::select(id...1)) %>% pull())
  
  if (ncol(tournament_df) == 23 &&
      !"stage" %in% names(tournament_df)) {
    tournament_df %>%
      rename(
        tournament_id = id...1,
        tournament_name = name...2,
        attendee_count = numAttendees,
        start_date = startAt,
        end_date = endAt,
        event_id = id...6,
        event_state = state,
        is_online = isOnline,
        event_name = name...9,
        competition_tier = competitionTier,
        event_type = type,
        set_id = id...12,
        set_round = round,
        set_name = fullRoundText,
        match_id = id...15,
        match_round = orderNum,
        selection_id = id...17,
        selection_type = selectionType,
        selection_value = selectionValue,
        player_id = id...20,
        player_name = name...21,
        winner_id = winnerId,
        stage_name = name...23
      ) %>%
      return()
  } else {
    return(NULL)
  }
}

pull_tournament_res_from_id <- function(tournament_id) {
  
  games_in_tourney_query <- '
query gamesInTournament($tournamentId: ID!, $page: Int!, $perPage: Int!) {
  tournament(id: $tournamentId) {
      id
      name
      numAttendees
      startAt
      endAt
      events (limit: 1000, filter: {videogameId: 1386}){
        id
        state
        isOnline
        name
      	competitionTier
      	type
        sets(page: $page,
        perPage: $perPage
      ){
        nodes {
          id
          round
          fullRoundText
          games {
            id
            orderNum
            selections {
              id
              selectionType
              selectionValue
              entrant {
                id
                name
              }
            }
            winnerId
            stage {
              name
            }
          }
        }
      }
    }
  }
}
'
  
  list_of_res_json <- list()
  
  i <- 1
  
  q <- Query$new()$query('url', games_in_tourney_query)
  
  print(paste0("Currently querying tournament: ", tournament_id))
  
  while (T) {
    q_var <- list(tournamentId = tournament_id,
                  page = i,
                  perPage = 10)
    
    res_json <- con$exec(q$url, variables = q_var) %>%
      fromJSON(flatten = F)
    
    if (res_json %>%
        .[['data']] %>%
        .[['tournament']] %>%
        .[['events']] %>%
        .[['sets']] %>%
        .[['nodes']] %>%
        .[[1]] %>%
        length() == 0) {
      print(paste0("Finished at page: ", i))
      break
    } else {
      
      unnested_json <- res_json %>%
        .[['data']] %>%
        .[['tournament']]
      
      if (length(unnested_json) != 0) {
        list_of_res_json[[length(list_of_res_json) + 1]] <- unnested_json
      }
      
      print(paste0("Successfully appended page: ", i))
      Sys.sleep(0.8)
      i <- i + 1
    }
  }
  
  return(list_of_res_json)
  
}

parse_tournament_object <- function(tournament_object) {
  if (length(tournament_object) == 0) {
    print("Empty")
    return(NULL)
  }
  
  tournament_id <-
    tournament_object %>%
    .[[1]] %>%
    .[['id']]
  
  print(paste0("Current ID: ", tournament_id))
  
  unnested_df <- NULL
  
  tryCatch(
    unnested_df <- tournament_object %>%
      .[[1]] %>%
      as.tibble() %>%
      unnest(everything(), names_repair = "unique") %>%
      unnest(everything(), names_repair = "unique") %>%
      unnest(everything(), names_repair = "unique") %>%
      dplyr::filter(games != "NULL") %>%
      unnest(col = games, names_repair = "unique") %>%
      unnest(everything(), names_repair = "unique") %>%
      unnest(everything(), names_repair = "unique"),
    error = function(e) {
      message(print("Failure"))
    }
  )
  
  return(unnested_df)
  
}
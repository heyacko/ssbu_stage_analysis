tourney_query <- '
query TournamentsByVideogame($tournamentId: ID!) {
  tournament(id: $tournamentId) {
      id
      name
      numAttendees
      state
      startAt
      endAt
      events (limit: 100, filter: {videogameId: 1386}){
        id
        isOnline
        name
      	competitionTier
      	type
    }
  }
}
'

tournament_21_df <- t %>% dplyr::select(id) %>% pull() %>% .[1:10] %>%
  lapply(pull_tourney_data_from_id) %>%
  lapply(tidy_tourney_data) %>%
  bind_rows()

tournament_df %>%
  view()

relevant_ids <- tournament_df %>%
  dplyr::select(event_id) %>%
  pull()

pull_tourney_data_from_id <- function(tournament_id) {
  
  q_var <- list(tournamentId = tournament_id)
  
  q <- Query$new()$query('url', tourney_query)
  
  res_json <- con$exec(q$url, variables = q_var) %>%
    fromJSON(flatten = F)
  
  res_json %>%
    #tidy_tourney_data() %>%
    return()
}

tidy_tourney_data <- function(res_json) {
  res_json %>%
    .[['data']] %>%
    .[['tournament']] %>%
    as_tibble() %>%
    unnest() %>%
    rename(
      tournament_id = id,
      tournament_name = name,
      attendee_count = numAttendees,
      date_start = startAt,
      date_end = endAt,
      event_id = id1,
      is_online = isOnline,
      event_name = name1,
      competition_tier = competitionTier,
      event_type = type
    ) %>%
    dplyr::filter(event_type == 1) %>%
    return()
}

###############################################################################
tourn_event_list <- pull_tournaments_from_pages(1641024000)

tourn_event_df <- tourn_event_list %>%
  lapply(tidy_tourney_data) %>%
  bind_rows()

tourn_event_df

pull_tournaments_from_pages <- function(date_timestamp) {
  
  list_of_res_json <- list()
  
  i <- 1
  
  while (T) {
    q_var <- list(afterDate= date_timestamp,
                  page= i,
                  perPage = 100,
                  videogameId= 1386)
    
    res_json <- con$exec(q$url, variables = q_var) %>%
      fromJSON(flatten = F) %>%
      .[['data']] 
    
    if (is.null(res_json %>%
        .[['tournaments']])) {
      break
    } else {
      list_of_res_json[[length(list_of_res_json) + 1]] <- res_json
      i <- i + 1
      Sys.sleep(1)
    }
  }
  
  return(list_of_res_json)
  
}
































































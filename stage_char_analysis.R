library(httr)
library(ghql)
library(rvest)
library(dplyr)

gg_key <- "YOUR_API_KEY"

url <- "https://api.start.gg/gql/alpha"

con <- GraphqlClient$new(url = url,
                         headers= list('Authorization'= paste0("Bearer ", gg_key),
                                       'Content-Type'= 'application/json'))
###############################################################################

game_df_21_jan_h1 <- tournaments_21 %>% 
  dplyr::filter(!is.na(numAttendees)) %>%
  dplyr::filter(start_date <= "2021-01-15") %>%
  dplyr::select(id) %>% 
  pull() %>%
  lapply(pull_game_info_from_pages) %>%
  lapply(tidy_game_data) %>%
  bind_rows()

game_df_21_jan_h1 %>% write.csv("game_df_21_jan_h1.csv")
t %>% write.csv("tournaments_21.csv")

game_df_21_jan_h2 <- tournaments_21 %>% 
  dplyr::filter(!is.na(numAttendees)) %>%
  dplyr::filter(start_date > "2021-01-15") %>%
  dplyr::filter(start_date <= "2021-01-31") %>%
  dplyr::select(id) %>%
  pull() %>%
  create_games_df_from_ids_list()

game_df_21_jan_h2_tbl <- game_df_21_jan_h2 %>%
  lapply(parse_tournament_object) %>%
  remove_noncompliant_tournaments_from_list() %>%
  lapply(tidy_game_data) %>%
  bind_rows()

game_df_21_jan_h2_tbl %>%
  remove_noncompliant_tournaments_from_list() %>%
  lapply(tidy_game_data) %>%
  bind_rows()

test %>%
  lapply(as.tibble)

test <- pull_ids_from_tourney_df(tournaments_21, "2021-02-01", "2021-02-15") %>%
  lapply(pull_tournament_res_from_id)
  
t <- test %>%
  lapply(combine_pages_from_object)
  

combine_pages_from_object <- function(tournament_object) {
  
  if (length(tournament_object) == 0) {
    return(NULL)
  }
  
  tournament_id <- tournament_object %>% .[[1]] %>% .[['id']]
  
  print(paste0("Current ID: ", tournament_id))
  
  unnested_df <- tournament_object %>%
    lapply(as.tibble, .name_repair= "unique") %>%
    bind_rows() %>%
    unnest(everything(), names_repair = "unique") %>%
    unnest(everything(), names_repair = "unique") %>%
    unnest(everything(), names_repair = "unique") %>%
    unnest(everything(), names_repair = "unique") %>%
    unnest(everything(), names_repair = "unique") %>%
    unnest(everything(), names_repair = "unique") %>%
    bind_rows()
  
  if (ncol(unnested_df) == 23 && !"stage" %in% names(unnested_df)){
    unnested_df %>%
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
  } else if ("stage" %in% names(unnested_df)) {
    return(NULL)
  } else {
    return(NULL)
  }
}

game_df_21_feb_h2 <- tournaments_21 %>% 
  dplyr::filter(!is.na(numAttendees)) %>%
  dplyr::filter(start_date > "2021-02-15") %>%
  dplyr::filter(start_date <= "2021-02-28") %>%
  dplyr::select(id) %>% 
  pull() %>%
  create_games_df_from_ids_list()

game_df_21_q2 <- tournaments_21 %>%
  dplyr::filter(!is.na(numAttendees)) %>%
  dplyr::filter(start_month %in% c(4:6)) %>%
  dplyr::select(id) %>%
  pull() %>%
  lapply(pull_game_info_from_pages) %>%
  lapply(tidy_game_data) %>%
  bind_rows()
  
game_df_21_q3 <- t %>% 
  dplyr::filter(!is.na(numAttendees)) %>%
  dplyr::filter(start_month %in% c(7:9)) %>%
  dplyr::select(id) %>% 
  pull() %>%
  lapply(pull_game_info_from_pages) %>%
  lapply(tidy_game_data) %>%
  bind_rows()

game_df_21_q4 <- t %>%
  dplyr::filter(!is.na(numAttendees)) %>%
  dplyr::filter(start_month %in% c(10:12)) %>%
  dplyr::select(id) %>%
  pull() %>%
  lapply(pull_game_info_from_pages) %>%
  lapply(tidy_game_data) %>%
  bind_rows()

###############################################################################

q_var <- list(afterDate= start_date,
              beforeDate= end_date,
              page= 6,
              perPage = 500,
              videogameId= 1386)

q <- Query$new()$query('url', search_tourneys_query)

res_json <- con$exec(q$url, variables = q_var) %>%
  fromJSON(flatten = F)

res_json %>%
  .[['data']] %>%
  .[['tournaments']] %>%
  .[['nodes']] %>% length()
  as_tibble() %>%
  unnest()

###############################################################################
# input tournament id, return every game within every event in tournament

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

create_games_df_from_ids_list <- function(ids_list) {
  
  raw_tbl <- ids_list %>%
    lapply(pull_game_info_from_pages)

  return(raw_tbl)
}

remove_noncompliant_tournaments_from_list <- function(tourneys_list) {
  tourneys_list %>%
    purrr::keep(is_tibble) %>%
    purrr::keep(function(x) ncol(x) == 23) %>%
    purrr::keep(function(x) !"stage" %in% names(x)) %>%
    return()
}

pull_tournament_res_from_id <- function(tournament_id) {
  list_of_res_json <- list()
  
  i <- 1
  
  q <- Query$new()$query('url', games_in_tourney_query)
  
  print(paste0("Currently querying tournament: ", tournament_id))
  
  while (T) {
    q_var <- list(tournamentId = tournament_id,
                  page = i,
                  perPage = 50)
    
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
      #unnested_json <- tryCatch(
      #  parse_res_json(res_json),
      #  error= function(e) {
      #    message(paste0("Error occurred at: ", tournament_id, ", page: ", i))
      #  }
      #)
      
      unnested_json <- res_json %>%
        .[['data']] %>%
        .[['tournament']]
      
      #list_of_res_json[[length(list_of_res_json) + 1]] <- unnested_json
      
      if (length(unnested_json) != 0) {
        list_of_res_json[[length(list_of_res_json) + 1]] <- unnested_json
      }
      
      print(paste0("Successfully appended page: ", i))
      Sys.sleep(1)
      i <- i + 1
    }
  }
  
  return(
    list_of_res_json
  )
  
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

tidy_game_data <- function(raw_tbl) {
  
  raw_tbl %>%
    rename(
      tournament_id = id...1,
      tournament_name = name...2,
      attendee_count = numAttendees,
      start_date = startAt,
      end_date = endAt,
      event_id = id...6,
      state= event_state,
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
    mutate_all(as.character) %>%
    return()
}
















































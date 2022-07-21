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
# initial pull of tournament objects
games_jan_h1 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-01-01", "2021-01-15") %>%
  lapply(pull_tournament_res_from_id)

games_jan_h2 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-01-16", "2021-01-31") %>%
  lapply(pull_tournament_res_from_id)

games_feb_h1 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-02-01", "2021-02-15") %>%
  lapply(pull_tournament_res_from_id)

games_feb_h2 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-02-16", "2021-02-28") %>%
  lapply(pull_tournament_res_from_id)

games_mar_h1 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-03-01", "2021-03-15") %>%
  lapply(pull_tournament_res_from_id)

games_mar_h2 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-03-16", "2021-03-31") %>%
  lapply(pull_tournament_res_from_id)

games_apr_h1 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-04-01", "2021-04-15") %>%
  lapply(pull_tournament_res_from_id)

games_apr_h2 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-04-16", "2021-04-30") %>%
  lapply(pull_tournament_res_from_id)

games_may_h1 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-05-01", "2021-05-15") %>%
  lapply(pull_tournament_res_from_id)

games_may_h2 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-05-16", "2021-05-31") %>%
  lapply(pull_tournament_res_from_id)

games_jun_h1 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-06-01", "2021-06-15") %>%
  lapply(pull_tournament_res_from_id)

games_jun_h2 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-06-16", "2021-06-30") %>%
  lapply(pull_tournament_res_from_id)

###############################################################################

games_jan_h1 <- games_jan_h1 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_jan_h1 %>% write.csv("data/games_jan_h1_21.csv")

games_feb_h1 <- games_feb_h1 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_feb_h1 %>%
  write.csv("data/games_feb_h1_21.csv")

games_feb_h2 <- games_feb_h2 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_feb_h2 %>%
  write.csv("data/games_feb_h2_21.csv")

games_mar_h1 <- games_mar_h1 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_mar_h1 %>%
  write.csv("data/games_mar_h1_21.csv")

games_apr_h1 <- games_apr_h1 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_apr_h1 %>%
  write.csv("data/games_apr_h1_21.csv")

games_apr_h2 <- games_apr_h2 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_apr_h2 %>%
  write.csv("data/games_apr_h2_21.csv")

games_may_h2 <- games_may_h2 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_may_h2 %>%
  write.csv("data/games_may_h2_21.csv")

games_jun_h1 <- games_jun_h1 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_jun_h1 %>%
  write.csv("data/games_jun_h1_21.csv")

games_jun_h2 <- games_jun_h2 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_jun_h2 %>%
  write.csv("data/games_jun_h2_21.csv")

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
  
  if (ncol(tournament_df) == 23 && !"stage" %in% names(tournament_df)){
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
















































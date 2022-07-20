library(httr)
library(ghql)
library(rvest)

gg_key <- "959b07076cb22e08be35841d23ff7cc9"

url <- "https://api.start.gg/gql/alpha"

con <- GraphqlClient$new(url = url,
                         headers= list('Authorization'= paste0("Bearer ", gg_key),
                                       'Content-Type'= 'application/json'))
###############################################################################

game_df_21_jan_h1 <- t %>% 
  dplyr::filter(!is.na(numAttendees)) %>%
  dplyr::filter(start_date <= "2021-01-15") %>%
  dplyr::select(id) %>% 
  pull() %>%
  lapply(pull_game_info_from_pages) %>%
  lapply(tidy_game_data) %>%
  bind_rows()

game_df_21_jan_h1 %>% write.csv("game_df_21_jan_h1.csv")
t %>% write.csv("tournaments_21.csv")

game_df_21_jan_h2 <- t %>% 
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

game_df_21_feb <- t %>% 
  dplyr::filter(!is.na(numAttendees)) %>%
  dplyr::filter(start_date > "2021-02-01") %>%
  dplyr::filter(start_date <= "2021-02-15") %>%
  dplyr::select(id) %>% 
  pull() %>%
  .[1:3] %>%
  create_games_df_from_ids_list()

game_df_21_feb_h2 <- t %>% 
  dplyr::filter(!is.na(numAttendees)) %>%
  dplyr::filter(start_date > "2021-02-15") %>%
  dplyr::filter(start_date <= "2021-02-28") %>%
  dplyr::select(id) %>% 
  pull() %>%
  create_games_df_from_ids_list()

game_df_21_q2 <- t %>%
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

search_tourneys_query <- '
query TournamentsByVideogame($beforeDate: Timestamp!, $afterDate: Timestamp!, 
  $page:Int!, $perPage: Int!, $videogameId: ID!) {
  tournaments(query: {
    perPage: $perPage
    page: $page
    filter: {
      afterDate: $afterDate,
      beforeDate: $beforeDate,
      published: true,
      videogameIds: [
        $videogameId
      ]
    }
  }) {
    nodes {
      id
      name
      numAttendees
      startAt
      endAt
    }
  }
}

'

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

start_date <- as.numeric(as.POSIXct('2022-01-01', '%Y-%m-%d'))  
end_date <- as.numeric(as.POSIXct('2022-02-01', '%Y-%m-%d'))  
  
pull_tournaments_from_pages(start_date, end_date)

t <- c(pull_tournaments_from_pages('2021-01-01', '2021-02-01'),
       pull_tournaments_from_pages('2021-02-02', '2021-03-01'),
       pull_tournaments_from_pages('2021-03-02', '2021-04-01'),
       pull_tournaments_from_pages('2021-04-02', '2021-05-01'),
       pull_tournaments_from_pages('2021-05-02', '2021-06-01'),
       pull_tournaments_from_pages('2021-06-02', '2021-07-01'),
       pull_tournaments_from_pages('2021-07-02', '2021-08-01'),
       pull_tournaments_from_pages('2021-08-02', '2021-09-01'),
       pull_tournaments_from_pages('2021-09-02', '2021-10-01'),
       pull_tournaments_from_pages('2021-10-02', '2021-11-01'),
       pull_tournaments_from_pages('2021-11-02', '2021-12-01'),
       pull_tournaments_from_pages('2021-12-02', '2022-01-01')) %>%
  bind_rows() %>%
  unnest()

t %>% dplyr::select(id) %>% pull()

#pull_tournaments_from_pages <- function(start_date, stop_date, page_start) {
#  
#  start_date_formatted <- as.numeric(as.POSIXct(start_date, '%Y-%m-%d'))
#  end_date_formatted <- as.numeric(as.POSIXct(stop_date, '%Y-%m-%d')) 
#  
#  if (missing(page_start)) {
#    i <- 1
#  } else {
#    i <- page_start
#  }
#  
#  list_of_res_json <- list()
#  
#  q <- Query$new()$query('url', search_tourneys_query)
#  
#  while (T) {
#    q_var <- list(afterDate= start_date_formatted,
#                  beforeDate= end_date_formatted,
#                  page= i,
#                  perPage = 500,
#                  videogameId= 1386)
#    
#    res_json <- con$exec(q$url, variables = q_var) %>%
#      fromJSON(flatten = F) %>%
#      .[['data']] 
#    
#    if (res_json %>%
#                .[['tournaments']] %>%
#                .[['nodes']] %>%
#                length() == 0) {
#      print(paste0("Found end of query at page: ", i))
#      break
#    } else {
#      list_of_res_json[[length(list_of_res_json) + 1]] <- res_json
#      print(paste0("Successfully appended page: ", i))
#      i <- i + 1
#      Sys.sleep(0.5)
#    }
#  }
#  
#  return(list_of_res_json)
#  
#}


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
  
  tryCatch(
    tidy_tbl <- raw_tbl %>%
      remove_noncompliant_tournaments_from_list %>%
      lapply(parse_tournament_object),
    error = function(e) {
      message(print(e))
      return(raw_tbl)
    }
  )
    
  tidy_tbl %>%
    lapply(tidy_game_data) %>%
    bind_rows() %>%
    return()
  
}

remove_noncompliant_tournaments_from_list <- function(tourneys_list) {
  tourneys_list %>%
    purrr::keep(is_tibble) %>%
    purrr::keep(function(x) ncol(x) == 23) %>%
    purrr::keep(function(x) !"stage" %in% names(x)) %>%
    return()
}

pull_game_info_from_pages <- function(tournament_id, trial= F) {
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
      
      list_of_res_json[[length(list_of_res_json) + 1]] <- unnested_json
      
      #if (length(unnested_json) != 0) {
      #  list_of_res_json[[length(list_of_res_json) + 1]] <- unnested_json
      #}
      
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
      tournament_state = state,
      start_date = startAt,
      end_date = endAt,
      event_id = id...7,
      is_online = isOnline,
      videogame = name...9,
      competition_tier = competitionTier,
      event_type = type,
      set_id = id...12,
      set_round = round,
      set_name = fullRoundText,
      game_id = id...15,
      game_round = orderNum,
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
















































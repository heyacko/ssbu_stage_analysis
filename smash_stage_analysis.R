library(httr)
library(ghql)
library(rvest)

gg_key <- "959b07076cb22e08be35841d23ff7cc9"

url <- "https://api.start.gg/gql/alpha"

con <- GraphqlClient$new(url = url,
                         headers= list('Authorization'= paste0("Bearer ", gg_key),
                                    'Content-Type'= 'application/json'))

raw_query <- 'query getStages($eventId: ID!, $page: Int!, $perPage: Int!) {
  event(id: $eventId) {
    id
    name
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
'

###############################################################################
# get character lookup table

smash_char_lookup <- 'https://api.smash.gg/characters?videogameId=1386' %>%
  read_html() %>%
  html_text() %>%
  fromJSON() %>%
  .[['entities']] %>%
  .[['character']] %>%
  as_tibble() %>%
  dplyr::select(id, name) %>%
  rename(character_id= id,
         character_name= name)

###############################################################################

relevant_ids[1:10]

format_events_df <- function(df) {
  df %>%
    lapply(lapply, tidy_raw_data) %>%
    lapply(lapply, compact) %>%
    lapply(compact) %>%
    bind_rows() %>%
    return()
}

final_events_df <- list(first_qtr_events_df %>% format_events_df(),
  second_qtr_events_df %>% format_events_df(),
  third_qtr_events_df %>% format_events_df(),
  fourth_qtr_events_df %>% format_events_df(),
  fifth_qtr_events_df %>% format_events_df(),
  sixth_qtr_events_df %>% format_events_df(),
  seventh_qtr_events_df %>% format_events_df(),
  eighth_qtr_events_df %>% format_events_df()) %>%
  bind_rows()

final_events_df <- final_events_df %>%
  left_join(smash_char_lookup)

final_events_df %>%
  dplyr::filter(!is.na(stage_name)) %>%
  dplyr::filter(!is.na(player_name)) %>%
  group_by(character_name) %>%
  summarise(games= n_distinct(game_id))
  
final_events_df %>%
  distinct(player_name) %>%
  view()

final_events_df %>%
  dplyr::filter(!is.na(stage_name)) %>%
  group_by(character_name, stage_name) %>%
  summarise(count= n_distinct(game_id),
            wins= sum(is_win),
            win_rate= wins/count) %>%
  view()

### 2022

first_qtr_events_df <- relevant_ids[1:10] %>%
  lapply(pull_json_from_pages)

second_qtr_events_df <- relevant_ids[11:20] %>%
  lapply(pull_json_from_pages)

third_qtr_events_df <- relevant_ids[21:30] %>%
  lapply(pull_json_from_pages)

fourth_qtr_events_df <- relevant_ids[31:40] %>%
  lapply(pull_json_from_pages)

### 2021

fifth_qtr_events_df <- relevant_ids[41:50] %>%
  lapply(pull_json_from_pages)

sixth_qtr_events_df <- relevant_ids[51:60] %>%
  lapply(pull_json_from_pages)

seventh_qtr_events_df <- relevant_ids[61:70] %>%
  lapply(pull_json_from_pages)

eighth_qtr_events_df <- relevant_ids[71:86] %>%
  lapply(pull_json_from_pages)

### 

second_qtr_events_df %>%
  .[[8]] %>%
  lapply(tidy_raw_data) %>%
  bind_rows()

###############################################################################
# given event_id, paginate through sets, appending to list_of_res_json until empty

pull_json_from_pages <- function(event_id) {
  
  q <- Query$new()$query('url',raw_query)
  
  list_of_res_json <- list()
  
  i <- 1
  
  while (T) {
    q_var <- list(eventId = event_id,
                  page = i,
                  perPage = 10)
    
    res_json <- con$exec(q$url, variables = q_var) %>%
      fromJSON(flatten = F) %>%
      .[['data']] 
    
    if (length(res_json %>% 
               .[['event']] %>% 
               .[['sets']] %>% 
               .[['nodes']]) == 0) {
      break
    } else {
      list_of_res_json[[length(list_of_res_json) + 1]] <- res_json
      i <- i + 1
      Sys.sleep(1)
    }
  }
  
  return(list_of_res_json)
  
}

tidy_raw_data <- function(raw_tbl) {
  cleaned_tbl <- raw_tbl %>%
    .[['event']] %>% 
    .[['sets']] %>% 
    .[['nodes']] %>%
    compact() %>%
    bind_rows() %>%
    unnest() %>%
    unnest() %>%
    unnest() %>%
    as.tibble()
  
  if (length(cleaned_tbl) != 12) {
    return(
      NULL
      )
  } else {
    names(cleaned_tbl) <-
      c(
        'set_id',
        'set_int',
        'set_name',
        'game_id',
        'game_order',
        'selection_id',
        'selection_type',
        'selection_value',
        'player_id',
        'player_name',
        'winner_id',
        'stage_name'
      )
    
    cleaned_tbl %>%
      mutate(is_win= ifelse(player_id == winner_id,1,0),
                          character_id= selection_value) %>%
    return(cleaned_tbl)
  }
  
}

###############################################################################

res_tbl_origin <- res_json %>%
  .[['data']] %>%
  .[['event']] %>%
  .[['sets']] %>%
  compact() %>%
  bind_rows() %>%
  unnest() %>%
  unnest() %>%
  unnest() %>%
  as.tibble()

res_tbl_raw %>%
  view()

res_tbl_raw_names <- c('set_id','set_int','set_name','game_id','game_order',
                       'selection_id','selection_type','selection_value',
                       'player_id','player_name','winner_id','stage_name')

names(res_tbl_raw) <- res_tbl_raw_names

stage_tbl <- res_tbl_raw %>%
  mutate(is_win= ifelse(player_id == winner_id,1,0),
         character_id= selection_value) %>%
  dplyr::select(-c(selection_type, selection_id, selection_value))

stage_tbl %>%
  dplyr::filter(!is.na(stage_name)) %>%
  group_by(stage_name) %>%
  summarise(num_games= n_distinct(game_id)) %>%
  mutate(pct_usage= num_games/sum(num_games)) %>%
  arrange(desc(num_games))
  


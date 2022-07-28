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

games_mar_h2_1 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-03-16", "2021-03-22") %>%
  lapply(pull_tournament_res_from_id)

games_mar_h2_2_1 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-03-23", "2021-03-26") %>%
  lapply(pull_tournament_res_from_id)

games_mar_h2_2_2 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-03-27", "2021-03-28") %>%
  lapply(pull_tournament_res_from_id)

games_mar_h2_3 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-03-29", "2021-03-31") %>%
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

games_7_21 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-07-01", "2021-07-31") %>%
  lapply(pull_tournament_res_from_id)

games_7_21 <- games_7_21 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_8_h1_21 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-08-01", "2021-08-15") %>%
  lapply(pull_tournament_res_from_id) %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_8_h2_21 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-08-16", "2021-08-31") %>%
  lapply(pull_tournament_res_from_id) %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_9_h1_21 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-09-01", "2021-09-15") %>%
  lapply(pull_tournament_res_from_id) %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_9_h2_21 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-09-16", "2021-09-30") %>%
  lapply(pull_tournament_res_from_id) %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_10_h1_21 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-10-01", "2021-10-15") %>%
  lapply(pull_tournament_res_from_id)

games_10_h2_21 <-
  pull_ids_from_tourney_df(tournaments_21, "2021-10-16", "2021-10-31") %>%
  lapply(pull_tournament_res_from_id)

###############################################################################

games_jan_h1 <- games_jan_h1 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_jan_h1 %>% write.csv("data/games_jan_h1_21.csv")

games_jan_h2_t <- games_jan_h2 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes)

games_jan_h2 <- games_jan_h2_t %>%
  discard(is.null) %>%
  lapply(mutate, set_id= as.character(set_id)) %>%
  bind_rows()

games_jan_h2 %>% write.csv("data/games_1_h2_21.csv")

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

games_mar_h2 <- c(games_mar_h2_1,games_mar_h2_2_1,games_mar_h2_2_2,
                  games_mar_h2_3) %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_mar_h2 %>%
  write.csv("data/games_mar_h2_21.csv")

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

games_may_h1 <- games_may_h1 %>%
  lapply(combine_pages_from_object) %>%
  lapply(discard_noncompliant_dataframes) %>%
  bind_rows()

games_may_h1 %>%
  write.csv("data/games_5_h1_21.csv")

games_5_21 <- games_may_h1

games_jun_h2 <- c(games_jun_h2) %>%
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

games_6_21 <- games_6_h1_21 %>%
  rbind(games_6_h2_21) %>%
  select(-...1)

games_6_21 %>% write.csv("data/games_6_21.csv", row.names = F)

games_4_21 <- games_4_h1_21 %>%
  rbind(games_4_h2_21) %>%
  select(-...1)

games_4_21 %>% write.csv("data/games_4_21.csv", row.names = F)

games_2_21 <- games_2_h1_21 %>%
  rbind(games_2_h2_21) %>%
  select(-...1)

games_2_21 %>% write.csv("data/games_2_21.csv", row.names = F)

games_3_21 <- games_3_h1_21 %>%
  rbind(games_3_h2_21) %>%
  select(-...1)

games_3_21 %>% write.csv("data/games_3_21.csv", row.names = F)

games_1_21 <- games_1_h1_21 %>%
  rbind(games_1_h2_21) %>%
  select(-...1)

games_1_21 %>% write.csv("data/games_1_21.csv", row.names = F)

games_5_21 <- games_5_h1_21 %>%
  rbind(games_5_h2_21) %>%
  select(-...1)

games_5_21 %>% write.csv("data/games_5_21.csv", row.names = F)

games_8_21 <- games_8_h1_21 %>%
  rbind(games_8_h2_21)

games_8_21 %>% write.csv("data/games_8_21.csv", row.names = F)

games_7_21 %>% write.csv("data/games_7_21.csv", row.names = F)

































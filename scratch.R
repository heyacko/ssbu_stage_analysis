games_h1 <- games_1_21 %>%
  rbind(games_2_21) %>%
  rbind(games_3_21) %>%
  rbind(games_4_21) %>%
  rbind(games_5_21) %>%
  rbind(games_6_21) 

games_h1 %>%
  dplyr::filter(!is.na(stage_name)) %>%
  mutate(is_win= ifelse(player_id == winner_id,1,0),
         character_id= selection_value) %>%
  left_join(smash_char_lookup) %>%
  group_by(character_name, stage_name) %>%
  summarise(win_rate= sum(is_win, na.rm=T)/n_distinct(match_id),
            games= n_distinct(match_id)) %>%
  dplyr::filter(games > 1000) %>%
  arrange(character_name, desc(games), desc(win_rate)) %>%
  view()

games_h1 %>%
  dplyr::filter(!is.na(stage_name)) %>%
  mutate(is_win= ifelse(player_id == winner_id,1,0),
         character_id= selection_value) %>%
  left_join(smash_char_lookup) %>%
  group_by(character_name, stage_name) %>%
  summarise(win_rate= sum(is_win, na.rm=T)/n_distinct(match_id),
            games= n_distinct(match_id)) %>%
  dplyr::filter(games > 1000) %>%
  arrange(character_name, desc(games)) %>%
  dplyr::filter(stage_name == "Battlefield") %>%
  view()

games_h1 %>%
  dplyr::filter(!is.na(stage_name)) %>%
  slice(1:100) %>%
  view()













































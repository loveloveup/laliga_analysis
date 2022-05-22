devtools::install_github("JaseZiv/worldfootballR")
library(tidyverse)
library(worldfootballR)
team_urls <- understat_team_meta(team_names = "Barcelona")

# リーグ結果
epl_results <- understat_league_match_results(league = "EPL", season_start_year = 2021)
epl_results <- tibble(epl_results)
epl_results %>% 
  count(season)

away <- epl_results %>% 
  group_by(away_team) %>% 
  summarise(xG = mean(away_xG)) %>% 
  rename("home_team" = away_team)

xG_2122 <- epl_results %>% 
  group_by(home_team) %>% 
  summarise(xG = mean(home_xG)) %>% 
  left_join(away, by = "home_team") %>% 
  rename("team" = home_team) %>% 
  mutate(xG = (xG.x+xG.y)/2) %>% 
  arrange(desc(xG))
xG_2122 %>% 
  ggplot(aes(x = xG.x,xG.y))+
  geom_point()+
  ggrepel::geom_label_repel(label = xG_2122$team)+
  ylim(0,3)+
  xlim(0,3)


# 選手???詳細なプレイログ
Menphis_shots <- 
  understat_player_shots(player_url = "https://understat.com/player/555")
Menphis_shots %>% 
  count(season)


# 試合ごとの詳細なプレイログ
Barcelona2021 <- understat_team_season_shots(team_url = "https://understat.com/team/Barcelona/2021")
Barcelona2021 <- as_tibble(Barcelona2021)

n_distinct(Barcelona2021$match_id)
ManU_team_players <- understat_team_players_stats(team_url = "https://understat.com/team/Manchester_United/2021")
dplyr::glimpse(ManU_team_players)
as_tibble(ManU_team_players) 



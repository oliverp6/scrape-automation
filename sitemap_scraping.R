##### PART ONE OF THE FOOTBALL SCRIPT - This part cannot run on a databricks workbook and therefore needs to be scheduled locally.

library(worldfootballR)
library(tidyverse)
library(lubridate)

topsix_URLS <- c("https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats",
                 "https://fbref.com/en/squads/822bd0ba/Liverpool-Stats",
                 "https://fbref.com/en/squads/cff3d9bb/Chelsea-Stats",
                 "https://fbref.com/en/squads/19538871/Manchester-United-Stats",
                 "https://fbref.com/en/squads/18bb7c10/Arsenal-Stats",
                 "https://fbref.com/en/squads/361ca564/Tottenham-Hotspur-Stats",
                 "https://fbref.com/en/squads/1862c019/England-Men-Stats")


getres <-  get_team_match_results(topsix_URLS,time_pause = 3)

new_fixtures_cleaned <- getres %>% 
  mutate(home = ifelse(Venue == "Home",Team,
                       ifelse(Venue == "Neutral",Team,Opponent)),
         away = ifelse(Venue == "Away",Team,
                       ifelse(Venue == "Neutral",Opponent,Opponent)),
         home_goals = ifelse(Venue == "Home",GF,
                             ifelse(Venue == "Away",GA,
                                    ifelse(Venue == "Neutral",GF,NA))),
         away_goals = ifelse(Venue == "Away",GF,
                             ifelse(Venue == "Home",GA,
                                    ifelse(Venue == "Neutral",GA,NA)))) %>%
  select(Comp,Date,Time,Round,Day,home,away,home_goals,away_goals)  %>%
  mutate(id = gsub(x = paste0(str_to_lower(substr(Comp,1,3)),Date,substr(home,1,3),substr(away,1,3)),replacement = "",pattern = " "),
         dt = ymd_hm(paste(Date,Time,sep = " ")),
         est_enddt = as.character(dt + minutes(110)),
         dt = as.character(dt)) %>% arrange(dt) %>% filter(duplicated(id) == "FALSE")


write_csv(new_fixtures_cleaned,paste0('data/xml_url_count.csv'),append = T)    

library(shiny)
library(shinythemes)
library(dplyr)
library(RCurl)
library(png)
library(hoopR)
library(tidyverse)
library(knitr)
library(kableExtra)
library(tinytex)
library(bigballR)
library("xlsx")
library(ncaahoopR)
library(ggplot2)
library("toRvik")
library(gt)
library(here)

cbb_stats_2023 <- bart_player_season(year = 2023, stat = 'all')
cbb_stats_2022 <- bart_player_season(year = 2022, stat = 'all')

freshmen_rankings_2023 <- read.xlsx("/Users/lukespring/Desktop/2022-23_Freshmen.xlsx", sheetName = "Rankings")

ken_pom_2022_23 <- read.xlsx("/Users/lukespring/Desktop/Ken_Pom_2022-23.xlsx", sheetName = "Efficiency")

cbb_stats_2022$`Year 2022` <- 2022
cbb_stats_2022$`Player 2022` <- cbb_stats_2022$player
cbb_stats_2022$`Conf 2022` <- cbb_stats_2022$conf
cbb_stats_2022$`Team 2022` <- cbb_stats_2022$team
cbb_stats_2022$`PPG 2022` <- cbb_stats_2022$ppg
cbb_stats_2022$`Adj. OE 2022` <- cbb_stats_2022$adj_oe
cbb_stats_2022$`Adj. DE 2022` <- cbb_stats_2022$adj_de

cbb_stats_2023$`Year 2023` <- 2023
cbb_stats_2023$`Player 2023` <- cbb_stats_2023$player
cbb_stats_2023$`Conf 2023` <- cbb_stats_2023$conf
cbb_stats_2023$`Team 2023` <- cbb_stats_2023$team
cbb_stats_2023$`PPG 2023` <- cbb_stats_2023$ppg
cbb_stats_2023$`Adj. OE 2023` <- cbb_stats_2023$adj_oe
cbb_stats_2023$`Adj. DE 2023` <- cbb_stats_2023$adj_de

cbb_stats_2022 <- subset(cbb_stats_2022, select = c(`Player 2022`, id, `Conf 2022`, `Year 2022`, `Team 2022`, 
                                                    `PPG 2022`,`Adj. OE 2022`, `Adj. DE 2022`))

cbb_stats_2023 <- subset(cbb_stats_2023, select = c(`Player 2023`, id, `Conf 2023`, `Year 2023`, `Team 2023`, 
                                                    `PPG 2023`,`Adj. OE 2023`, `Adj. DE 2023`))

transfer_data <- full_join(cbb_stats_2023, cbb_stats_2022, by = "id")

transfer_data$`Is Transfer?` <- "No"

transfer_data <- transfer_data %>% mutate(`Team 2022` = ifelse(is.na(`Team 2022`), '-', `Team 2022`))
transfer_data <- transfer_data %>% mutate(`Team 2023` = ifelse(is.na(`Team 2023`), '-', `Team 2023`))

for(x in seq_len(nrow(transfer_data))){
  if(transfer_data$`Team 2022`[x] != transfer_data$`Team 2023`[x] & (transfer_data$`Team 2022`[x] != "-" & transfer_data$`Team 2023`[x] != "-")){
    transfer_data$`Is Transfer?`[x] <- "Yes"
  }
}

transfer_data <- transfer_data %>% filter(`Is Transfer?` == "Yes")

transfer_data$`Transfer Rating` <- 1

transfer_data <- transfer_data %>%
  mutate(
    `Transfer Rating` = case_when(
      `PPG 2022` < 7 & `PPG 2022` >= 5 ~ 2,
      TRUE ~ `Transfer Rating`
    )
  )

transfer_data <- transfer_data %>%
  mutate(
    `Transfer Rating` = case_when(
      (`Conf 2022` %in% c("ACC", "BE", "B10", "B12", "SEC", "P12","MWC","A10", "WCC", "Amer") &
         `PPG 2022` < 10) |
        (`PPG 2022` < 10 & `PPG 2022` >= 7) ~ 3,
      TRUE ~ `Transfer Rating`
    )
  )

transfer_data <- transfer_data %>%
  mutate(
    `Transfer Rating` = case_when(
      (`Conf 2022` %in% c("ACC", "BE", "B10", "B12", "SEC", "P12", "MWC", "A10", "WCC", "Amer") &
         `PPG 2022` >= 5 & `PPG 2022` < 9 &
         `Adj. OE 2022` >= 90 &
         `Adj. DE 2022` <= 105)|
        (`PPG 2022` < 15 & `PPG 2022` >= 10) ~ 4,
      TRUE ~ `Transfer Rating`
    )
  )

transfer_data <- transfer_data %>%
  mutate(
    `Transfer Rating` = case_when(
      `Conf 2022` %in% c("ACC", "BE", "B10", "B12", "SEC", "P12", "MWC", "A10", "WCC", "Amer") &
        `PPG 2022` >= 9 &
        `Adj. OE 2022` >= 95 &
        `Adj. DE 2022` <= 100 |
        `PPG 2022` >= 15 ~ 5,
      TRUE ~ `Transfer Rating`
    )
  )

freshmen_rankings_2023[freshmen_rankings_2023 == "Miami (FL)"] <- "Miami FL"
freshmen_rankings_2023[freshmen_rankings_2023 == "Ohio State"] <- "Ohio St."
freshmen_rankings_2023[freshmen_rankings_2023 == "Florida State"] <- "Florida St."
freshmen_rankings_2023[freshmen_rankings_2023 == "St. John's (NY)"] <- "St. John's"
freshmen_rankings_2023[freshmen_rankings_2023 == "UNC"] <- "North Carolina"
freshmen_rankings_2023[freshmen_rankings_2023 == "Washington State"] <- "Washington St."
freshmen_rankings_2023[freshmen_rankings_2023 == "Arizona State"] <- "Arizona St."
freshmen_rankings_2023[freshmen_rankings_2023 == "Michigan State"] <- "Michigan St."
freshmen_rankings_2023[freshmen_rankings_2023 == "Ole Miss"] <- "Mississippi"
freshmen_rankings_2023[freshmen_rankings_2023 == "UConn"] <- "Connecticut"

team_avg_transfer_rating <- transfer_data %>%
  group_by(`Team 2023`) %>%
  summarize(Avg_Transfer_Rating = mean(`Transfer Rating`, na.rm = TRUE))

team_avg_freshman_rating <- freshmen_rankings_2023 %>%
  group_by(freshmen_rankings_2023$School.s.) %>%
  summarize(Avg_Freshman_Rating = mean(`RSCI`, na.rm = TRUE))

colnames(team_avg_freshman_rating)[1] ="Team 2023"

team_rankings <- full_join(team_avg_freshman_rating, team_avg_transfer_rating, by = "Team 2023")

team_rankings <- team_rankings %>% drop_na(Avg_Freshman_Rating)
team_rankings <- team_rankings %>% drop_na(`Team 2023`)

team_rankings[is.na(team_rankings)] <- 0

colnames(ken_pom_2022_23)[2] ="Team 2023"

team_rankings <- full_join(ken_pom_2022_23, team_rankings, by = "Team 2023")

team_rankings <- team_rankings %>% drop_na(Avg_Freshman_Rating)

team_rankings$Avg_Freshman_Rating = (team_rankings$Avg_Freshman_Rating -100)*-1

filtered_transfer_data <- subset(team_rankings, Avg_Transfer_Rating != 0)
transfer_model <- lm(Avg_Transfer_Rating ~ AdjEM.x, data = filtered_transfer_data)
plot(Avg_Transfer_Rating ~ AdjEM.x, data=filtered_transfer_data)
abline(transfer_model, col = "red")

freshman_model <- lm(Avg_Freshman_Rating ~ AdjEM.x, data = team_rankings)
plot(Avg_Freshman_Rating ~ AdjEM.x, data = team_rankings)
abline(freshman_model, col = "red")
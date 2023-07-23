library(nflreadr)
library(tidyverse)

#### Expected Starting Field Position ####

# We're going to use play by play data dating back to 2015

pbp2015_2022 <- nflreadr::load_pbp(2015:2022)

# Filtering out timeouts,cblocked punts and safeties. Timeouts cause issues with lead when trying to grab end
# yard line data

pbp2015_2022 <- pbp2015_2022 %>% filter(timeout == 0,
                                        punt_blocked == 0,
                                        safety == 0)

# We need to modify the kick_distance column to not be null on touchbacks since we'll need it later

pbp2015_2022 <- pbp2015_2022 %>% mutate(kick_distance = ifelse(play_type == 'punt' & touchback == 1 & is.na(kick_distance),
                                                               yardline_100, kick_distance))

# We're going to create an end yard line column based on the starting field position of the offense
# on the play following the punt

pbp2015_2022 <- pbp2015_2022 %>% mutate(end_yardline_100 = 100 - (yardline_100 - kick_distance + return_yards))

# Setting touchbacks

pbp2015_2022 <- pbp2015_2022 %>% mutate(end_yardline_100 = ifelse(touchback == 1 & play_type == 'punt',
                                                                  80,
                                                                  end_yardline_100))

# We're going to keep return TDs and long returns in, but only assign up to 7 yards of the return yardage to the punter

pbp2015_2022 <- pbp2015_2022 %>% mutate(end_yardline_100 = ifelse(play_type == 'punt' & return_yards > 7,
                                                                  100 - (yardline_100 - kick_distance + 7),
                                                                  end_yardline_100))

# Removing any punts that are muffed and recovered by the kicking team

pbp2015_2022 <- pbp2015_2022 %>%
  filter(
    is.na(fumble_recovery_1_team) | fumble_recovery_1_team != posteam,
    is.na(fumble_recovery_2_team) | fumble_recovery_2_team != posteam
  )

# Filtering to just punts

punts_2015_2022 <- pbp2015_2022 %>% filter(play_type == 'punt')

# Building the model

model <- lm(end_yardline_100 ~ yardline_100 + score_differential + qtr + quarter_seconds_remaining, data = punts_2015_2022)

predictions <- predict(model, punts_2015_2022)

# Setting predicted yard line to cap out at 99

predictions <- ifelse(predictions > 99, 99, predictions)

# Making the prediction into a column in our dataframe

punts_2015_2022$predicted_starting_yardline <- round(predictions, 0)

# Using predicted starting yard line to compare with actual starting yard line

punts_2015_2022 <- punts_2015_2022 %>% mutate(FPOE = end_yardline_100 - predicted_starting_yardline)

# Creating ranks for 2022

Ranks_2022 <- punts_2015_2022 %>% filter(season == 2022,
                                         season_type == 'REG') %>%
  group_by(punter_player_name, posteam) %>%
  summarise(total_punts = n(),
            total_FPOE = sum(FPOE, na.rm = T),
            FPOE_per_punt = total_FPOE/total_punts) %>%
  filter(total_punts >= 30) %>%
  rename(team_abbr = posteam)

teams_colors_logos <- nflfastR::teams_colors_logos

Ranks_2022 <- left_join(Ranks_2022, teams_colors_logos)

ggranks_2022 <- ggplot(data = ranks <- Ranks_2022,
                       aes(y = reorder(punter_player_name, FPOE_per_punt), x = FPOE_per_punt)) +
  geom_col(color = ranks$team_color2,
           fill = ranks$team_color,
           width = 0.8) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team_abbr), width = 0.035) +
  labs(title = "2022 Field Position Over Expected (FPOE) Per Punt Leaders",
       subtitle = paste("Min.", 30, "Punt Attempts"),
       caption = "Chart by @Mike_Lounsberry, data from nflreadr",
       x = "FPOE/Punt",
       y = "")

#### Year to Year Consistency ####

FPOE_Rankings_2015_2022 <- punts_2015_2022 %>%
  filter(season_type == 'REG') %>% 
  group_by(punter_player_name, posteam, season) %>%
  summarise(total_punts = n(),
            total_FPOE = sum(FPOE, na.rm = T),
            FPOE_per_punt = total_FPOE/total_punts) %>%
  filter(total_punts >= 30) %>%
  rename(team_abbr = posteam)

YearToYear <- FPOE_Rankings_2015_2022 %>% 
  arrange(punter_player_name, desc(season)) %>%
  group_by(punter_player_name) %>%
  summarise(Y1 = lead(FPOE_per_punt), Y2 = FPOE_per_punt, .groups = 'drop') %>%
  na.omit

eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

YearToYear %>% ggplot(aes(x = Y1, y = Y2)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE) +
  geom_text(x = -2.5, y = 3, label = eq(YearToYear$Y1,YearToYear$Y2), parse = TRUE) +
  labs(title = "Year To Year Comparison of FPOE/Punt",
       subtitle = "2015-2022",
       caption = "Chart by @Mike_Lounsberry, data from nflreadr") 





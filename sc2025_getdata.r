library(tidyverse)
library(baseballr)



################################# Collecting yearly data 
#Write statcast csv 
#write.csv(sc_2025, "/Users/markrecupero/Desktop/sc_data/sc_2025.csv")

library(sabRmetrics)
library(parallelly)

#availableWorkers()

#cluster = parallelly::makeClusterPSOCK(8)


#sc_2025 = sabRmetrics::download_baseballsavant(
#  start_date = "2025-03-27",
  #end_date = "2025-09-28",
 # cl = cluster
#)
#sc_2025 = bind_rows(sc_2025)



#write.csv(sc_2025, "~/Desktop/sc_data/sc_2025.csv")


#Load all the datasets in the sc_data folder in desktop 
path = "/Users/markrecupero/Desktop/sc_data"

sc_files = list.files(path, pattern = "^sc_\\d{4}\\.csv", full.names = T)


for(f in sc_files){
  name = gsub("\\.csv$", "",basename(f))
  
  df = read.csv(f)
  
  assign(name, df, envir = .GlobalEnv)
}


sc_2015 = read_csv("~/Desktop/sc_data/sc_2015.csv")
sc_2016 = read_csv("~/Desktop/sc_data/sc_2016.csv")
sc_2017 = read_csv("~/Desktop/sc_data/sc_2017.csv")
sc_2018 = read_csv("~/Desktop/sc_data/sc_2018.csv")
sc_2019 = read_csv("~/Desktop/sc_data/sc_2019.csv")
sc_2020 = read_csv("~/Desktop/sc_data/sc_2020.csv")
sc_2021 = read_csv("~/Desktop/sc_data/sc_2021.csv")
sc_2022 = read_csv("~/Desktop/sc_data/sc_2022.csv")
sc_2023 = read_csv("~/Desktop/sc_data/sc_2023.csv")
sc_2024 = read_csv("~/Desktop/sc_data/sc_2024.csv")
sc_2025 = read_csv("~/Desktop/sc_data/sc_2025.csv")


#######################Mutate data function

#Find all the player names
people = chadwick_player_lu() %>% 
  select(key_mlbam, name_first, name_last) %>% 
  mutate(full_name = paste0(name_last, ", ", name_first)) %>% 
  rename("pitcher" = "key_mlbam")


#mutate function
mutated_sc_data.2025 = function(sc_df){
  sc_df = sc_df %>% 
    filter(game_date <= "2025-09-28") %>% 
    mutate(
    team_batting = ifelse(inning_topbot == "Bot",
                          home_team,
                          away_team),
    team_pitching = ifelse(inning_topbot == "Bot",
                           away_team,
                           home_team),
    inning_bins = cut(
      inning,
      breaks = c(0, 3, 6, 9, Inf),
      labels = c("1–3", "4–6", "7–9", "Extras"),
      include.lowest = TRUE,
      right = TRUE
    ),
    on_base = ifelse(
      (!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b)),
      "runners on",
      "no runners on"
    ),
    risp = ifelse(
      (!is.na(on_2b) | !is.na(on_3b)),
      "RISP",
      "no RISP"
    ),
    bat_spray_angle = atan((hc_x - 125.42)/
                             (198.27 - hc_y)),
    bat_spray_angle = ifelse(
      stand == "L", -bat_spray_angle, bat_spray_angle
    ),
    oppo_pull = ifelse(
      bat_spray_angle < 0, "pull", "oppo"
    ),
    spray_angle_radians = atan2((hc_x - 125.42), (198.27 - hc_y)) * 180 / pi,
    spray_bucket = case_when(
      stand == "R" & spray_angle_radians < -15 ~ "pull",
      stand == "R" & spray_angle_radians > 15  ~ "oppo",
      stand == "R" ~ "straight",
      
      stand == "L" & spray_angle_radians > 15  ~ "pull",
      stand == "L" & spray_angle_radians < -15 ~ "oppo",
      stand == "L" ~ "straight"
    ),
    hardhit = ifelse(
      launch_speed >= 95, "hardhit", "not hardhit"
    ),
    sweetspot = ifelse(
      (launch_angle >= 8 | launch_angle <=32),
      "sweetspot",
      "not sweetspot"
    ),
    count = case_when(
      (balls == 0 & strikes == 0) ~ "First Pitch",
      ((balls == 0 & strikes == 1) |
         (balls == 0 & strikes == 2) |
         (balls == 1 & strikes == 2)) ~ "Pitchers Count",
      ((balls == 1 & strikes == 1) |
         (balls == 2 & strikes == 2)) ~ "Even",
      ((balls == 1 & strikes == 0) |
         (balls == 2 & strikes == 0) |
         (balls == 2 & strikes == 1) |
         (balls == 3 & strikes == 0) |
         (balls == 3 & strikes == 1)) ~ "Batters Count",
      (balls == 3 & strikes == 2) ~ "Full Count"
    ),
    swing_or_noswing = ifelse(description %in% c("foul_tip", "foul", "swinging_strike",
                                                 "swinging_strike_blocked", "hit_into_play"),
                              "swing", "no_swing"),
    whiff_or_contact = case_when(
      description %in% c("foul_tip", "swinging_strike_blocked",
                         "swinging_strike") ~ "whiff",
      description %in% c("foul", "hit_into_play") ~ "contact"),
    attack_zone_heart = ifelse(
      ((plate_x <= 6.7/12) & 
         (plate_x >= -6.7/12)) &
        ((plate_z <= sz_top - 1/6*(sz_top - sz_bot)) & 
           (plate_z >= sz_bot + 1/6*(sz_top - sz_bot))),
      "heart",
      "no_heart"),
    attack_zone_shadow = ifelse(
      attack_zone_heart != "heart" &
        ((plate_x >= -13.3/12 & plate_x <= 13.3/12) &
           (plate_z >= (sz_bot - 1/6*(sz_top - sz_bot)) & 
              plate_z <= (sz_top + 1/6*(sz_top - sz_bot)))),
      "shadow",
      "no_shadow"
    ),
    attack_zone_chase = ifelse(
      ((attack_zone_shadow != "shadow") &
         (attack_zone_heart != "heart") &
         (plate_z >= sz_bot - 1/2*(sz_top - sz_bot) &
            (plate_z <= sz_top + 1/2*(sz_top - sz_bot))&
            (plate_x >= -20 & plate_x <= 20))),
      "chase", "no_chase"
    )) %>% 
  inner_join(people,
             by = "pitcher") %>% 
  rename(c("batter_name" = "player_name",
           "pitcher_name" = "full_name"))
  
  return(sc_df)

}  




mutated_sc_data = function(sc_df){
    sc_df1 = sc_df %>% mutate(
      team_batting = ifelse(inning_topbot == "Bot",
                            home_team,
                            away_team),
      team_pitching = ifelse(inning_topbot == "Bot",
                             away_team,
                             home_team),
      inning_bins = cut(
        inning,
        breaks = c(0, 3, 6, 9, Inf),
        labels = c("1–3", "4–6", "7–9", "Extras"),
        include.lowest = TRUE,
        right = TRUE
      ),
      on_base = ifelse(
        (!is.na(pre_runner_1b_id) | !is.na(pre_runner_2b_id) | 
           !is.na(pre_runner_3b_id)),
        "runners on",
        "no runners on"
      ),
      risp = ifelse(
        (!is.na(pre_runner_2b_id) | !is.na(pre_runner_3b_id)),
        "RISP",
        "no RISP"
      ),
      bat_spray_angle = atan((hit_coord_x - 125.42)/
                               (198.27 - hit_coord_y)),
      bat_spray_angle = ifelse(
        bat_side == "L", -bat_spray_angle, bat_spray_angle
      ),
      oppo_pull = ifelse(
        bat_spray_angle < 0, "pull", "oppo"
      ),
      spray_angle_radians = atan2((hit_coord_x - 125.42), (198.27 - hit_coord_y)) * 180 / pi,
      spray_bucket = case_when(
        bat_side == "R" & spray_angle_radians < -15 ~ "pull",
        bat_side == "R" & spray_angle_radians > 15  ~ "oppo",
        bat_side == "R" ~ "straight",
        
        bat_side == "L" & spray_angle_radians > 15 ~ "pull",
        bat_side == "L" & spray_angle_radians < -15 ~ "oppo",
        bat_side == "L" ~ "straight"
      ),
      hardhit = ifelse(
        launch_speed >= 95, "hardhit", "not hardhit"
      ),
      sweetspot = ifelse(
        (launch_angle >= 8 | launch_angle <=32),
        "sweetspot",
        "not sweetspot"
      ),
      count = case_when(
        (balls == 0 & strikes == 0) ~ "First Pitch",
        ((balls == 0 & strikes == 1) |
           (balls == 0 & strikes == 2) |
           (balls == 1 & strikes == 2)) ~ "Pitchers Count",
        ((balls == 1 & strikes == 1) |
           (balls == 2 & strikes == 2)) ~ "Even",
        ((balls == 1 & strikes == 0) |
           (balls == 2 & strikes == 0) |
           (balls == 2 & strikes == 1) |
           (balls == 3 & strikes == 0) |
           (balls == 3 & strikes == 1)) ~ "Batters Count",
        (balls == 3 & strikes == 2) ~ "Full Count"
      ),
      swing_or_noswing = ifelse(description %in% c("foul_tip", "foul", "swinging_strike",
                                                   "swinging_strike_blocked", "hit_into_play"),
                                "swing", "no_swing"),
      whiff_or_contact = case_when(
        description %in% c("foul_tip", "swinging_strike_blocked",
                           "swinging_strike") ~ "whiff",
        description %in% c("foul", "hit_into_play") ~ "contact"),
      attack_zone_heart = ifelse(
        ((plate_x <= 6.7/12) & 
           (plate_x >= -6.7/12)) &
          ((plate_z <= strike_zone_top - 1/6*(strike_zone_top - strike_zone_bottom)) & 
             (plate_z >= strike_zone_bottom + 1/6*(strike_zone_top - strike_zone_bottom))),
        "heart",
        "no_heart"),
      attack_zone_shadow = ifelse(
        attack_zone_heart != "heart" &
          ((plate_x >= -13.3/12 & plate_x <= 13.3/12) &
             (plate_z >= (strike_zone_bottom - 1/6*(strike_zone_top - strike_zone_bottom)) & 
                plate_z <= (strike_zone_top + 1/6*(strike_zone_top - strike_zone_bottom)))),
        "shadow",
        "no_shadow"
      ),
      attack_zone_chase = ifelse(
        ((attack_zone_shadow != "shadow") &
           (attack_zone_heart != "heart") &
           (plate_z >= strike_zone_bottom - 1/2*(strike_zone_top - strike_zone_bottom) &
              (plate_z <= strike_zone_top + 1/2*(strike_zone_top - strike_zone_bottom))&
              (plate_x >= -20 & plate_x <= 20))),
        "chase", "no_chase"
      )) %>% 
    inner_join(people,
               by = c("pitcher_id" = "pitcher")) %>% 
    rename(c("pitcher_name" = "full_name",
             "batter" = "batter_id",
             "pitcher" = "pitcher_id",
             "p_throws" = "pitch_hand",
             "stand" = "bat_side",
             "on_1b" = "pre_runner_1b_id",
             "on_2b" = "pre_runner_2b_id",
             "on_3b" = "pre_runner_3b_id",
             "outs_when_up" = "outs",
             "hc_x" = "hit_coord_x",
             "hc_y" = "hit_coord_y",
             "sz_top" = "strike_zone_top",
             "sz_bot" = "strike_zone_bottom",
             "release_extension" = "extension",
             "fielder_2" = "fielder_2_id",
             "fielder_3" = "fielder_3_id",
             "fielder_4" = "fielder_4_id",
             "fielder_5" = "fielder_5_id",
             "fielder_6" = "fielder_6_id",
             "fielder_7" = "fielder_7_id",
             "fielder_8" = "fielder_8_id",
             "fielder_9" = "fielder_9_id",
             "estimated_woba_using_speedangle" = "expected_woba",
             "estimated_ba_using_speedangle" = "expected_babip"))
  
  return(sc_df1)
  
}  


mutated_sc_2015 = mutated_sc_data(sc_2015)
mutated_sc_2016 = mutated_sc_data(sc_2016)
mutated_sc_2017 = mutated_sc_data(sc_2017)
mutated_sc_2018 = mutated_sc_data(sc_2018)
mutated_sc_2019 = mutated_sc_data(sc_2019)
mutated_sc_2020 = mutated_sc_data(sc_2020)
mutated_sc_2021 = mutated_sc_data(sc_2021)
mutated_sc_2022 = mutated_sc_data(sc_2022)
mutated_sc_2023 = mutated_sc_data(sc_2023)
mutated_sc_2024 = mutated_sc_data(sc_2024)
mutated_sc_2025 = mutated_sc_data.2025(sc_2025)






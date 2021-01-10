library(tidyverse)
library(MASS)
library(rcompanion)

setwd("C:/Users/Vince/Documents/Vincent Work/Rutgers/Regression Time Series/project")

BaseballSavant2019 <- read_csv("2019_Baseball_Savant_Pitch_Data.csv")
Statcast2019 <- read_csv("statcast2019.csv")
str(Statcast2019)

Statcast2019 <- Statcast2019 %>%
  mutate(Swing =  ifelse(description %in%
                           c("foul", "foul_bunt", "foul_pitchout",
                             "foul_top", "hit_into_play",
                             "hit_into_play_no_out",
                             "hit_into_play_score", "missed_bunt",
                             "swinging_pitchout",
                             "swinging_strike",
                             "swinging_strike_blocked"), 1, 0),
         Miss = ifelse(description %in%
                         c("swinging_pitchout",
                           "swinging_strike",
                           "swinging_strike_blocked"), 1, 0)) %>%
  rename(pitcher_id = pitcher) %>%
  group_by(pitcher_id, pitch_type) %>% 
  summarize(TotalPitches = n(),
            TotalSwings = sum(Swing),
            TotalMisses = sum(Miss),
            AverageExitVelocity = mean(launch_speed, na.rm = TRUE),
            Barrels = sum(barrel, na.rm = TRUE),
            AverageLaunchAngle = mean(launch_angle, na.rm = TRUE),
            AverageHitDistance = mean(hit_distance_sc, na.rm = TRUE),
            AverageEstimatedBattingAverage = mean(estimated_ba_using_speedangle, na.rm = TRUE),
            AverageEstimatedWOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE)) %>%
  filter(TotalSwings > 10) %>% 
  mutate(SwingPercentage = TotalSwings/TotalPitches,
         MissPercentage = TotalMisses/TotalPitches, 
         BarrelPercentage = Barrels/TotalPitches)

total2019 <- merge(Statcast2019, BaseballSavant2019, by = c('pitcher_id','pitch_type'))

total2019_2 <- total2019[,-c(15,16,20, 22, 23, 26, 27)]


total2019_2$pitch_type <-case_when(
  total2019_2$pitch_type == "CH" ~ 1,
  total2019_2$pitch_type == "CU" ~ 2,
  total2019_2$pitch_type == "FC" ~ 3,
  total2019_2$pitch_type == "FF" ~ 4,
  total2019_2$pitch_type == "FS" ~ 5,
  total2019_2$pitch_type == "SI" ~ 6,
  total2019_2$pitch_type == "SL" ~ 7,
)

total2019_2$pitch_hand <- case_when(
  total2019_2$pitch_hand == "L" ~ 0,
  total2019_2$pitch_hand == "R" ~ 1
)



total2019_2 <- total2019_2 %>%
  mutate(barrelPerSwing = Barrels / TotalSwings)

total2019_3 <- total2019_2[,-c(1,22,23)]


full_model <- lm(pitch_type ~., data = total2019_3)
model_select_both_AIC <- step(full_model,scope=list(lower=~1,upper=full_model),direction="both",trace=FALSE, na.rm = T)
summary(model_select_both_AIC)

ggplot(data = model_select_both_AIC, aes(y = model_select_both_AIC$residuals, x = model_select_both_AIC$fitted.values)) +
  geom_point()

shapiro.test(model_select_both_AIC$residuals)

boxcox(model_select_both_AIC)

total2019_4_log <- log(na.omit(total2019_3))

count(total2019_3, NA)

full_model_log <- lm(pitch_type ~., data = total2019_4_log, na.action = na.omit)

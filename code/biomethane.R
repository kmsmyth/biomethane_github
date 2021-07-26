### Impacts of Sonication Treatment on Swine Manure Sludge Biomethane potential (BMP) and Biomethane Production Kinetics
### Authors: Sissy Clay and Kelsey Smyth
### Date Created: July 26, 2021
### Last Updated: July 26, 2021

## Clear workspace and load packages ----
rm(list = ls())
library(tidyverse)
library(dplyr)
library(janitor)
library(ggthemes)
library(extrafont)
library(minpack.lm)
## Load data ----
cumulative_pres <- read_csv("data/cum_pres.csv")
absolute_pres <- read_csv("data/abs_pres.csv")
temp <- read_csv("data/temp.csv")
set_up <- read_csv("data/set_up.csv")

## Tidy data ----

# Create vector for number of time increments (time 0 to last time - the minus 1 is because rows start at 1 but time starts at 0)
time_interval <- 0:(nrow(absolute_pres)-1)
# Find the number of time intervals in 1 day
k = 2 # Start at observation 2
while (cumulative_pres$time[k] != cumulative_pres$time[1]) { # continue while loop until time is equal to start time
  k = k+1 # add one observation each while loop cycle
}
# Create vector for day increments by dividing by number of observations in 1 day
day_interval <- time_interval/(k-1) # subtract 1 becuase while loop stops at first observation of second day

# Add time and day interval columns to data frames and tidy into long format
cumulative_pres <- cumulative_pres %>%
  mutate(time_interval = time_interval,
         day = day_interval) %>%
  pivot_longer(!c(time_interval , day , time),
               names_to = "bottle",
               values_to = "cumulative_pressure") %>%
  filter(bottle != 0)

absolute_pres <- absolute_pres %>%
  mutate(time_interval = time_interval,
         day = day_interval) %>%
  pivot_longer(!c(time_interval , day , time),
               names_to = "bottle",
               values_to = "absolute_pressure") %>%
  filter(bottle != 0)

temp <- temp %>%
  mutate(time_interval = time_interval,
         day = day_interval) %>%
  pivot_longer(!c(time_interval , day , time),
               names_to = "bottle",
               values_to = "temperature") %>%
  filter(bottle != 0)

# Combine into 1 data frame
combined <- absolute_pres %>%
  mutate(cumulative_pressure = cumulative_pres$cumulative_pressure,
         temperature = temp$temperature)

# Clean names of set up data
set_up <- clean_names(set_up)

# Add head space volume , treatment , and volatile solids from set_up data set
combined <- combined %>%
  mutate(treatment = rep(set_up$treatment , nrow(combined)/nrow(set_up)),
         headspace_volume = rep(set_up$headspace_volume_m_l , nrow(combined)/nrow(set_up)),
         vs_fed = rep(set_up$total_vs_fed_g , nrow(combined)/nrow(set_up)),
         vs_in = rep(set_up$vs_of_inoculum_g , nrow(combined)/nrow(set_up)),
         vs_s = rep(set_up$vs_of_substrate_g , nrow(combined)/nrow(set_up))) %>%
  arrange(bottle)


## 1.a. Quantify and plot cumulative volume of biogas gas per replicate ----

# Define constants and conversion factors
to_kpa = 6.894757293 # conversion factor from psi to kPa
p_stp <- 101.2 #kPa - standard atmospheric pressure
t_stp <- 273.15 #K - standard atmospheric temperature

# Drop NAs
combined <- drop_na(combined)

# Find delta P
# Create new, empty column in combine data set to store pressure difference
combined$delta_p <- rep(NA , nrow(combined))
# Run loop to calculate pressure difference
for (i in 2:nrow(combined)) {
  if (combined$bottle[i] == combined$bottle[i-1]) {
    combined$delta_p[i] = combined$cumulative_pressure[i]-combined$cumulative_pressure[i-1]
  } else {
    combined$delta_p[i] = combined$cumulative_pressure[i]
  }
}
# Input 0 psi into time stamp 1 for bottle 1 (as loop cannot calculate for place of [1-1=0])
combined$delta_p[1] = 0

# Calculate volume production at each time stamp
combined <- combined %>%
  mutate(v_stp = ((delta_p*to_kpa*headspace_volume*t_stp)/(p_stp*(temperature + 273.15))))

# Find cumulative biogas volume
# Create new, empty column in combine data set to store cumulative volume
combined$cumulative_vol <- rep(NA , nrow(combined))
# Input 0 mL into time stamp 1 for bottle 1 (as loop cannot calculate for place of [1-1=0])
combined$cumulative_vol[1] = 0
# Run loop to calculate cumulative volume
for (i in 2:nrow(combined)) {
  if (combined$bottle[i] == combined$bottle[i-1]) {
    combined$cumulative_vol[i] = combined$v_stp[i]+combined$cumulative_vol[i-1]
  } else {
    combined$cumulative_vol[i] = combined$v_stp[i]
  }
}

# Create data frame of average biogas production for inoculum bottles by gram of VS
inoculum_volume <- combined %>%
  filter(treatment == "Inoculum") %>%
  group_by(time_interval) %>%
  summarize(inoculum_volume = mean(cumulative_vol/vs_in)) %>%
  ungroup()

# Add average inoculum to combined data set by time_interval (automatically selected by left_join)
combined <- left_join(combined, inoculum_volume)

combined <- combined %>%
  # Add column for volume made by just the substrate
  mutate(volume_s = (cumulative_vol - (inoculum_volume*vs_in))/vs_s,
         # Add column that shows the biogas production from inoculum for inoculum and just substrate for all other treatments
         biogas_vol = if_else(treatment == "Inoculum" , cumulative_vol/vs_in , volume_s))

# Plot cumulative volume of biogas

# Change order of bottles for the legend
combined$bottle <- factor(combined$bottle , levels = order(unique(combined$bottle)))

# Create plot for cumulative biogas faceted by treatment
ggplot(data = combined) +
  geom_point(aes(x = day, 
                 y = biogas_vol,
                 color = bottle),
             size = 0.1)+
  labs(x = "Day",
       y = "Biogas Produced (mL/gram of Volative Solid)",
       title = "Cumulative Volume of Biogas Produced",
       color = "Replicate") +
  scale_color_manual(values = c("burlywood4" , "darkorange2" , "darkgoldenrod1", "springgreen2" , "chartreuse4" , "darkgreen" , "skyblue" , "skyblue4" , "slateblue4" ,  "brown1" , "brown4" ,"hotpink3")) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_igray() +
  facet_wrap(~treatment , scales = "free_y") +
  theme(plot.title = element_text(face = "bold", size = 16, family = "Calibri"),
        legend.background = element_rect(fill = "white", size = 1, colour = "snow4"),
        legend.title = element_text(size = 12),
        panel.grid.major = element_line(colour = "snow4", size = .5),
        panel.grid.minor = element_line(colour = "snow4", size = .5),
        text = element_text(size = 14, family = "Calibri", colour = "gray10"))+
  guides(color = guide_legend(override.aes = list(size = 2))) +
  facet_wrap(~treatment , scales = "free_y")
# Save cumulative biogas plot
ggsave("figures/cumulative_biogas.png" , height = 8 , width = 10 , units = "in")

# Create data frame that shows only the final volume of biogas production for all treatments
final_volume_biogas <- combined %>%
  select(time_interval , bottle , treatment , biogas_vol) %>%
  filter(time_interval == max(time_interval))

## 1.b. Quantify and plot daily gas production per replicate ----

# Create column for day of treatment
combined$actual_day = floor(combined$day) + 1

# Find delta biogas production
# Create new, empty column in combine data set to store biogas difference
combined$delta_biogas <- rep(NA , nrow(combined))
# Run loop to calculate pressure difference
for (i in 2:nrow(combined)) {
  if (combined$bottle[i] == combined$bottle[i-1]) {
    combined$delta_biogas[i] = combined$biogas_vol[i]-combined$biogas_vol[i-1]
  } else {
    combined$delta_biogas[i] = combined$biogas_vol[i]
  }
}
# Input 0 mL/g VS into time stamp 1 for bottle 1 (as loop cannot calculate for place of [1-1=0])
combined$delta_biogas[1] = 0

# Create a new data frame for daily biogas production
daily_biogas <- combined %>%
  select(actual_day , bottle , treatment , delta_biogas , vs_in , vs_s , vs_fed) %>%
  group_by(actual_day , bottle , treatment , vs_in , vs_s , vs_fed) %>%
  summarize(daily_biogas = sum(delta_biogas)) %>%
  ungroup() %>%
  arrange(bottle)

# Determine maximum daily gas production for each bottle
max_daily_biogas <- daily_biogas %>%
  group_by(bottle , treatment) %>%
  summarize(maximum_pro = max(daily_biogas)) %>%
  ungroup()

# Plot the daily biogas production faceted by treatment
ggplot(data = daily_biogas,
       aes(x = actual_day,
           y = daily_biogas)) +
  geom_line(aes(color = bottle)) +
  geom_point(aes(color = bottle),
             size = 1.25) +
  scale_color_manual(values = c("burlywood4" , "darkorange2" , "darkgoldenrod1", "springgreen2" , "chartreuse4" , "darkgreen" , "skyblue" , "skyblue4" , "slateblue4" ,  "brown1" , "brown4" ,"hotpink3")) +
  theme_igray() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 16, family = "Calibri"),
        legend.background = element_rect(fill = "white", size = 1, colour = "snow4"),
        legend.title = element_text(size = 12),
        panel.grid.major = element_line(colour = "snow4", size = .5),
        panel.grid.minor = element_line(colour = "snow4", size = .5),
        text = element_text(size = 14, family = "Calibri", colour = "gray10"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  labs(title = "Daily Biogas Production",
       x = "Day",
       y = "Biogas Produced (mL/gram of Volative Solids)",
       color = "Replicate") +
  facet_wrap(~treatment , scales = "free_y")
# Save daily biogas plot
ggsave("figures/daily_biogas.png" , height = 8 , width = 10 , units = "in")

## Repeat 1.a and 1.b with methane production ----

# Read in data
ch4_v_frac <- read_csv("data/volume_comp_methane.csv") %>%
  clean_names() %>%
  na.omit() %>%
  pivot_wider(names_from = treatment,
              values_from = volume_fraction_of_methane) %>%
  clean_names() %>%
  arrange(day)

# Interpolate to find daily methane composition
# Raw sludge
is.function(fc_i_rs <- approxfun(ch4_v_frac$day , ch4_v_frac$i_rs , method = "linear"))
i_rs_inter <- c(fc_i_rs(1:max(ch4_v_frac$day)) , rep(ch4_v_frac$i_rs[length(ch4_v_frac$i_rs)] , times = length(1:(max(unique(combined$actual_day))-max(ch4_v_frac$day)))))

# Sonicated sludge
is.function(fc_i_ss <- approxfun(ch4_v_frac$day , ch4_v_frac$i_ss , method = "linear"))
i_ss_inter <- c(fc_i_ss(1:max(ch4_v_frac$day)) , rep(ch4_v_frac$i_ss[length(ch4_v_frac$i_ss)] , times = length(1:(max(unique(combined$actual_day))-max(ch4_v_frac$day)))))

# Microcrystalline cellulose
is.function(fc_i_mc <- approxfun(ch4_v_frac$day , ch4_v_frac$i_mc , method = "linear"))
i_mc_inter <- c(fc_i_mc(1:max(ch4_v_frac$day)) , rep(ch4_v_frac$i_mc[length(ch4_v_frac$i_mc)] , times = length(1:(max(unique(combined$actual_day))-max(ch4_v_frac$day)))))

# Inoculum
is.function(fc_ino <- approxfun(ch4_v_frac$day , ch4_v_frac$inoculum , method = "linear"))
ino_inter <- c(fc_ino(1:max(ch4_v_frac$day)) , rep(ch4_v_frac$inoculum[length(ch4_v_frac$inoculum)] , times = length(1:(max(unique(combined$actual_day))-max(ch4_v_frac$day)))))

# Create vector with all interpolations
all_methane_v_frac <- c(rep(ino_inter , times = length(unique(combined$bottle))/length(unique(combined$treatment))) , rep(i_rs_inter , times = length(unique(combined$bottle))/length(unique(combined$treatment))) , rep(i_ss_inter , times = length(unique(combined$bottle))/length(unique(combined$treatment))) , rep(i_mc_inter , times = length(unique(combined$bottle))/length(unique(combined$treatment))))

# Multiply daily biogas production by volume fraction to get daily methane production
methane <- daily_biogas %>%
  mutate(methane_v_frac = all_methane_v_frac, # Add column of volume fractions
         daily_methane = daily_biogas * methane_v_frac)

# Replace negative daily production with daily production of 0
for (n in 1:nrow(methane)) {
  if (methane$daily_methane[n] < 0) {
    methane$daily_methane[n] = 0
  } else {
    methane$daily_methane[n] = methane$daily_methane[n]
  }
}

# Maximum daily methane gas production for each bottle
max_daily_methane <- methane %>%
  group_by(bottle , treatment) %>%
  summarize(maximum_methane = max(daily_methane)) %>%
  ungroup()

# Find cumulative methane production
# Create empty column to store cumulative values
methane$cumu_methane <- rep(NA , nrow(methane))
# Create for loop to calculate cumulative methane production
for (k in 1:nrow(methane)) {
  if (k-1 == 0){
    methane$cumu_methane[k] = methane$daily_methane[k]
  } else {
    if (methane$bottle[k] == methane$bottle[k-1]) {
      methane$cumu_methane[k] = methane$daily_methane[k] + methane$cumu_methane[k-1]
    } else {
      methane$cumu_methane[k] = methane$daily_methane[k]
    }
  }
}

# Create data frame that shows only the final volume of methane production for all treatments
final_volume_methane <- methane %>%
  select(actual_day , bottle , treatment , cumu_methane) %>%
  filter(actual_day == max(actual_day))

# Create plot of cumulative methane production
cumulative_plot <- ggplot(data = methane) +
  geom_point(aes(x = actual_day, 
                 y = cumu_methane,
                 color = bottle),
             size = 2)+
  labs(x = "Day",
       y = "Methane Produced (mL/gram of Volative Solid)",
       title = "Cumulative Volume of Methane Gas Produced",
       color = "Replicate") +
  scale_color_manual(values = c("burlywood4" , "darkorange2" , "darkgoldenrod1", "springgreen2" , "chartreuse4" , "darkgreen" , "skyblue" , "skyblue4" , "slateblue4" ,  "brown1" , "brown4" ,"hotpink3")) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_igray() +
  facet_wrap(~treatment , scales = "free_y") +
  theme(plot.title = element_text(face = "bold", size = 16, family = "Calibri"),
        legend.background = element_rect(fill = "white", size = 1, colour = "snow4"),
        legend.title = element_text(size = 15),
        panel.grid.major = element_line(colour = "snow4", size = .5),
        panel.grid.minor = element_line(colour = "snow4", size = .5),
        text = element_text(size = 12, family = "Calibri", colour = "gray10"))+
  guides(color = guide_legend(override.aes = list(size = 2))) +
  facet_wrap(~treatment , scales = "free_y")
cumulative_plot
# Save daily methane production plot
ggsave("figures/cumulative_methane.png" , width = 10 , height = 8 , units = "in")

# Create plot of daily methane production
ggplot(data = methane,
       aes(x = actual_day,
           y = daily_methane)) +
  geom_line(aes(color = bottle)) +
  geom_point(aes(color = bottle),
             size = 1.25) +
  scale_color_manual(values = c("burlywood4" , "darkorange2" , "darkgoldenrod1", "springgreen2" , "chartreuse4" , "darkgreen" , "skyblue" , "skyblue4" , "slateblue4" ,  "brown1" , "brown4" ,"hotpink3")) +
  theme_igray() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 16, family = "Calibri"),
        legend.background = element_rect(fill = "white", size = 1, colour = "snow4"),
        legend.title = element_text(size = 12),
        panel.grid.major = element_line(colour = "snow4", size = .5),
        panel.grid.minor = element_line(colour = "snow4", size = .5),
        text = element_text(size = 14, family = "Calibri", colour = "gray10"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  labs(title = "Daily Methane Gas Production",
       x = "Day",
       y = "Methane Produced (mL/gram of Volative Solids)",
       color = "Replicate") +
  facet_wrap(~treatment , scales = "free_y")
# Save daily methane production plot
ggsave("figures/daily_methane.png" , width = 10 , height = 8 , units = "in")

# Export csv file for methane production
write_csv(methane , file = "data/methane.csv")

## 1.c. Use statistical methods to assess whether the sonication treatment had a statistically significant effect on (i) the BMP, and (ii) the daily biogas production ----

# H_0 : mu_SS == mu_RS
# H_a : mu_SS != mu_RS

# t test for final cumulative biogas production
# mean of final cumulative biogas volume from substrate for raw sludge
mean_bmp_rs <- final_volume_biogas %>%
  filter(treatment == "I+RS (ISR: 2)") %>%
  summarise(mean_bmp = mean(biogas_vol)) %>%
  deframe()
# data frame for only final cumulative biogas volume from substrate for sonicated sludge
bmp_ss = final_volume_biogas %>%
  filter(treatment == "I+SS")
         #bottle != 8) # this is included in original code due to data collection errors and is not needed
# t test for final cumulative biogas volume from substrate
p_val_cb <- t.test(bmp_ss$biogas_vol,
                   mu = mean_bmp_rs,
                   conf.level = 0.95)$p.value

# t test for maximum daily biogas production 
# mean of maximum daily biogas production from substrate for raw sludge
mean_bmp_rs_d <- max_daily_biogas %>%
  filter(treatment == "I+RS (ISR: 2)") %>%
  summarise(mean_bmp = mean(maximum_pro)) %>%
  deframe()
# data frame for maximum daily biogas production from substrate for sonicated sludge
bmp_ss_d = max_daily_biogas %>%
  filter(treatment == "I+SS")
         #bottle != 8) # this is included in original code due to data collection errors and is not needed
# t test for maximum daily biogas production from substrate
p_val_db <- t.test(bmp_ss_d$maximum_pro,
                   mu = mean_bmp_rs_d,
                   conf.level = 0.95)$p.value

# t test for final cumulative methane production
# mean of final cumulative methane volume for raw sludge
mean_bmp_rs_m <- final_volume_methane %>%
  filter(treatment == "I+RS (ISR: 2)") %>%
  summarise(mean_bmp = mean(cumu_methane)) %>%
  deframe()
# data frame for final cumulative methane volume for sonicated sludge
bmp_ss_m = final_volume_methane %>%
  filter(treatment == "I+SS")
         #bottle != 8) # this is included in original code due to data collection errors and is not needed
# t test for final cumulative methane volume 
p_val_cm <- t.test(bmp_ss_m$cumu_methane,
                   mu = mean_bmp_rs_m,
                   conf.level = 0.95)$p.value

# t test for maximum daily methane production
# mean of maximum daily production of methane for raw sludge
mean_bmp_rs_dm <- max_daily_methane %>%
  filter(treatment == "I+RS (ISR: 2)") %>%
  summarise(mean_bmp = mean(maximum_methane)) %>%
  deframe()
# data frame for maximum daily production of methane for sonicated sludge
bmp_ss_dm = max_daily_methane %>%
  filter(treatment == "I+SS")
         #bottle != 8) # this is included in original code due to data collection errors and is not needed
# t test for maximum daily methane production
p_val_dm <- t.test(bmp_ss_dm$maximum_methane,
                   mu = mean_bmp_rs_dm,
                   conf.level = 0.95)$p.value

## Create plots to graphically show p-values and production significance

# Create a tibble with final volumes of biogas and methane
final_volume <- left_join(final_volume_biogas , final_volume_methane)

# Create a plot with average final volume of biogas and methane for raw sludge and sonicated sludge and add in p-value
final_volume %>%
  #filter(bottle != 8) %>% # this is included in original code due to data collection errors and is not needed
  select(treatment, biogas_vol , cumu_methane) %>%
  pivot_longer(!c(treatment),
               names_to = "type",
               values_to = "volume") %>%
  group_by(treatment , type) %>%
  summarize(avg_final_v = mean(volume)) %>%
  ungroup() %>%
  filter(treatment == "I+RS (ISR: 2)" | treatment == "I+SS") %>%
  ggplot() +
  geom_col(aes(x = type,
               y = avg_final_v,
               fill = treatment),
           position = "dodge") +
  geom_text(aes(x = 2,
                y = max(avg_final_v) + 1,
                label = paste("p =" , round(p_val_cm , 3))),
            size = 5) +
  geom_text(aes(x = 1,
                y = max(avg_final_v) + 1,
                label = paste("p =" , round(p_val_cb , 3))),
            size = 5) +
  labs(x = "",
       y = "Final Volume (mL/gVS)",
       fill = "Treatment") +
  scale_fill_manual(values = c("darkseagreen2" , "#0072b2")) +
  scale_x_discrete(labels = c("Biogas" , "Biomethane")) +
  theme_igray() +
  theme(legend.background = element_rect(fill = "white", size = 1, colour = "snow4"), 
        panel.grid.major = element_line(colour = "snow4", size = .5),
        panel.grid.minor = element_line(colour = "snow4", size = .5),
        text = element_text(size = 12, family = "Calibri", colour = "gray10"),
        legend.key.width = unit(0.25 , "cm"),
        legend.key.height = unit(0.5 , "cm"),
        axis.text = element_text(size = 13, family = "Calibri", colour = "gray10"),
        axis.title = element_text(size = 13, family = "Calibri", colour = "gray10"))
# Save final volume p-value plot
ggsave("figures/final_volume_significance.png" , height = 8 , width = 10 , units = "in")

# Create a tibble with maximum daily production values of biogas and methane
max_daily_pro <- left_join(max_daily_biogas , max_daily_methane)

# Create a plot with average maximum production values of biogas and methane of biogas and methane for raw sludge and sonicated sludge and add in p-value
max_daily_pro %>%
  #filter(bottle != 8) %>% # this is included in original code due to data collection errors and is not needed
  select(treatment, maximum_pro , maximum_methane) %>%
  pivot_longer(!c(treatment),
               names_to = "type",
               values_to = "volume") %>%
  group_by(treatment , type) %>%
  summarize(avg_max = mean(volume)) %>%
  ungroup() %>%
  filter(treatment == "I+RS (ISR: 2)" | treatment == "I+SS") %>%
  ggplot() +
  geom_col(aes(x = type,
               y = avg_max,
               fill = treatment),
           position = "dodge") +
  geom_text(aes(x = 2,
                y = max(avg_max) + 1,
                label = paste("p =" , round(p_val_dm , 3))),
            size = 5) +
  geom_text(aes(x = 1,
                y = max(avg_max) + 1,
                label = paste("p =" , round(p_val_db , 3))),
            size = 5) +
  labs(x = "",
       y = "Daily Production (mL/gVS)",
       fill = "Treatment") +
  scale_fill_manual(values = c("darkseagreen2" , "#0072b2")) +
  scale_x_discrete(limits = rev , labels = c("Biogas" , "Biomethane")) +
  theme_igray() +
  theme(legend.background = element_rect(fill = "white", size = 1, colour = "snow4"),
        panel.grid.major = element_line(colour = "snow4", size = .5),
        panel.grid.minor = element_line(colour = "snow4", size = .5),
        text = element_text(size = 11, family = "Calibri", colour = "gray10"),
        legend.key.width = unit(0.25 , "cm"),
        legend.key.height = unit(0.5 , "cm"),
        axis.text = element_text(size = 13, family = "Calibri", colour = "gray10"),
        axis.title = element_text(size = 13, family = "Calibri", colour = "gray10"))
# Save daily maximum production p-value plot
ggsave("figures/daily_production_significance.png" , height = 8 , width = 10 , units = "in")
## 1.d. Determine the suitable point to terminate each test (when sum of 3 consecutive days does not exceed 5% of total production) ----

# create empty vector to store dates
end_day <- c() 
# create empty vector to store volumes
end_volume <- c()
for(k in unique(methane$bottle)){
  bottle_frame <- methane %>%
    filter(bottle == k) # filter to only have 1 bottle 
  i = 2 # Starting point of day 1
  # collect last day and volume when sum of 3 consecutive days is not greater than 5% of cumulative gas produced
  while(sum(bottle_frame$daily_methane[i:(i+2)]) > 0.05*bottle_frame$cumu_methane[i-1]){
    end_day[k] = bottle_frame$actual_day[i+2]
    end_volume[k] = bottle_frame$cumu_methane[i+2]
    i = i + 1
  }
}
# Create vector for all bottles
bottle <- unique(methane$bottle)
treatment <- rep(unique(methane$treatment) , each = length(unique(methane$bottle))/length(unique(methane$treatment)))
# Combine end day and volume to a tibble
end_production <- tibble(bottle , treatment , end_day , end_volume)

# Create plot with end days highlighted
cumulative_plot +
  geom_point(data = end_production,
             aes(x = end_day,
                 y = end_volume,
                 color = bottle),
             size = 8,
             alpha = 0.5,
             shape = 18) +
  facet_wrap(~treatment , scales = "free_y") +
  labs(caption = "Note: The highlighted point signifies the suggested termination point for each replicate",
       title = "Cumulative Production of Methane with Termination Points") +
  theme(plot.caption = element_text(hjust = 0, face= "italic", size = 10),
        plot.title = element_text(face = "bold" , size = 16, family = "Calibri"),
        legend.background = element_rect(color = "snow4" , size = 1),
        axis.title = element_text(size = 12, family = "Calibri", colour = "gray10"),
        axis.text = element_text(size = 10, family = "Calibri", colour = "gray10"),
        strip.text = element_text(size = 12, family = "Calibri", colour = "gray10"),
        legend.text = element_text(size = 10 , family = "Calibru", color = "gray10"),
        legend.title = element_text(size = 12 , family = "Calibri" , color = "gray10"),
        legend.title.align = 0.5,
        legend.text.align = 0.5)
# Save termination plot
ggsave("figures/termination_point.png" , height = 8 , width = 10 , units = "in")

## 1.e. Determine what is the coefficient of variation among the replicates of the same treatment for cumulative gas production rate and daily gas production rate ----

# coefficient of variation = standard deviation / mean

# Cumulative Biogas
co_var_cb <- combined %>% 
  group_by(actual_day , treatment) %>% # select date_time and treatment
  summarize(mean_v = mean(biogas_vol , na.rm = T), # take mean
            sd_v = sd(biogas_vol , na.rm = T)) %>% # take standard deviation
  ungroup() %>%
  mutate(co_var = sd_v/mean_v) # calculate coefficient of variation

# Daily Biogas
co_var_db <- daily_biogas %>% 
  group_by(actual_day , treatment) %>% # select date_time and treatment
  summarize(mean_v = mean(daily_biogas , na.rm = T), # take mean
            sd_v = sd(daily_biogas , na.rm = T)) %>% # take standard deviation
  ungroup() %>%
  mutate(co_var_db = sd_v/mean_v) # calculate coefficient of variation

# Methane
co_var_methane <- methane %>% 
  group_by(actual_day , treatment) %>% # select date_time and treatment
  summarize(mean_v_d = mean(daily_methane , na.rm = T), # take mean
            sd_v_d = sd(daily_methane , na.rm = T),
            mean_v_c = mean(cumu_methane , na.rm = T),
            sd_v_c = sd(cumu_methane , na.rm = T)) %>% # take standard deviation
  ungroup() %>%
  mutate(co_var_dm = if_else(mean_v_d!=0 , sd_v_d/mean_v_d, 0),
         co_var_cm = sd_v_c/mean_v_c) # calculate coefficient of variation

# Put coefficient of variances in one data table
coeff_var <- tibble(co_var_cb$actual_day , co_var_cb$treatment , co_var_cb$co_var , co_var_db$co_var_db , co_var_methane$co_var_cm , co_var_methane$co_var_dm) %>%
  rename(day = "co_var_cb$actual_day", 
         treatment = "co_var_cb$treatment",
         cumulative_biogas = "co_var_cb$co_var",
         daily_biogas = "co_var_db$co_var_db",
         cumulative_methane = "co_var_methane$co_var_cm",
         daily_methane = "co_var_methane$co_var_dm")

# Determine the maximum, minimum, and mean coefficient of variation for each treatment
coeff_var_summarized <- coeff_var %>%
  pivot_longer(!c(day , treatment),
               names_to = "type",
               values_to = "coeff_variation") %>%
  group_by(treatment , type) %>%
  summarize(maximum = max(coeff_variation),
            average = mean(coeff_variation)) %>%
  ungroup()


## 2.a. Optimize kinetic model parameters ----

# Create a variable for how many replicates are for each treatment, assuming it is the same for all treatments
rep_per_treat <- length(unique(methane$bottle))/length(unique(methane$treatment))
# Create a vector that stores all treatments
treat <- unique(methane$treatment)
# Create vector where a number corresponds to each treatment
k_no <- 1:length(treat)
# Set value of constant
e = 2.7182
# Create vectors to store parameter values for each model, where l is logistic, g to gompertz, t to transfert, and f is first order
A_l <- rep(NA , length(treat))
mu_l <- rep(NA , length(treat))
lambda_l <- rep(NA , length(treat))
A_g <- rep(NA , length(treat))
mu_g <- rep(NA , length(treat))
lambda_g <- rep(NA , length(treat))
A_t <- rep(NA , length(treat))
mu_t <- rep(NA , length(treat))
lambda_t <- rep(NA , length(treat))
A_f <- rep(NA , length(treat))
k_f <- rep(NA , length(treat))
# Run a loop for all treatments and kinetic models
for(k in k_no){ # select for number corresponding to each treatment
  data <- methane %>%
    filter(treatment == treat[k], # filter to only include current treatment
           bottle == k*rep_per_treat) # remove a replicate of treatment for comparison later on (assuming all treatments have the same number of replicates, the number corresponding to treatment * number of replicates per treatment should select for and remove a bottle within the treatment)
  # Logistic model: y=A/({1+exp[(4µ)/A (λ-t)+2]})
  # optimize parameters using nonlinear regression from nlsLM function
  logistic <- nlsLM(cumu_methane~(A/((1+exp((4*mu)/A*(lambda-actual_day)+2)))), # input logistic function 
                    data = data, # select for filtered data frame
                    start = list(A = max(data$cumu_methane) , mu = max(data$daily_methane) , lambda = 5), # choose starting parameter values from data information and estimate for lambda
                    lower = rep(0,3)) # values should not be less than 0
  A_l[k] = summary(logistic)$coeff[1,1] # store A for each treatment
  mu_l[k] = summary(logistic)$coeff[2,1] # store mu for each treatment
  lambda_l[k] = summary(logistic)$coeff[3,1] # store lambda for each treatment
  
  # Gompertz model: y=A*exp{-exp[µe/A (λ-t)+1]}
  # optimize parameters using nonlinear regression from nlsLM function
  gompertz <- nlsLM(cumu_methane~(A*exp(-exp(mu*e/A*(lambda-actual_day)+1))), # input gompertz function 
                    data = data, # select for filtered data frame
                    start = list(A = max(data$cumu_methane) , mu = max(data$daily_methane) , lambda = 5),
                    lower = rep(0, 3)) # choose starting parameter values from data information and estimate for lambda
  A_g[k] = summary(gompertz)$coeff[1,1] # store A for each treatment
  mu_g[k] = summary(gompertz)$coeff[2,1] # store mu for each treatment
  lambda_g[k] = summary(gompertz)$coeff[3,1] # store lambda for each treatment
  
  # Transfert model: Y = A exp(-exp[1-{(µe/A)(λ-t)}])
  # optimize parameters using nonlinear regression from nlsLM function
  #transfert <- nlsLM(cumu_methane~(A*exp(-exp(1-(((-mu*e)/A)*(-lambda-actual_day))))), # input transfert function 
  transfert <- nlsLM(cumu_methane~(A*(1-exp(-(mu*(actual_day - lambda))/A))),
                     data = data, # select for filtered data frame
                     start = list(A = max(data$cumu_methane) , mu = max(data$daily_methane) , lambda = 5),
                     lower = c(0,0,0)) # choose starting parameter values from data information and estimate for lambda
  A_t[k] = summary(transfert)$coeff[1,1] # store A for each treatment
  mu_t[k] = summary(transfert)$coeff[2,1] # store mu for each treatment
  lambda_t[k] = summary(transfert)$coeff[3,1] # store lambda for each treatment
  
  # First order model: y= A (1- exp[-kt])
  # optimize parameters using nonlinear regression from nlsLM function
  first_order <- nlsLM(cumu_methane~(A*(1-exp(-k*actual_day))), # input first order function 
                       data = data, # select for filtered data frame
                       start = list(A = max(data$cumu_methane) , k = 1),
                       lower = rep(0, 2)) # choose starting parameter values from data information and estimate for lambda
  A_f[k] = summary(first_order)$coeff[1,1] # store A for each treatment
  k_f[k] = summary(first_order)$coeff[2,1] # store k for each treatment
}

# Create new data frame to store actual and predicted values of methane gas yield
models <- methane %>%
  select(actual_day , bottle , treatment , cumu_methane) %>%
  mutate(logistic = rep(NA , nrow(methane)),
         gompertz = rep(NA , nrow(methane)),
         transfert = rep(NA , nrow(methane)),
         first_order = rep(NA , nrow(methane)))

# Calculate estimated gas yield for each day for each treatment using all models
for (n in 1:length(treat)) {
  for (i in 1:nrow(models)) {
    if (models$treatment[i] == treat[n]) {
      models$logistic[i] = A_l[n]/((1+exp((4*mu_l[n])/A_l[n]*(lambda_l[n]-models$actual_day[i])+2)))
      models$gompertz[i] = A_g[n]*exp(-exp(mu_g[n]*e/A_g[n]*(lambda_g[n]-models$actual_day[i])+1))
      models$transfert[i] = A_t[n]*(1-exp(-(mu_t[n]*(models$actual_day[i] - lambda_t[n]))/A_t[n]))
      models$first_order[i] = A_f[n]*(1-exp(-k_f[n]*models$actual_day[i]))
    }
  }
}

# Pivot longer the models data frame so that it can be filtered by model
models <- models %>%
  pivot_longer(!c(actual_day , bottle , treatment , cumu_methane),
               names_to = "model",
               values_to = "est_yield")

# Plot points for each model and cumulative gas
ggplot(data = models) + 
  geom_line(aes(x = actual_day,
                y = est_yield,
                color = model),
            size = 1) +
  geom_point(aes(x = actual_day,
                 y = cumu_methane),
             size = 1) +
  facet_wrap(~bottle , scales = "free") +
  labs(x = "Day",
       y = "Methane Yield (mL per gram VS)",
       caption = "Note: Low correlation of replicate 8 may be due to errors in pressure readings during BMP test.",
       color = "Model") +
  scale_color_manual(values = c("cyan4" , "goldenrod2" , "darkseagreen2" , "#0072b2") , labels = c("First Order" , "Gompertz" , "Logistic" , "Transfert")) +
  theme_igray() +
  theme(plot.title = element_text(face = "bold", size = 16, family = "Calibri"),
        legend.background = element_rect(fill = "white", size = 1, colour = "snow4"),
        legend.title = element_text(size = 12),
        panel.grid.major = element_line(colour = "snow4", size = .5),
        panel.grid.minor = element_line(colour = "snow4", size = .5),
        text = element_text(size = 10, family = "Calibri", colour = "gray10"),
        plot.caption = element_text(hjust = 0, face= "italic"))
# Save kinetic models plot
ggsave("figures/kinetic_models.png" , height = 8 , width = 10 , units = "in")

# Save vectors of model names and colors
model_names <- unique(models$model)
model_colors <- c("darkseagreen3" , "goldenrod2" , "#0072b2" , "cyan4")
# Create plots with only one kinetic model per replicate
for (b in 1:length(model_names)) {
  models %>%
    filter(model == model_names[b]) %>%
    ggplot()+
    geom_point(aes(x = actual_day,
                   y = cumu_methane),
               size = 1) +
    geom_line(aes(x = actual_day,
                  y = est_yield),
              color = model_colors[b],
              size = 1) +
    facet_wrap(~bottle , scales = "free" , ncol = 3) +
    labs(x = "Day",
         y = "Methane Yield (mL/gVS)")+
    theme_igray() +
    theme(text = element_text(size = 8, family = "Calibri", colour = "gray10"),
          plot.caption = element_text(hjust = 0, face= "italic"),
          strip.background.x = element_rect(size = 0)) 
  ggsave(paste("figures/",model_names[b],"_fit.png"),
         height = 8,
         width = 10,
         unit = "in")
}

# Create graphs that plots the model values versus actual gas yield
ggplot(data = models) + 
  geom_point(aes(x = cumu_methane,
                 y = est_yield,
                 color = model)) +
  geom_line(aes(x = cumu_methane,
                y = cumu_methane),
            size = 0.25) +
  facet_wrap(~bottle , scales = "free") +
  labs(x = "Observed Methane Yield (mL per gram VS)",
       y = "Estimated Methane Yield (mL per gram VS)",
       #title = "Estimated vs. Observed Methane Yield by Replicate",
       caption = "Note: Black line represents acutal methane production.
Note: Low correlation of replicate 8 may be due to errors in pressure readings during BMP test.",
       color = "Model") +
  scale_color_manual(values = c("cyan4" , "goldenrod2" , "darkseagreen2" , "#0072b2") , labels = c("First Order" , "Gompertz" , "Logistic" , "Transfert")) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1) , n.breaks = 5) +
  theme_igray() +
  theme(plot.title = element_text(face = "bold", size = 16, family = "Calibri"),
        legend.background = element_rect(fill = "white", size = 1, colour = "snow4"),
        legend.title = element_text(size = 8),
        panel.grid.major = element_line(colour = "white", size = .5),
        panel.grid.minor = element_line(colour = "white", size = .5),
        text = element_text(size = 10, family = "Calibri", colour = "gray10"),
        plot.caption = element_text(hjust = 0, face= "italic"),
        strip.background.x = element_rect(size = 0),
        legend.text = element_text(size = 7))
# Save kinetic models linear plot
ggsave("figures/kinetic_models_linear.png" , height = 8 , width = 10 , units = "in")

## 2.b. Model of best fit for each treatment ----

# Create function to calculate RSME values
RMSE <- function(m , o) {
  sqrt(mean((m-o)^2))
}

# Create new data frame that summarizes models to find R^2, R^2 adjusted, and RMSE
r_stats <- models %>%
  group_by(bottle , treatment , model) %>%
  summarize(r_square = summary(lm(est_yield ~ cumu_methane))$r.squared,
            r_square_adj = summary(lm(est_yield ~ cumu_methane))$adj.r.squared,
            rmse = RMSE(est_yield , cumu_methane),
            rmse_by_mean = rmse/mean(est_yield)) %>%
  ungroup()

# Make a plot for the RMSE values
ggplot(data = r_stats) +
  geom_col(aes(x = bottle,
               y = rmse,
               fill = model),
           position = "dodge") +
  facet_wrap(~treatment , scales = "free") +
  labs(x = "Replicate",
       y = "RMSE (mL methane per gram VS)",
       fill = "Kinetic Model",
       title = "RMSE Values of Treatments and Kinetic Models",
       caption = "Note: Replicates 3, 6, 9, and 12 were used for parameter optimization.") +
  coord_flip() +
  scale_fill_manual(values = c("cyan4" , "goldenrod2" , "darkseagreen2" , "#0072b2") , labels = c("First Order" , "Gompertz" , "Logistic" , "Transfert")) +
  theme_igray() +
  theme(plot.title = element_text(face = "bold", size = 24, family = "Calibri"),
        legend.background = element_rect(fill = "white", size = 1, colour = "snow4"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        panel.grid.major = element_line(colour = "snow4", size = .5),
        panel.grid.minor = element_line(colour = "snow4", size = .5),
        text = element_text(size = 24, family = "Calibri", colour = "gray10"),
        plot.caption = element_text(hjust = 0, face= "italic")) 
# Save RMSE plot
ggsave("figures/rmse.png" , height = 8 , width = 10 , units = "in")

# Make a plot the mean RMSE values
ggplot(data = r_stats,
       aes(x = bottle,
           y = rmse_by_mean,
           fill = model)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(rmse_by_mean , 3)),
            position = position_dodge(0.9),
            hjust = -0.1,
            size = 6) +
  facet_wrap(~treatment , scales = "free_y") +
  expand_limits(y = 0.55) +
  labs(x = "Replicate",
       y = "RMSE",
       fill = "Kinetic Model",
       title = "Mean RMSE Values of Treatments and Kinetic Models",
       caption = "Note: Replicates 3, 6, 9, and 12 were used for parameter optimization.") +
  coord_flip() +
  scale_fill_manual(values = c("cyan4" , "goldenrod2" , "darkseagreen2" , "#0072b2") , labels = c("First Order" , "Gompertz" , "Logistic" , "Transfert")) +
  theme_igray() +
  theme(plot.title = element_text(face = "bold", size = 16, family = "Calibri"),
    legend.background = element_rect(fill = "white", size = 1, colour = "snow4"), 
    legend.title = element_text(size = 20),
    panel.grid.major = element_line(colour = "snow4", size = .5),
    panel.grid.minor = element_line(colour = "snow4", size = .5),
    text = element_text(size = 24, family = "Calibri", colour = "gray10"),
    plot.caption = element_text(hjust = 0, face= "italic" , size = 20),
    legend.text = element_text(size = 20),
    legend.key.size = unit(0.5 , "cm")) 
# Save mean RMSE plot
ggsave("figures/mean_rmse.png" , height = 8 , width = 10 , units = "in")

# Create tibble for R2 terms
r_square <- r_stats %>%
  select(bottle , treatment , model , r_square)
# Create vector for bottles
bottle = unique(r_square$bottle)
# Create R2 plot
ggplot(data = r_square) +
  geom_raster(aes(x = model,
                  y = bottle,
                  fill = r_square)) +
  geom_text(aes(x = model,
                y = bottle,
                label = signif(r_square , 4)),
            size = 6.5) +
  scale_fill_gradient2(low = "#eeb422" , high = "#0072b2" , midpoint = 0.95, mid = "#009e73") +
  labs(x = "",
       y = "",
       fill = "R-Squared",
       title = "R-Squared") +
  scale_y_discrete(labels = as.character(bottle), breaks = bottle) +
  scale_x_discrete(labels = c("First Order" , "Gompertz" , "Logistic" , "Transfert")) +
  theme_igray() +
  theme(plot.title = element_text(face = "bold", size = 24, family = "Calibri"),
        legend.background = element_rect(fill = "white", size = 1, colour = "black"), #color = "snow4"
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        panel.grid.major = element_line(colour = "white", size = .5),
        panel.grid.minor = element_line(colour = "white", size = .5),
        text = element_text(size = 24, family = "Calibri", colour = "gray10"),
        legend.key.height = unit(0.5 , "cm"),
        legend.position = "right")
# Save R2 plot
ggsave("figures/r_squared.png" , width = 10 , height = 8 , units = "in")

## Create plots of linearized plots with R2 values
# Create tibble with where to place R2 values on plot
text_placement <- models %>%
  select(bottle , cumu_methane) %>%
  group_by(bottle) %>%
  summarize(x = max(cumu_methane)) %>%
  ungroup() %>%
  mutate(x = x*0.75,
         y = x*0.45)
x = text_placement$x
y = text_placement$y


# Run loop to create plots
for (b in 1:length(model_names)) {
  r_2_plot <- r_square %>%
    filter(model == model_names[b]) %>%
    mutate(x = x,
           y = y)
  models %>%
    filter(model == model_names[b]) %>%
    ggplot()+
    geom_point(aes(x = cumu_methane,
                   y = est_yield),
               color = model_colors[b]) +
    geom_line(aes(x = cumu_methane,
                  y = cumu_methane),
              size = 0.25) +
    geom_text(data = r_2_plot,
              aes(x = x,
                  y = y,
                  label = round(r_square , 4)),
              size = 3.75) +
    facet_wrap(~bottle , scales = "free" , ncol = 3) +
    labs(x = "Observed Methane Yield (mL/gVS)",
         y = "Estimated Methane Yield (mL/gVS)",
         caption = "Note: Black line represents acutal methane production.")+
    scale_x_continuous(labels = scales::number_format(accuracy = 1) , n.breaks = 5) +
    theme_igray() +
    theme(panel.grid.major = element_line(colour = "white", size = .5),
          panel.grid.minor = element_line(colour = "white", size = .5),
          text = element_text(size = 14, family = "Calibri", colour = "gray10"),
          plot.caption = element_text(hjust = 0, face= "italic"),
          strip.background.x = element_rect(size = 0)) 
  ggsave(paste("figures/",model_names[b],"_r_sqaured.png"),
         height = 8,
         width = 10,
         unit = "in")
}
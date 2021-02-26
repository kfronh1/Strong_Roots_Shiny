library(tidyverse)
library(dplyr)
library(janitor)
library(ggplot2)
library(readxl)
library(here)

se_data <- read_csv("se_data.csv")

# put it into a range



se_data$meter_range = cut(se_data$NEAR_DIST, c(0, 1000, 6200, 11400, 16600, 21800, 27000))

distance_range <- as.data.frame(tabyl(se_data$`meter_range`, sort = TRUE))

se_data$meter_range_code = cut(se_data$NEAR_DIST, c(0, 1000, 6200, 11400, 16600, 21800, 27000))

se_data$meter_range_code <- as.character(se_data$meter_range_code)

se_data$meter_range_code <- as.vector(se_data$meter_range_code)






df_83 <- as.data.frame(tabyl(se_data$`83_rni_limits_local_protests_relations`, sort = TRUE))
df_83

df_79 <- as.data.frame(tabyl(se_data$`79_rni_limits_started_protests`, sort = TRUE))
df_79

df_84 <- as.data.frame(tabyl(se_data$`84_soldiers_guards_violence`, sort = TRUE))
df_84

df_85 <- as.data.frame(tabyl(se_data$`85_soldiers_guards_killings`, sort = TRUE))
df_85

df_87 <- as.data.frame(tabyl(se_data$`87_rni_officials_harass_local_populations`, sort = TRUE))
df_87

df_88 <- as.data.frame(tabyl(se_data$`88_harassment_mistrust_rni_agents`, sort = TRUE))
df_88

df_114 <- as.data.frame(tabyl(se_data$`114_ngos_protect_flagship_species`, sort = TRUE))
df_114

df_138 <- as.data.frame(tabyl(se_data$`138_areas_linked_good_roads`, sort = TRUE))
df_138

df_140 <- as.data.frame(tabyl(se_data$`140_chiefdom_develops_ag_roads`, sort = TRUE))
df_140

df_142 <- as.data.frame(tabyl(se_data$`142_poor_bridges_repaired_asap`, sort = TRUE))
df_142

df_143 <- as.data.frame(tabyl(se_data$`143_chiefdom_roads_good_condition`, sort = TRUE))
df_143

df_147 <- as.data.frame(tabyl(se_data$`147_secure_area`, sort = TRUE))
df_147

df_162 <- as.data.frame(tabyl(se_data$`162_agriculture_threatens_biodiversity`, sort = TRUE))
df_162

df_163 <- as.data.frame(tabyl(se_data$`163_livestock_threatens_biodiversity`, sort = TRUE))
df_163

df_169 <- as.data.frame(tabyl(se_data$`169_deforestation_cause_commercial_ag`, sort = TRUE))
df_169





ggplot(data = se_data, aes(x = `85_soldiers_guards_killings`, fill = `85_soldiers_guards_killings`)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("Relative Frequency") +
  labs(title = "Actions of soldiers and forest \n guards create real insecurity") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(name ="Response",
                   limits=c("Totally Agree","Agree","Disagree", "Strongly Disagree")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("black"))


table(se_data$meter_range, se_data$`85_soldiers_guards_killings`)





# MAKE TOTAL TABLE THAT SHOWS MEAN ANSWER BY GROUPING


mean_distance <- as.data.frame(aggregate(x = se_data$NEAR_DIST, by = list(se_data$grouping_con), FUN = mean))

mean_138 <- as.data.frame(aggregate(x = se_data$`138_areas_linked_good_roads`, by = list(se_data$grouping_con), FUN = mean))

mean_140 <- as.data.frame(aggregate(x = se_data$`140_chiefdom_develops_ag_roads`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(mean_distance, mean_138, by= "Group.1") %>%
  rename(
    mean_distance = x.x,
    mean_138 = x.y)

total <- merge(total, mean_140, by= "Group.1") %>%
  rename(mean_140 = x)

mean_143 <- as.data.frame(aggregate(x = se_data$`143_chiefdom_roads_good_condition`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_143, by= "Group.1") %>%
  rename(mean_143 = x)

mean_144 <- as.data.frame(aggregate(x = se_data$`144_poor_roads_being_serviced`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_144, by= "Group.1") %>%
  rename(mean_144 = x)

mean_146 <- as.data.frame(aggregate(x = se_data$`146_vehicle_access_community_forests`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_146, by= "Group.1") %>%
  rename(mean_146 = x)

mean_192 <- as.data.frame(aggregate(x = se_data$`92_ccc_interface_population_officials`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_192, by= "Group.1") %>%
  rename(mean_192 = x)

mean_107 <- as.data.frame(aggregate(x = se_data$`107_regulatory_instruments_encourage_conservation`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_107, by= "Group.1") %>%
  rename(mean_107 = x)

mean_106 <- as.data.frame(aggregate(x = se_data$`106_community_involvement_all_stages`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_106, by= "Group.1") %>%
  rename(mean_106 = x)

mean_153 <- as.data.frame(aggregate(x = se_data$`153_rni_land_development_essential`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_153, by= "Group.1") %>%
  rename(mean_153 = x)

mean_172 <- as.data.frame(aggregate(x = se_data$`172_deforestation_cause_illegal_logging`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_172, by= "Group.1") %>%
  rename(mean_172 = x)





mean_distance <- as.data.frame(aggregate(x = se_data$NEAR_DIST, by = list(se_data$grouping_con), FUN = mean))

mean_138 <- as.data.frame(aggregate(x = se_data$`138_areas_linked_good_roads`, by = list(se_data$grouping_con), FUN = mean))

mean_140 <- as.data.frame(aggregate(x = se_data$`140_chiefdom_develops_ag_roads`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(mean_distance, mean_138, by= "Group.1") %>%
  rename(
    mean_distance = x.x,
    mean_138 = x.y)

total <- merge(total, mean_140, by= "Group.1") %>%
  rename(mean_140 = x)

mean_143 <- as.data.frame(aggregate(x = se_data$`143_chiefdom_roads_good_condition`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_143, by= "Group.1") %>%
  rename(mean_143 = x)

mean_144 <- as.data.frame(aggregate(x = se_data$`144_poor_roads_being_serviced`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_144, by= "Group.1") %>%
  rename(mean_144 = x)

mean_146 <- as.data.frame(aggregate(x = se_data$`146_vehicle_access_community_forests`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_146, by= "Group.1") %>%
  rename(mean_146 = x)

mean_192 <- as.data.frame(aggregate(x = se_data$`92_ccc_interface_population_officials`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_192, by= "Group.1") %>%
  rename(mean_192 = x)

mean_107 <- as.data.frame(aggregate(x = se_data$`107_regulatory_instruments_encourage_conservation`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_107, by= "Group.1") %>%
  rename(mean_107 = x)

mean_106 <- as.data.frame(aggregate(x = se_data$`106_community_involvement_all_stages`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_106, by= "Group.1") %>%
  rename(mean_106 = x)

mean_153 <- as.data.frame(aggregate(x = se_data$`153_rni_land_development_essential`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_153, by= "Group.1") %>%
  rename(mean_153 = x)

mean_172 <- as.data.frame(aggregate(x = se_data$`172_deforestation_cause_illegal_logging`, by = list(se_data$grouping_con), FUN = mean))

total <- merge(total, mean_172, by= "Group.1") %>%
  rename(mean_172 = x)




# Plot the heatmap


data_for_heat <- total

data_for_heat_long <- gather(data_for_heat, mean, score, mean_138:mean_172, factor_key = TRUE)

data_for_heat_long$Group.1 <- factor(data_for_heat_long$Group.1)

data_for_heat_long$mean_distance <- factor(data_for_heat_long$mean_distance)


#######################################################################
### Load required libraries ###########################################
#######################################################################

library(ggplot)
library(tidyr)
library(ggtext)
library(camcorder)

#######################################################################
### Load data #########################################################
#######################################################################

anes <- read.csv("~/Desktop/anes_pilot_2022_csv_20221214.csv")

#######################################################################
### Create single clean dataset #######################################
#######################################################################

anes_clean <- anes |> 
  mutate(political_leaning = case_when(
    pid1r == 1 ~ "Democrat leaning",
    pid1r == 2 ~ "Republican leaning",
    pidlean == 1 ~ "Republican leaning",
    pidlean == 2 ~ "Democrat leaning",
    .default = NA)) |> 
  mutate(trust_climate_experts = factor(
    case_when(
    trustexpclim == 1 ~ "Completely",
    trustexpclim == 2 ~ "A great deal",
    trustexpclim == 3 ~ "A moderate amount",
    trustexpclim == 4 ~ "A little",
    trustexpclim == 5 ~ "Not at all",
    .default = NA),
    levels = c("Completely", "A great deal", "A moderate amount", "A little", "Not at all"))) |> 
  mutate(trust_public_health_experts = factor(
    case_when(
    trustexpcdc == 1 ~ "Completely",
    trustexpcdc == 2 ~ "A great deal",
    trustexpcdc == 3 ~ "A moderate amount",
    trustexpcdc == 4 ~ "A little",
    trustexpcdc == 5 ~ "Not at all",
    .default = NA),
    levels = c("Completely", "A great deal", "A moderate amount", "A little", "Not at all"))) |> 
  mutate(trust_economists = factor(
    case_when(
    trustecon == 1 ~ "Completely",
    trustecon == 2 ~ "A great deal",
    trustecon == 3 ~ "A moderate amount",
    trustecon == 4 ~ "A little",
    trustecon == 5 ~ "Not at all",
    .default = NA),
    levels = c("Completely", "A great deal", "A moderate amount", "A little", "Not at all"))) |> 
  filter(complete.cases(political_leaning)) |> 
  select(trust_climate_experts, 
         trust_public_health_experts, 
         trust_economists,
         political_leaning)

#######################################################################
### Table #############################################################
#######################################################################

table(anes_clean$trust_climate_experts, 
      anes_clean$political_leaning, 
      useNA = "ifany")

#######################################################################
### Proportion Table  #################################################
#######################################################################

prop.table(table(anes_clean$trust_climate_experts, 
                 anes_clean$political_leaning, 
                 useNA = "ifany"), 
           margin = 2)

#######################################################################
### Barchart: overall #################################################
#######################################################################

## doesn't really answer our question

anes_clean |>
  ggplot(aes(x = trust_climate_experts)) +
  geom_bar(color = "black", fill = "#596699") +
  labs(x = "Trust in climate experts",
       y = "Frequency",
       title = "Trust in climate experts") +
  theme_minimal()

#######################################################################
### Barchart: stacked barchart (frequency on y axis) ##################
#######################################################################

## doesn't really answer our question

anes_clean |>
  ggplot(aes(x = trust_climate_experts, fill = political_leaning)) +
  geom_bar() +
  labs(x = "Trust in climate experts",
       y = "Frequency",
       fill = "Political leaning",
       title = "Trust in climate experts") +
  theme_minimal()

########################################################################
### Barchart: dodged barchart (frequency on y axis) ####################
########################################################################

## more helpful, but the Ns aren't the same in each group

anes_clean |>
  ggplot(aes(x = trust_climate_experts, 
             y = (..count..)/sum(..count..),
             group = political_leaning,
             fill = political_leaning)) +
  geom_bar(position = "dodge") +
  labs(x = "Trust in climate experts",
       y = "% of respondents",
       fill = "Political leaning",
       title = "Trust in climate experts by political leaning") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

########################################################################
### Barchart: dodged barchart (% on y axis) ############################
########################################################################

anes_clean |>
  ggplot(aes(x = trust_climate_experts, 
             y = (..count..)/sum(..count..),
             group = political_leaning,
             fill = political_leaning)) +
  geom_bar(position = "dodge") +
  labs(x = "Trust in climate experts",
       y = "Frequency",
       fill = "Political leaning",
       title = "Trust in climate experts") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

########################################################################
### Barchart: faceted barchart (% on y axis) ###########################
########################################################################

anes_clean |>
  ggplot(aes(x = trust_climate_experts, 
             y = (..count..)/sum(..count..),
             group = political_leaning)) +
  geom_bar(position = "dodge") +
  labs(x = "Trust in climate experts",
       y = "Frequency",
       title = "Trust in climate experts") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~political_leaning, ncol = 1) + 
  theme_minimal()

########################################################################
### Barchart: stacked barchart (another way) ###########################
########################################################################

anes_clean |>
  mutate(trust_climate_experts_flipped = factor(trust_climate_experts,
         levels = rev(levels(anes_clean$trust_climate_experts)))) %>%
  ggplot(aes(y = political_leaning, fill = trust_climate_experts_flipped)) +
  geom_bar(position = "fill", color = "black") +
  labs(x = "% of respondents",
       y = "",
       fill = "How much do you trust climate experts?",
       title = "Trust in climate experts") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #scale_fill_brewer(palette = "Purples") +
  scale_fill_manual(values = rev(c("#3E6487", "#829CB2", "#C7CDD1", "#EDAD88", "#E36C33"))) +
  scale_x_continuous(labels = scales::percent_format()) +
  guides(fill = guide_legend(reverse = TRUE))

########################################################################
### Barchart: stacked barchart (legend on top) #########################
########################################################################

color_vector <-  rev(c("#3E6487", "#829CB2", "#C7CDD1", "#EDAD88", "#E36C33"))
ordered_labels <- c("Not at all", "A little", "A moderate amount", "A great deal", "Completely")

colored_subtitle <- paste0(
  "<span style='color:", color_vector[5], "'>", ordered_labels[5], "</span>, ",
  "<span style='color:", color_vector[4], "'>", ordered_labels[4], "</span>, ",
  "<span style='color:", color_vector[3], "'>", ordered_labels[3], "</span>, ",
  "<span style='color:", color_vector[2], "'>", ordered_labels[2], "</span>, ",
  "<span style='color:", color_vector[1], "'>", ordered_labels[1], "</span>"
)


anes_clean |>
  mutate(trust_climate_experts_flipped = factor(trust_climate_experts, levels = ordered_labels)) %>%
  ggplot(aes(y = political_leaning, fill = trust_climate_experts_flipped)) +
  geom_bar(position = "fill", color = "black") +
  labs(x = "% of respondents",
       y = "",
       fill = "How much do you trust climate experts?",
       title = "How much do you trust the expert opinion of climate scientists?",
       subtitle = colored_subtitle) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.subtitle = element_markdown()) +
  #scale_fill_brewer(palette = "Purples") +
  scale_fill_manual(values = color_vector) +
  scale_x_continuous(labels = scales::percent_format()) +
  guides(fill = guide_legend(reverse = TRUE))

########################################################################
### Getting even fancier ###############################################
########################################################################

anes_clean |>
  mutate(trust_climate_experts_flipped = factor(trust_climate_experts, levels = ordered_labels)) %>%
  ggplot(aes(y = political_leaning, fill = trust_climate_experts_flipped)) +
  geom_bar(position = "fill", color = "black") +
  labs(x = "% of respondents",
       y = "",
       fill = "How much do you trust climate experts?",
       title = "How much do you trust the expert opinion of climate scientists?",
       subtitle = colored_subtitle) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(colour = "black", family = "Barlow"),
        plot.title = element_text(hjust = 0.5, size = rel(1.2), face = "bold"),
        plot.subtitle = element_markdown(hjust = 0.5, size = rel(1.1), face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_fill_manual(values = color_vector) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(labels = scales::percent_format()) 

#######################################################################
### Start recording of building example ###############################
#######################################################################

gg_record(
  dir = file.path("figure_versions"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


anes_clean |>
  mutate(trust_climate_experts_flipped = factor(trust_climate_experts, levels = ordered_labels)) %>%
  ggplot(aes(y = political_leaning, fill = trust_climate_experts_flipped)) +
  geom_bar(position = "fill", color = "black") +
  labs(x = "% of respondents",
       y = "",
       fill = "How much do you trust climate experts?",
       title = "How much do you trust the expert opinion of climate scientists?",
       subtitle = colored_subtitle) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(colour = "black", family = "Barlow"),
        plot.title = element_text(hjust = 0.5, size = rel(1.2), face = "bold"),
        plot.subtitle = element_markdown(hjust = 0.5, size = rel(1.1), face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_fill_manual(values = color_vector) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(labels = scales::percent_format()) 


gg_stop_recording()

gg_playback(
  name = file.path("figure_versions/figure_versions.gif"),
  first_image_duration = 5,
  last_image_duration = 5,
  frame_duration = .75,
  background = "white"
)
  
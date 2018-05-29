library(lubridate)
library(tidyverse)
library(ggridges)

load("data/crash_bridge_matches.rdata") ## crash ids matched to bridges
load("data/agg_crash_data.rdata") ## full aggregate crash data
load("data/bridge_timeseries_for_crashes_may24.rdata")

matched_crashes <- unique(crashes_join$crash_number)
matched_bridge_ids <- unique(crashes_join$bridge_department_number)

crash_df <- full_crash_df %>%
  dplyr::filter(crash_number %in% matched_crashes) %>%
  dplyr::select(
    crash_number,
    crash_date,
    crash_severity,
    total_nonfatal_injuries,
    total_fatal_injuries,
    most_harmful_events
    ) %>%
  mutate(
    crash_year = year(crash_date)
  ) %>%
  left_join(crashes_join %>% dplyr::select(crash_number, crash_date, bridge_department_number, distance_crash_bridge)) %>%
  rename(
    bridgeID = bridge_department_number
  )

bridge_df <- bridge_ts %>%
  dplyr::filter(bridgeID %in% matched_bridge_ids & data_year > 2008) %>%
  mutate(
    deck = ifelse(deck == -999, NA, deck),
    superstructure = ifelse(superstructure == -999, NA, superstructure),
    substructure = ifelse(substructure == -999, NA , substructure),
    min_health = factor(pmin(deck, superstructure, substructure, na.rm=T)),
    area_sm = area_sf * 0.3048
  )

bridge_info <- bridge_df %>%
  group_by(bridgeID) %>%
  summarize(
    area_sm = first(area_sm),
    adt = first(adt_029)
  )

crash_df <- crash_df %>%
  left_join(bridge_info, by = "bridgeID")

crashes_by_bridge <- crash_df %>%
  group_by(crash_year, bridgeID) %>%
  mutate(
    num_crashes = n(),
  ) %>%
  group_by(crash_year, bridgeID, crash_severity) %>%
  summarize(
    num_crashes = first(num_crashes),
    # crashes_over_adt = ifelse(!is.na(first(adt)) & first(adt) > 0,num_crashes/first(adt),NA),
    num_crashes_by_severity = n(),
    # crashes_over_adt_by_severity = num_crashes_by_severity/first(adt)
  )

bridge_df <- bridge_df %>%
  left_join(crashes_by_bridge, by = c("data_year" = "crash_year", "bridgeID"))
# %>%
#   mutate(
#     num_crashes = ifelse(is.na(num_crashes), 0.01, num_crashes),
#     num_crashes_by_severity = ifelse(is.na(num_crashes_by_severity), 0.01, num_crashes_by_severity)
#   )

bridge_df %>%
  group_by(bridgeID) %>%
  summarize(
    num_crashes = first(num_crashes),
    cndtn = first(cndtn)
  ) %>%
  ggplot(aes(x = (num_crashes), fill = cndtn)) + geom_density(position="dodge", alpha = 0.3) + scale_x_log10(labels = comma)

bridge_df %>%
  group_by(bridgeID) %>%
  summarize(
    num_crashes = first(num_crashes),
    cndtn = first(cndtn)
  ) %>%
  ggplot(aes(x = (num_crashes), y = cndtn, fill = cndtn)) +
  geom_density_ridges(alpha = 0.3) +
  scale_x_log10(labels = function (x) floor(x)) +
  labs(
    title = "Density of # of Crashes per Bridge Between 2008 and 2015 by Bridge Condition",
    x = "Number of Crashes 2008-2015",
    y = "Bridge Condition"
  ) +
  ggthemes::theme_hc() +
  theme(plot.title = element_text(size=22, hjust = 0.5))

# bridge_df %>%
#   group_by(bridgeID) %>%
#   summarize(
#     crashes_over_adt = first(crashes_over_adt),
#     cndtn = first(cndtn)
#   ) %>%
#   ggplot(aes(x = (crashes_over_adt), y = cndtn, fill = cndtn)) +
#   geom_density_ridges(alpha = 0.3) +
#   scale_x_log10(labels = function (x) floor(x)) +
#   labs(
#     title = "Density of # of Crashes Per ADT per Bridge Between 2008 and 2015 by Bridge Condition",
#     x = "Number of Crashes 2008-2015",
#     y = "Bridge Condition"
#   ) +
#   ggthemes::theme_hc() +
#   theme(plot.title = element_text(size=22, hjust = 0.5))

# require(scales)
bridge_df %>%
  dplyr::filter(!is.na(crash_severity)) %>%
  ggplot(aes(x = (num_crashes_by_severity), y = cndtn, fill = cndtn)) +
  geom_density_ridges(alpha = 0.3) +
  scale_x_log10(labels = function (x) floor(x)) +
  facet_wrap(~crash_severity) +
  labs(
    title = "Density of # of Crashes by Severity per Bridge Between 2008 and 2015",
    x = "Number of Crashes 2008-2015",
    y = "Bridge Condition"
  ) +
  ggthemes::theme_hc() +
  theme(plot.title = element_text(size=22, hjust = 0.5))


bridge_df %>%
  group_by(bridgeID) %>%
  summarize(
    num_crashes = first(num_crashes),
    cndtn = first(cndtn),
    min_health = (first(min_health))
  ) %>%
  ggplot(aes(x = (num_crashes), y = min_health, fill = min_health)) +
  geom_density_ridges(alpha = 0.3) +
  scale_x_log10(labels = function (x) floor(x)) +
  labs(
    title = "Density of # of Crashes per Bridge Between 2008 and 2015 by Bridge Health Score",
    x = "Number of Crashes 2008-2015",
    y = "Bridge Health Score (Minimum of Parts)"
  ) +
  ggthemes::theme_hc() +
  theme(legend.position="none") +
  theme(plot.title = element_text(size=22, hjust = 0.5))


bridge_df %>%
  dplyr::filter(!is.na(crash_severity)) %>%
  ggplot(aes(x = (num_crashes_by_severity), y = min_health, fill = min_health)) +
  geom_density_ridges(alpha = 0.3) +
  scale_x_log10(labels = function (x) floor(x)) +
  facet_wrap(~crash_severity) +
  labs(
    title = "Density of # of Crashes By Severity per Bridge Between 2008 and 2015 Across Bridge Health Score",
    x = "Number of Crashes 2008-2015",
    y = "Bridge Health Score (Minimum of Parts)"
  ) +
  ggthemes::theme_hc() +
  theme(legend.position="none") +
  theme(plot.title = element_text(size=22, hjust = 0.5))

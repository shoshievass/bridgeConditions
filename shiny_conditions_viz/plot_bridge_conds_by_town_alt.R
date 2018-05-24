library(tidyverse)

# Load complete time series
load("data/bridge_timeseries_merged_by_proj.rdata")

bridge_ts <- bridge_ts %>%
  arrange(bridgeID, data_year) %>%
  mutate(
    spending = ifelse(is.na(total_bridge_spending), 0, total_bridge_spending ),
    spending_in_year = ifelse( spending > 0, data_year, NA),
    project_end_year = ifelse(spending > 0, last_end_year, NA)
    # min_health_project_start = ifelse( spending > 0, min_health, NA),
    # min_health_project_start = ifelse( spending > 0, min_health, NA),
  ) %>%
  group_by(bridgeID) %>%
  mutate(
    total_spending = sum(spending),
    has_projects = ifelse( total_spending > 0 , 1, 0)
  ) %>%
  ungroup()

town_list <- unique(bridge_ts$town) 
town_list <- town_list[!is.na(town_list)]

# sample_town <- sample(town_list,1)
sample_town <- "Hanover"

project_list <- unique(bridge_ts$proj_no)
project_list <- project_list[!is.na(project_list)]
sample_proj <- sample(project_list,1)
sample_proj <- 601263

for(townID in sample_town){
  print(paste0("Working for ", townID))
  
  bridge_list <- bridge_ts$bridgeID[bridge_ts$proj_no %in% 603183]
  bridge_ts_small <- bridge_ts %>%
    dplyr::filter(bridgeID %in% bridge_list) %>%
    # dplyr::filter(town %in% sample_town) %>%
    # dplyr::filter(proj_no %in% sample_proj) %>%
    dplyr::select(
      bridgeID,
      proj_no,
      has_projects,
      data_year,
      spending_in_year,
      spending,
      project_end_year,
      # deck,
      # superstructure,
      # substructure,
      # culvert,
      min_health
    ) %>%
    gather(
      key = structure,
      value = health,
      -bridgeID,
      -data_year,
      -spending,
      -spending_in_year,
      -project_end_year,
      -proj_no,
      -has_projects
    )
  
  bridge_ts_small$proj_no <- factor(bridge_ts_small$proj_no)
  addNA(bridge_ts_small$proj_no)
  
  town_plot <- d
    
    bridge_ts_small %>%
    ggplot(
      aes(group=bridgeID)
    ) + geom_line(aes(x=data_year, y = health)) +
    facet_wrap(~bridgeID) +
    geom_vline(data = bridge_ts_small, aes(xintercept = spending_in_year, color=(proj_no)), linetype = "dashed") + 
    geom_vline(data = bridge_ts_small, aes(xintercept = project_end_year, color=(proj_no))) +
    labs(x="Year", 
         y="Bridge Condition Score",
         # title= paste0("Minimum Bridge Condition Score vs Spending by Year for Town ", townID),
         subtitle="Dashed line is project record year; straight line is year of completion") + 
    guides(color=F) + 
    scale_colour_hue()
  + geom_label(data = bridge_ts_small, aes(label = as.character(proj_no), x = project_end_year, y = min(bridge_ts_small$health, na.rm = T), color = factor(proj_no)), nudge_y = 0.25)
  
  # ggsave(town_plot, file = paste0("graphs/MinScorePlots/MinCondition_vs_spending_town_", townID, ".png"), height = 15, width = 20)
  
}


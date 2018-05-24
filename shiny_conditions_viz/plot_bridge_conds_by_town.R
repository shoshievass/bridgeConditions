library(tidyverse)

# Load complete time series
load("data/bridge_timeseries_merged.rdata")

bridge_ts <- bridge_ts %>%
  arrange(bridgeID, data_year) %>%
  mutate(
    spending = ifelse(is.na(total_bridge_spending), 0, total_bridge_spending ),
    spending_in_year = ifelse( spending > 0, data_year, NA),
    project_end_year = ifelse(spending > 0, last_end_year, NA)
  )

town_list <- unique(bridge_ts$town) 
town_list <- town_list[!is.na(town_list)]

# for(townID in town_list){
#   print(paste0("Working for ", townID))
#   
#   bridge_ts_small <- bridge_ts %>%
#     dplyr::filter(town == townID) %>%
#     dplyr::select(
#       bridgeID,
#       data_year,
#       spending_in_year,
#       spending,
#       deck,
#       superstructure,
#       substructure,
#       culvert,
#       min_health
#     ) %>%
#     gather(
#       key = structure,
#       value = health,
#       -bridgeID,
#       -data_year,
#       -spending,
#       -spending_in_year
#     )
#   
#   town_plot <- bridge_ts_small %>%
#     ggplot(
#       aes(group=bridgeID, color=bridgeID)
#     ) + geom_line(aes(x=data_year, y = health)) +
#     facet_grid(structure~bridgeID) +
#     geom_vline(data = bridge_ts_small, aes(xintercept = spending_in_year)) + 
#     ggtitle(paste0("Bridge Conditions vs Spending by Year for Town ", townID)) + 
#     guides(color=F)
#   
#   ggsave(town_plot, file = paste0("graphs/ConditionsPlots/Conditions_vs_spending_town_", townID, ".png"))
#   
# }

for(townID in town_list){
  print(paste0("Working for ", townID))
  
  bridge_ts_small <- bridge_ts %>%
    # dplyr::filter(bridgeID %in% bridge_sample) %>%
    dplyr::filter(town %in% "C13") %>%
    dplyr::select(
      bridgeID,
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
      -project_end_year
    )
  
  town_plot <- bridge_ts_small %>%
    ggplot(
      aes(group=bridgeID, color=bridgeID)
    ) + geom_line(aes(x=data_year, y = health)) +
    facet_wrap(structure~bridgeID) +
    geom_vline(data = bridge_ts_small, aes(xintercept = spending_in_year), linetype = "dashed") + 
    geom_vline(data = bridge_ts_small, aes(xintercept = project_end_year)) +
    labs(x="Year", 
         y="Bridge Condition Score",
         title= paste0("Minimum Bridge Condition Score vs Spending by Year for Town ", townID),
         subtitle="Dashed line is project record year; straight line is year of completion") + 
    guides(color=F)
  
  ggsave(town_plot, file = paste0("graphs/MinScorePlots/MinCondition_vs_spending_town_", townID, ".png"), height = 15, width = 20)
  
}


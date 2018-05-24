library(shiny); library(tidyverse)
load("bridge_timeseries_merged_by_proj.rdata")

bridge_ts <- bridge_ts %>%
  arrange(bridgeID, data_year) %>%
  mutate(
    spending = ifelse(is.na(total_bridge_spending), 0, total_bridge_spending ),
    spending_in_year = ifelse( spending > 0, data_year, NA),
    project_end_year = ifelse(spending > 0, last_end_year, NA),
    project_init_year = ifelse(spending > 0, first_start_year, NA)
  ) %>%
  group_by(bridgeID) %>%
  mutate(
    total_spending = sum(spending),
    has_projects = ifelse( total_spending > 0 , 1, 0)
  ) %>%
  ungroup()

town_list <- unique(bridge_ts$town) 
town_list <- town_list[!is.na(town_list)]

ui <- fluidPage(
  
  titlePanel("Bridge Conditions and Maintenance Projects", windowTitle = "Plots of Bridge Conditions by Town Against Maintenance Project Start/End Dates"),
  
  plotOutput("conditionPlot"),
  
  hr(),
  
  
  fluidRow(
    column(4,
           selectInput("townInput", "Town",
                       choices = town_list,
                       selected= "Alford")
    ),
    column(4,
           radioButtons("segmentInput", "Bridge Segment",
                        choices = c(
                          "Deck" = "deck",
                          "Superstructure" = "superstructure",
                          "Substructure" = "substructure",
                          "Culvert" = "culvert",
                          "Overall Minimum" = "min_health"
                        ),
                        selected = "min_health"
           )
    ),
    column(4,
           checkboxGroupInput("dateInput", "Project Dates",
                              choices = c(
                                "Project Record Year" = "proj_date",
                                "Project Start Year" = "init_date",
                                "Project End Year" = "end_date"
                              ),
                              selected = c(
                                "init_date",
                                "end_date"
                              )
           )
    )
  )
  
)
server <- function(input, output) {
  
  output$conditionPlot <- renderPlot({
    
    townID <- input$townInput
    segment <- input$segmentInput
    dates <- as.array(input$dateInput)
    
    bridge_ts_small <- bridge_ts %>%
      dplyr::filter(town %in% townID) %>%
      dplyr::select(
        bridgeID,
        data_year,
        proj_no,
        has_projects,
        spending_in_year,
        spending,
        project_init_year,
        project_end_year,
        segment
      ) %>%
      gather(
        key = structure,
        value = health,
        -bridgeID,
        -data_year,
        -spending,
        -spending_in_year,
        -project_end_year,
        -project_init_year,
        -proj_no,
        -has_projects
      )
    
    project_indicator = max(bridge_ts_small$has_projects)
    
    bridgePlot <- bridge_ts_small %>%
      ggplot(
        aes(group=bridgeID)
      ) + geom_line(aes(x=data_year, y = health)) +
      facet_wrap(structure~bridgeID) +
      labs(x="Year", 
           y="Bridge Condition Score",
           title= paste0("Bridge Part Condition Score vs Spending by Year for Town ", townID),
           subtitle="Dotted line is the project record year. Dashed line is the project start year. Straight line is year of completion"
      )+ 
      guides(color=F)
    
    if(
      "proj_date" %in% dates
    ){
      print("proj date")
      if(project_indicator == 1){
        bridgePlot <- bridgePlot + geom_vline(data = bridge_ts_small, aes(xintercept = spending_in_year, color=factor(proj_no)), linetype = "dotted") 
      }
      else{
        bridgePlot <- bridgePlot + geom_vline(data = bridge_ts_small, aes(xintercept = spending_in_year), linetype = "dotted") 
      }
    }
    
    if(
      "init_date" %in% dates
    ){
      print("init date")
      if(project_indicator == 1){
        print("has projects!")
        bridgePlot <- bridgePlot + geom_vline(data = bridge_ts_small, aes(xintercept = project_init_year, color=factor(proj_no)), linetype = "dashed")
      }
      else{
        bridgePlot <- bridgePlot + geom_vline(data = bridge_ts_small, aes(xintercept = project_init_year), linetype = "dashed")
      }
    }
    if(
      "end_date" %in% dates
    ){
      print("end_date")
      if(project_indicator == 1){
        bridgePlot <- bridgePlot + geom_vline(data = bridge_ts_small, aes(xintercept = project_end_year, color=factor(proj_no)))
      }
      else{
        bridgePlot <- bridgePlot + geom_vline(data = bridge_ts_small, aes(xintercept = project_end_year))
      }
    }
    
    bridgePlot
    
  })
}



shinyApp(ui = ui, server = server)
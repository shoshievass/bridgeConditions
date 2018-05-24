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
project_list <- unique(bridge_ts$proj_no)
project_list <- project_list[!is.na(project_list)]
bridgeID_list <- unique(bridge_ts$bridgeID)
bridgeID_list <- bridgeID_list[!is.na(project_list)]

ui <- fluidPage(
  
  titlePanel("Bridge Conditions and Maintenance Projects", windowTitle = "Plots of Bridge Conditions by Town Against Maintenance Project Start/End Dates"),
  
  fluidRow(
    column(4, align="center",
           radioButtons("viewInput","View Bridges By:",
                        choices = c("Town" = "town",
                                    "Project" = "project",
                                    "BridgeID" = "bridgeID"),
                        selected = "town"),
           
           checkboxGroupInput("dateInput", "Project Dates",
                              choices = c(
                                "Project Start Year" = "init_date",
                                "Project End Year" = "end_date"
                              ),
                              selected = c(
                                "init_date",
                                "end_date"
                              )
           )
    ),
    column(4, align="center",
           selectInput("townInput", "Town",
                       choices = town_list,
                       selected = "Alford"
           ),
           selectInput("projectInput", "Project",
                       choices = project_list,
                       selected = NULL
           ),
           selectInput("bridgeInput", "BridgeID",
                       choices = bridgeID_list,
                       selected = NULL
           )
    ),
    column(4, align="center",
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
           )
  ),
  
  br(),
  
  plotOutput("conditionPlot")

)
server <- function(input, output) {
  
  output$conditionPlot <- renderPlot({
    
    townID <- input$townInput
    segment <- input$segmentInput
    dates <- as.array(input$dateInput)
    projectID <- input$projectInput
    view <- input$viewInput
    bridge <- input$bridgeInput
    
    print(view)
    print(projectID)
    
    if (view == "town") {
      bridge_ts_small <- bridge_ts %>%
        dplyr::filter(town %in% townID)
    }
    else if (view == "project") {
      bridge_list <- bridge_ts$bridgeID[bridge_ts$proj_no %in% projectID]
      bridge_ts_small <- bridge_ts %>%
        dplyr::filter(bridgeID %in% bridge_list)
    }
    else if (view == "bridgeID") {
      bridge_list <- bridge_ts$bridgeID[bridge_ts$bridgeID %in% bridge]
      bridge_ts_small <- bridge_ts %>%
        dplyr::filter(bridgeID %in% bridge_list)
    }
    
    bridge_ts_small <- bridge_ts_small %>%
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
        aes(group = bridgeID)
      ) + geom_line(aes(x = data_year, y = health)) +
      facet_wrap(~bridgeID) +
      labs(x = "Year", 
           y = "Bridge Condition Score",
           subtitle = "Dotted line is the project record year. Dashed line is the project start year. Straight line is year of completion"
      ) +
      guides(color = F)
    
    if (view == "town") {
      bridgePlot <- bridgePlot + 
        labs(title = paste0("Bridge Part Condition Score vs Project Spending by Year for Town ", townID)) 
      }
    else if (view == "project") {
      bridgePlot <- bridgePlot + 
        labs(title = paste0("Bridge Part Condition Score vs Spending by Year for Bridges Involved in Project ", projectID)) 
    }
    else if (view == "bridgeID") {
      bridgePlot <- bridgePlot + 
        labs(title = paste0("Bridge Part Condition Score vs Spending by Year for Bridge: ", bridge)) 
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
        bridgePlot <- bridgePlot + geom_vline(data = bridge_ts_small, aes(xintercept = project_end_year, color = factor(proj_no)))
        bridgePlot <- bridgePlot + geom_label(data = bridge_ts_small, aes(label = proj_no, x = project_end_year, y = min(bridge_ts_small$health, na.rm=T), color = factor(proj_no)), nudge_y = 0.25)
      }
      else{
        bridgePlot <- bridgePlot + geom_vline(data = bridge_ts_small, aes(xintercept = project_end_year))
      }
    }

    bridgePlot
    
  })
}



shinyApp(ui = ui, server = server)
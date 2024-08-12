# load libraries and data
library(shiny)
library(tidyr)
library(plotly)
library(ggplot2)
library(MASS)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(pROC)

#load sources
source("~/Side Practice/NBA Data/helpers.R")
load("~/Side Practice/NBA Data/nba_shots.RData")


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "R Shiny NBA Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", tabName = "filters", icon = icon("filter")),
      selectInput("player_choice", label = "Select player",
                  choices = c("LeBron James", "Kevin Durant", "Russell Westbrook", "Stephen Curry", "Carmelo Anthony"), selected = "LeBron James"),
      uiOutput("season_choice"),
      selectInput("zone_area", label = "Shot Location",
                  choices = c("All", "Right Side(R)", "Left Side(L)", "Center(C)", "Right Side Center(RC)", "Left Side Center(LC)")),
      sliderInput("time_filter", label = "Time in Game", min = 0, max = 48, value = c(0, 48)),
      sliderInput("Shot_distance", label = "Shot Distance (ft)", min = 0, max = 40, value = c(0, 40))
      )),
  dashboardBody(
    tabsetPanel(
      tabPanel("Analytics Dashboard",
               fluidRow(
                 #KPIS
                 box(title = "Key Performance Indicators", status = "primary", solidHeader = TRUE, width = 12,
                     column(3, align = "center", h4("Total Points"), verbatimTextOutput("shot_value_kpi")),
                     column(3, align = "center", h4("Total Shot Attempts"), verbatimTextOutput("shot_attempted_kpi")),
                     column(3, align = "center", h4("Total Shots Made"), verbatimTextOutput("shot_made_kpi")),
                     column(3, align = "center", h4("FG %"), verbatimTextOutput("percentage_kpi")))),
               fluidRow(
                 box(title = "Court Shot Charts", status = "primary", solidHeader = TRUE, width = 12,
                     column(6, plotOutput("court_shots")),
                     column(6, plotOutput("outside_shots")))),
               fluidRow(
                 box(title = "Distance and Points over Time Charts", status = "primary", solidHeader = TRUE, width = 12,
                     column(6, plotOutput("shot_distances")),
                     column(6, plotOutput("shots_period"))))),
      # tabPanel("Logistics Regression Analysis",
      #          fluidRow(
      #            box(title = "Model Summary", status = "primary", solidHeader = TRUE, width = 12,
      #                column(6, verbatimTextOutput("summary")))))
      # 
      )
  )
)

# Define server logic 
server <- function(input, output, session) {
  
  # set range of seasons based on player choice
  output$season_choice <- renderUI({
    seasons <- nba_shots %>% 
      filter(player_name ==input$player_choice) %>% 
      distinct(season) %>% 
      pull()
    
    pickerInput(
      "season_choice", 
      label = "Select season", 
      choices = seasons,
      options = list(`actions-box` = TRUE),
      selected = seasons[1], 
      multiple = TRUE
    )})

  filtered_data <- reactive({
    req(input$player_choice, input$zone_area, input$season_choice, input$time_filter, input$Shot_distance)  # Ensure all inputs have values
    
    nba_shots %>%
      mutate(time = round(48 - time_remaining, 0),
             points_scored = shot_made_numeric*shot_value,
             fg_percentage = sum(shot_attempted_flag/sum(shot_made_numeric))) %>%
      filter(player_name == input$player_choice,
             if(input$zone_area != 'All') shot_zone_area == input$zone_area else TRUE,
             season %in% input$season_choice,
             time >= input$time_filter[1], time <= input$time_filter[2],
             shot_distance >= input$Shot_distance[1], shot_distance <= input$Shot_distance[2]
      )})
  
  #2 Point Shot Chart
  output$court_shots <- renderPlot({
    create_plot(filtered_data(), "2PT Field Goal")})
  
  #3 Point Shot Chart
  output$outside_shots <- renderPlot({
    create_plot(filtered_data(), "3PT Field Goal")})
  
  #Distribution of Shot Distances by Season
  output$shot_distances <- renderPlot({
    ggplot(filtered_data(), aes(x = shot_distance, fill = shot_made_flag)) +
      geom_bar(position = "stack") +
      labs(title = "Histogram of Shot Distances by Attempt",
           x = "Shot Distance", y = "Count") +
      theme_minimal() +
      scale_fill_manual("", values = c(made = "blue", missed = "orange")) +
      guides(fill=FALSE) +  # Add this line to remove the legend
      theme(
        plot.title = element_text(size = 20),  
        axis.title = element_text(size = 16),  
        axis.text = element_text(size = 14))})
  
  output$shots_period <- renderPlot({
    # Create a summarized version of your data
    summarized_data <- filtered_data() %>%
      group_by(time) %>%
      summarize(total_shot_value = sum(points_scored), .groups = "drop")
    
    # Calculate the y-axis limit as 110% of the maximum total_shot_value
    if (any(!is.na(summarized_data$total_shot_value))) {
      y_limit <- max(summarized_data$total_shot_value, na.rm = TRUE) * 1.1
    } else {
      y_limit <- 0
    }
    
    # Now use the summarized data in your ggplot function
    ggplot(summarized_data, aes(x = time, y = total_shot_value)) +
      geom_line(color = "red") +  # Make the line red
      labs(title = "Total Points Over Game Time", align = "center",
           x = "Game Time", y = "Total Points") +
      theme_minimal() +
      ylim(0, y_limit) +  # Set the y-axis limit
      theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )})
  
  # Render KPIs
  output$shot_value_kpi <- renderText({
    total_points <- sum(filtered_data()$points_scored)
    format(total_points, big.mark = ",")
  })
  
  output$shot_attempted_kpi <- renderText({
    total_attempts <- sum(filtered_data()$shot_attempted_flag)
    format(total_attempts, big.mark = ",")
  })
  
  output$shot_made_kpi <- renderText({
    total_made <- sum(filtered_data()$shot_made_numeric)
    format(total_made, big.mark = ",")
  })
  
  output$percentage_kpi <- renderText({
    summarized_data <- filtered_data() %>%
      summarize(sum_shots = sum(shot_attempted_flag), 
                sum_made = sum(shot_made_numeric), .groups = "drop") %>%
      mutate(fg_percentage = sum_made / sum_shots)
    
    fg_percentage <- summarized_data$fg_percentage * 100
    paste0(format(fg_percentage, digits = 4), "%")
  })
  
  
  # ####This is where the Logistics Regression Starts####
  # #start with the stats outside of the shiny app
  # #Do the model evaluation on the test and look at what % is correct to look at precision and accuracy, roc
  # #working towards model optimization
  # 
  # #Scenario: Based on the parameters selected what is the optimal location in the court to shoot, what the best type of shot to
  # #shoot to increase their chance of making it in, and the corresponding fg%
  # #A plot of the court plot will be graphed with a single point, along with type of shot and fg% as a small tooltip within the chart
  # 
  # # Define reactive expressions for the training and test sets
  # train_data <- reactive({
  #   train_indices <- sample(1:nrow(filtered_data()), nrow(filtered_data()) * 0.7)
  #   filtered_data()[train_indices, ]
  # })
  # 
  # test_data <- reactive({
  #   train_indices <- sample(1:nrow(filtered_data()), nrow(filtered_data()) * 0.7)
  #   filtered_data()[-train_indices, ]
  # })
  # 
  # # Fit the model on the training set
  # shot_model <- reactive({
  #   shot_model_formula <- formula(shot_made_numeric ~ shot_distance + action_type + time_remaining)
  #   glm(shot_model_formula, data = train_data(), family = "binomial")
  # })
  # 
  # # Summarize the model
  # output$summary <- renderPrint({
  #   summary(shot_model())
  # })
  # 
  # # Generate the predicted probabilities on the test set
  # shot_prob <- reactive({
  #   predict(shot_model(), newdata = test_data(), type = "response")
  # })
  #   
  # # Generate the predicted labels using a threshold of 0.5
  # predicted_labels <- reactive({
  #   ifelse(shot_prob() > 0.5, 1, 0)
  # })
}

# Run the application
shinyApp(ui, server)
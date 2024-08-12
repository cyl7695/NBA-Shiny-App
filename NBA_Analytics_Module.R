# Define UI for application
nba_analytics_dashUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Key Performance Indicators", status = "primary", solidHeader = TRUE, width = 12,
          column(3, align = "center", h4("Total Points"), verbatimTextOutput(ns("shot_value_kpi"))),
          column(3, align = "center", h4("Total Shot Attempts"), verbatimTextOutput(ns("shot_attempted_kpi"))),
          column(3, align = "center", h4("Total Shots Made"), verbatimTextOutput(ns("shot_made_kpi"))),
          column(3, align = "center", h4("FG %"), verbatimTextOutput(ns("percentage_kpi")))),
      fluidRow(
        box(title = "Court Shot Charts", status = "primary", solidHeader = TRUE, width = 12,
            column(6, plotOutput(ns("court_shots"))),
            column(6, plotOutput(ns("outside_shots")))),
        fluidRow(
          box(title = "Distance and Points over Time Charts", status = "primary", solidHeader = TRUE, width = 12,
              column(6, plotOutput(ns("shot_distances"))),
              column(6, plotOutput(ns("shots_period"))))))
    ))
}

# Define server logic 
nba_analytics_dashServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    filtered_data <- reactive({
      req(input$player_choice, input$zone_area, input$season_choice)  # Ensure all inputs have values
      
      nba_shots %>%
        filter(player_name == input$player_choice,
               if(input$zone_area != 'All') shot_zone_area == input$zone_area else TRUE,
               season %in% input$season_choice) %>%
        mutate(time = round(48 - time_remaining, 0),
               points_scored = shot_made_numeric*shot_value,
               fg_percentage = sum(shot_attempted_flag/sum(shot_made_numeric)))
    })
    
    #2 Point Shot Chart
    output$court_shots <- renderPlot({
      create_plot(filtered_data(), "2PT Field Goal")
    })
    
    #3 Point Shot Chart
    output$outside_shots <- renderPlot({
      create_plot(filtered_data(), "3PT Field Goal")
    })
    
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
          axis.text = element_text(size = 14)    
        )})
    
    
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
  })
}
  
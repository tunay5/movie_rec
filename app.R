# Load required libraries
library(shiny)
library(DT)
library(dplyr)

# Sample data for today's matches (you can replace this with real API data)
todays_matches <- data.frame(
  Match_ID = 1:6,
  Time = c("15:00", "17:30", "20:00", "15:00", "17:30", "20:00"),
  Home_Team = c("Manchester United", "Liverpool", "Arsenal", "Barcelona", "Real Madrid", "Bayern Munich"),
  Away_Team = c("Chelsea", "Manchester City", "Tottenham", "Atletico Madrid", "Sevilla", "Borussia Dortmund"),
  League = c("Premier League", "Premier League", "Premier League", "La Liga", "La Liga", "Bundesliga"),
  Home_Score_Prediction = rep(NA, 6),
  Away_Score_Prediction = rep(NA, 6),
  Winner_Prediction = rep("", 6),
  stringsAsFactors = FALSE
)

# Define UI
ui <- fluidPage(
  titlePanel("Today's Football Matches - Predictions"),
  
  # Add some styling
  tags$head(
    tags$style(HTML("
      .content-wrapper {
        margin: 20px;
      }
      .match-table {
        margin-top: 20px;
      }
      .btn-submit {
        background-color: #28a745;
        border-color: #28a745;
        margin-top: 15px;
        margin-bottom: 15px;
      }
      .btn-reset {
        background-color: #dc3545;
        border-color: #dc3545;
        margin-top: 15px;
        margin-bottom: 15px;
        margin-left: 10px;
      }
    "))
  ),
  
  div(class = "content-wrapper",
      fluidRow(
        column(12,
               h4(paste("Matches for", Sys.Date())),
               hr(),
               
               # Action buttons
               fluidRow(
                 column(3,
                        actionButton("submit_predictions", "Submit All Predictions", 
                                     class = "btn btn-success btn-submit", width = "100%")
                 ),
                 column(3,
                        actionButton("reset_predictions", "Reset All", 
                                     class = "btn btn-danger btn-reset", width = "100%")
                 ),
                 column(6,
                        div(id = "status_message", style = "padding-top: 15px;")
                 )
               ),
               
               # Editable table
               div(class = "match-table",
                   DT::dataTableOutput("matches_table")
               ),
               
               # Summary of predictions
               hr(),
               h4("Prediction Summary"),
               verbatimTextOutput("prediction_summary")
        )
      )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values to store the data
  values <- reactiveValues(
    matches_data = todays_matches
  )
  
  # Render the editable data table
  output$matches_table <- DT::renderDataTable({
    DT::datatable(
      values$matches_data,
      editable = list(
        target = 'cell',
        disable = list(columns = c(0, 1, 2, 3, 4))  # Disable editing for match info columns
      ),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = c(0), visible = FALSE),  # Hide Match_ID
          list(targets = c(5, 6), width = "80px", className = "dt-center"),  # Score columns
          list(targets = 7, width = "120px", className = "dt-center")  # Winner column
        )
      ),
      colnames = c("ID", "Time", "Home Team", "Away Team", "League", 
                   "Home Score", "Away Score", "Winner Prediction"),
      rownames = FALSE,
      selection = 'none'
    )
  }, server = FALSE)
  
  # Handle cell edits
  observeEvent(input$matches_table_cell_edit, {
    info <- input$matches_table_cell_edit
    row <- info$row
    col <- info$col + 1  # R is 1-indexed
    value <- info$value
    
    # Validate score predictions (must be numeric)
    if (col %in% c(6, 7)) {  # Home_Score_Prediction or Away_Score_Prediction columns
      if (!is.na(suppressWarnings(as.numeric(value))) && as.numeric(value) >= 0) {
        values$matches_data[row, col] <- as.numeric(value)
      } else {
        showNotification("Please enter a valid score (non-negative number)", 
                         type = "warning", duration = 3)
        return()
      }
    }
    
    # Validate winner prediction
    if (col == 8) {  # Winner_Prediction column
      valid_options <- c("Home", "Draw", "Away", "")
      if (value %in% valid_options) {
        values$matches_data[row, col] <- value
      } else {
        showNotification("Winner prediction must be 'Home', 'Draw', 'Away', or empty", 
                         type = "warning", duration = 3)
        return()
      }
    }
  })
  
  # Submit predictions
  observeEvent(input$submit_predictions, {
    # Check if all predictions are complete
    incomplete <- sum(is.na(values$matches_data$Home_Score_Prediction) | 
                        is.na(values$matches_data$Away_Score_Prediction) |
                        values$matches_data$Winner_Prediction == "")
    
    if (incomplete > 0) {
      showNotification(paste(incomplete, "matches still need complete predictions"), 
                       type = "warning", duration = 5)
    } else {
      showNotification("All predictions submitted successfully!", 
                       type = "success", duration = 5)
      
      # Here you could save the predictions to a file or database
      # saveRDS(values$matches_data, "predictions.rds")
    }
  })
  
  # Reset all predictions
  observeEvent(input$reset_predictions, {
    values$matches_data$Home_Score_Prediction <- rep(NA, nrow(values$matches_data))
    values$matches_data$Away_Score_Prediction <- rep(NA, nrow(values$matches_data))
    values$matches_data$Winner_Prediction <- rep("", nrow(values$matches_data))
    
    showNotification("All predictions have been reset", type = "message", duration = 3)
  })
  
  # Prediction summary
  output$prediction_summary <- renderText({
    data <- values$matches_data
    
    completed_scores <- sum(!is.na(data$Home_Score_Prediction) & !is.na(data$Away_Score_Prediction))
    completed_winners <- sum(data$Winner_Prediction != "")
    total_matches <- nrow(data)
    
    home_wins <- sum(data$Winner_Prediction == "Home", na.rm = TRUE)
    draws <- sum(data$Winner_Prediction == "Draw", na.rm = TRUE)
    away_wins <- sum(data$Winner_Prediction == "Away", na.rm = TRUE)
    
    paste(
      sprintf("Score predictions completed: %d/%d matches", completed_scores, total_matches),
      sprintf("Winner predictions completed: %d/%d matches", completed_winners, total_matches),
      sprintf("Predicted outcomes: %d Home wins, %d Draws, %d Away wins", home_wins, draws, away_wins),
      sep = "\n"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
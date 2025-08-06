library(shiny)
library(stringr)
library(bslib)
library(jsonlite)
library(httr)
library(shinycssloaders)
library(lsa)

df_movies <- read.csv("data/df_movies.csv")

custom_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Poppins"),
  heading_font = font_google("Poppins")
)

ui <- fluidPage(
  theme = custom_theme,
  
  titlePanel(div(h2("Movie Recommendation Algorithm", class = "text-center mt-4"))),
  
  tabsetPanel(id = "tabs", type = "hidden",
              
              tabPanel("movie_1",
                       div(class = "container mt-5",
                           div(class = "card shadow p-4",
                               h4("Movie You Watched Before", class = "mb-3"),
                               selectizeInput(
                                 "types_text_1",
                                 label = NULL,
                                 choices = df_movies$movies,
                                 multiple = TRUE,
                                 options = list(create = TRUE, placeholder = 'e.g. Interstellar, The Green Mile')
                               ),
                               selectInput(
                                 "rating_1",
                                 label = "Your Rating (0–5):",
                                 choices = seq(0, 5, by = 0.5),
                                 selected = NULL
                               ),
                               actionButton("next1", "Next ➡️", class = "btn btn-primary mt-3")
                           )
                       )
              ),
              
              tabPanel("movie_2",
                       div(class = "container mt-5",
                           div(class = "card shadow p-4",
                               h4("Movie You Watched Before", class = "mb-3"),
                               selectizeInput(
                                 "types_text_2",
                                 label = NULL,
                                 choices = df_movies$movies,
                                 multiple = TRUE,
                                 options = list(create = TRUE, placeholder = 'e.g. Interstellar, The Green Mile')
                               ),
                               selectInput(
                                 "rating_2",
                                 label = "Your Rating (0–5):",
                                 choices = seq(0, 5, by = 0.5),
                                 selected = NULL
                               ),
                               div(class = "d-flex justify-content-between mt-3",
                                   actionButton("back1", "⬅️ Back", class = "btn btn-secondary"),
                                   actionButton("next2", "Next ➡️ ", class = "btn btn-success")
                               )
                           )
                       )
              ),
              
              tabPanel("movie_3",
                       div(class = "container mt-5",
                           div(class = "card shadow p-4",
                               h4("Movie You Watched Before", class = "mb-3"),
                               selectizeInput(
                                 "types_text_3",
                                 label = NULL,
                                 choices = df_movies$movies,
                                 multiple = TRUE,
                                 options = list(create = TRUE, placeholder = 'e.g. Interstellar, The Green Mile')
                               ),
                               selectInput(
                                 "rating_3",
                                 label = "Your Rating (0–5):",
                                 choices = seq(0, 5, by = 0.5),
                                 selected = NULL
                               ),
                               div(class = "d-flex justify-content-between mt-3",
                                   actionButton("back2", "⬅️ Back", class = "btn btn-secondary"),
                                   actionButton("next3", "Show Recommendations ✅", class = "btn btn-success")
                               )
                           )
                       )
              ),
              
              tabPanel("recommendation",
                       div(class = "container mt-5",
                           div(class = "card shadow p-4",
                               h4("Recommended Movies", class = "mb-4"),
                               withSpinner(tableOutput("recommendations"), type = 6, color = "#0d6efd"),
                               actionButton("back4", "🔄 Start Again", class = "btn btn-warning mt-4")
                           )
                       )
              )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$next1, {
    updateTabsetPanel(session, "tabs", selected = "movie_2")
  })
  
  observeEvent(input$back1, {
    updateTabsetPanel(session, "tabs", selected = "movie_1")
  })
  
  observeEvent(input$back2, {
    updateTabsetPanel(session, "tabs", selected = "movie_2")
  })
  
  observeEvent(input$back3, {
    updateTabsetPanel(session, "tabs", selected = "movie_3")
  })
  
  observeEvent(input$next2, {
    updateTabsetPanel(session, "tabs", selected = "movie_3")
  })
  
  observeEvent(input$back4, {
    updateTabsetPanel(session, "tabs", selected = "movie_1")
  })
  
  recommend_movies <- eventReactive(input$next3, {
    
    df_1 <- read.csv("data/movie_ratings.csv")
    df_1 <- df_1[,-1]
    col_names <- colnames(df_1)[-1]
    colnames(df_1)[-1] <- df_movies$movies
    
    req(input$types_text_1, input$types_text_2,input$types_text_3)
    
    col_1 = which(names(df_1) == input$types_text_1)
    col_2 = which(names(df_1) == input$types_text_2)
    col_3 = which(names(df_1) == input$types_text_3)
    
    df_2 <- df_1[!is.na(df_1[,col_1]) & !is.na(df_1[,col_2]) &
                   !is.na(df_1[,col_3]), ]
    
    
    
    user <- data.frame(userId = 0 , rating_1 = input$rating_1, rating_2 = input$rating_2,
                       rating_3 = input$rating_3)
    
    colnames(user) <- c('userId', input$types_text_1,input$types_text_2,
                        input$types_text_3)
    
    df_compare <- df_2[,c(1,col_1,col_2,col_3)]
    df_compare <- df_compare |> rbind(user)
    df_compare$cosine <- 0
    
    for (i in c(1:nrow(df_compare))) {
      df_compare[i,ncol(df_compare)] <- cosine(
        as.numeric(df_compare[i,-c(1,ncol(df_compare))]),
        as.numeric(df_compare[nrow(df_compare),-c(1,ncol(df_compare))]))
    }
    
    df_compare <- df_compare[-nrow(df_compare),]
    df_sim_user <- df_compare[which.max(df_compare[[ncol(df_compare)]]), ]
    df_sim_user_id <- df_sim_user[,1]
    
    df_optimal_row <- df_1[df_1[,1]==df_sim_user_id,]
    df_optimal_row <- df_optimal_row[,-c(1,col_1,col_2,col_3)]
    df_optimal_row <- df_optimal_row[,!is.na(df_optimal_row[1,])]
    df_optimal_row <- df_optimal_row[,df_optimal_row[1,]==5]
    rec_movie <- colnames(df_optimal_row)
    rec_movies <- data.frame(movies = rec_movie)
    rec_movies_genres <- merge(rec_movies, df_movies, 'movies', all.x = TRUE)
    rec_movies_genres <- rec_movies_genres[,-2]
    
    if(nrow(rec_movies_genres)<10){
      rec_movies_genres[c(1:nrow(rec_movies_genres)),]
    }else{
      rec_movies_genres[c(1:10),]
    }
    
    
    
    
    
  })
  
  output$recommendations <- renderTable({
    recommend_movies()
  })
  
  observeEvent(input$next3, {
    updateTabsetPanel(session, "tabs", selected = "recommendation")
  })
  
}

shinyApp(ui, server)

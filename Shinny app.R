########################################################################
## Case Study: Build Your Own Recommendation System for artists/Songs ##
########################################################################
# Data #
# The dataset used in this system contains the number of time a user listened music 
# from a particular artist. The dataset contains 92834 relations 
# between 1892 users and 17632 artists.
# OBJECTIVE #
# Predicting ratings and creating personalized recommendations for songs
## METHODOLOGY ##
# Users with similar preferences will rate items similarly.
# Missing ratings for a user can be predicted by first finding a neighborhood of similar users and 
# then aggregate the ratings of these users to form a prediction.

library(shiny)
library(tidyr)
library(recommenderlab)
library(rvest)

## Load Data
artists = read.table("Data/artists_gp4.dat",sep="\t",stringsAsFactors = F,comment.char = "",quote="",header = T)
user_artists = read.table("Data/user_artists_gp4.dat",sep="\t",header = T)

### Data Transformation
#Let's convert to wider format such that each row represent the listened count of a user.
user_artists_wide <- spread(user_artists,key=artistID,value=weight)
dim(user_artists_wide)

## Create character Id
artists$charid=paste0("I",artists$id)
userids=user_artists_wide$userID
rownames(user_artists_wide) = paste0("U",userids)
colnames(user_artists_wide) = paste0("I",colnames(user_artists_wide))
user_artists_wide$IuserID = NULL
user_artists_wide[1:6,1:10]

# Select users who has listened to at least 10 artists
num_visits=apply(user_artists_wide,1,function(x) return(sum(!is.na(x))))
user_artists_10 = user_artists_wide[num_visits>10,]
dim(user_artists_10)

# Data is centered and scaled
user_artists_10=t(scale(t(user_artists_10))[,])

# Convert visits_1k into a recommanderlab sparse matrix
user_artists_10_rrm=as(as.matrix(user_artists_10),"realRatingMatrix")
r <- user_artists_10_rrm

set.seed(100)
database=as(as.matrix(user_artists_wide),"realRatingMatrix")
model <- Recommender(r, method = "SVD")

# Function to scrape the top tracks and background image URL from the Last.fm page
scrap_url <- function(artist_url) {
  page <- read_html(artist_url)
  
  # Get top tracks (first 5)
  top_tracks <- page %>%
    html_nodes(".chartlist-name a") %>%
    html_text() %>%
    head(5)
  
  # Scrape background image URL (from the background-image CSS property)
  img_url <- page %>%
    html_nodes(".header-new-background-image") %>%
    html_attr("style") %>%
    sub(".*url\\((.*)\\).*", "\\1", .)
  
  return(list(top_tracks = top_tracks, img_url = img_url))
}

############################################### APP ################################################

ui <- fluidPage(
  titlePanel(tags$strong("Music Recommendation System", style="text-align: center;")),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("user", "Select User:", choices = rownames(r), multiple = FALSE, options = list(
        placeholder = 'Search for a user...'
      )),
      actionButton("evaluate", "🔍 Show Recommendations"),
      h5('Click on "Show Recommendations" to see the recommendations', class="black-text")
    ),
    
    mainPanel(
      uiOutput("dynamic_css"),
      h3("🎧 Recommended Artists",
         style="text-align: center; font-size: 36px; font-weight: bold; color: #FFFFFF;"),
      uiOutput("recommendations_ui")
    )
  )
)

# 🔹 Server
server <- function(input, output, session) {
  
  # Reactive expression to hold recommendations
  recommendations <- reactiveVal(NULL)
  
  # Create a reactive expression for the user input
  selected_user <- reactive({
    req(input$user)  # Ensure user is selected
    input$user       # Return the selected user
  })
  
  # Observe the button click event to generate recommendations
  observeEvent(input$evaluate, {
    req(input$user)
    
#cat("user: ",input$user, "\n")
    
    # Generate recommendations for the selected user
    recomendations <- predict(model, database[selected_user(), drop = FALSE], n = 8)
    predicted_artists <- as(recomendations, "list")[[1]]
    
  #cat("predict: ",predicted_artists, "\n")
    
    if (length(predicted_artists) == 0) {
      recommendations(NULL)  # No recommendations found
    } else {
      # Filter artist details
      recommended_artists <- artists[artists$charid %in% predicted_artists, c("name", "charid", "url")]
      
      # Ensure recommended_artists has results
      if (nrow(recommended_artists) == 0) {
        recommendations(NULL)  # No matching artists found
      } else {
        # For each recommended artist, fetch their top tracks and image URL
        top_artist_info <- lapply(recommended_artists$url, scrap_url)
        
        # Add top tracks and background image URL to the data
        recommended_artists$top_tracks <- sapply(top_artist_info, function(info){
          # Flatten the list of top tracks to a single character vector
          if (length(info$top_tracks) == 0) {
            return("No top tracks found")
          }
          return(paste(info$top_tracks, collapse = ", "))
        })
        recommended_artists$img_url <- sapply(top_artist_info, function(info) info$img_url)
        
        data=recommended_artists[1:5,]
        recommendations(data) # Save the results to the reactive value
  #print(data$name)
      }
    }
    
    
################################ visual serv ##################################
    
    
    
    # Render the recommendations UI based on the reactive value
    output$recommendations_ui <- renderUI({
      rec_artists <- recommendations()
  #print(rec_artists$name)
      
      # Check if recommendations exist
      if (is.null(rec_artists)) {
        return(h4("❌ No recommendations found!", style="color: #FFFFFF; font-weight: bold;"))
      }
      
      # Display recommended artists and their top tracks
      lapply(1:nrow(rec_artists), function(i) {
        artist <- rec_artists[i, ]
        
        fluidRow(
          column(3, 
                 img(src = artist$img_url, height = "200px", style="border-radius:10px; max-width: 100%;")),
    #cat("img: ",artist$img_url, "\n"),
          column(9,
                 h4(artist$name, style="font-weight: bold; color: #FFCC00; font-size: 24px;"),
    #cat("name: ",artist$name, "\n"),
    #cat("traks: "),
  #print(artist$top_tracks),
                 strong("Top Tracks:", class = "top-song"),
                 tags$ul(lapply(strsplit(artist$top_tracks, ",")[[1]], function(song) {
                   tags$li(style="color: #FFFFFF; text-align: left; font-size: 16px;", trimws(song))
                 }))
          ),
          hr()
        )
      })
    })
  })
  # Apply CSS styles
  output$dynamic_css <- renderUI({
    tags$style(HTML("
      .sidebar {
        background-color: #1DB954;
      }
  
      body { 
        background-color: #1DB954;
        color: white;
      }

      .top-song { 
        color: white;
        text-decoration: underline;
        text-align: left;
        font-size: 18px; 
      }
      .black-text {
        color: black !important;
      }
      .control-label {
        color: black !important;
        font-weight: bold;
      }
    "))
  })
}
shinyApp(ui = ui, server = server)


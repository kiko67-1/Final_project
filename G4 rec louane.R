# 🔹 Automatic verification and installation of required packages
packages_needed <- c("shiny", "shinyWidgets", "DT", "shinythemes", "colourpicker")

for (pkg in packages_needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

# 🔹 Loading libraries
library(shiny)
library(shinyWidgets)
library(DT)
library(shinythemes)
library(colourpicker)

# 🔹 Simulating user recommendations
Myrecommendations <- list(
  "User1" = c(292, 227, 288, 295, 498), 
  "User2" = c(227, 498, 295, 288, 292)
)

# 🔹 Artist database
artists_info <- data.frame(
  charid = c(292, 227, 288, 295, 498), 
  name = c("Paramore", "Beyoncé", "The Beatles", "Britney Spears", "Radiohead"),
  image_url = c(
    "https://th.bing.com/th/id/OIP.pLagmVNkG1fNoyiAk0p7wwHaHa?w=180&h=181&c=7&r=0&o=5&dpr=1.3&pid=1.7",
    "https://th.bing.com/th/id/OIP.I9NsKHy9AU7ymi-dxIYGEgHaIn?w=163&h=190&c=7&r=0&o=5&dpr=1.3&pid=1.7",
    "https://th.bing.com/th/id/OIP.6SO23yjQA6lB09_ILY95gwHaFc?w=230&h=180&c=7&r=0&o=5&dpr=1.3&pid=1.7",
    "https://th.bing.com/th/id/OIP.MOGy7dSEb7QsnDs2F4fKygHaI5?w=155&h=185&c=7&r=0&o=5&dpr=1.3&pid=1.7",
    "https://th.bing.com/th/id/OIP.NchFCFuzvRb0Qt7JX2vtfAHaFI?w=286&h=198&c=7&r=0&o=5&dpr=1.3&pid=1.7"
  ),
  top_songs = I(list(
    c("Misery Business", "Still Into You", "Decode", "The Only Exception", "Brick by Boring Brick"),
    c("Halo", "Crazy in Love", "Single Ladies", "Irreplaceable", "Drunk in Love"),
    c("Hey Jude", "Let It Be", "Come Together", "Yesterday", "Something"),
    c("Oops!... I Did It Again", "...Baby One More Time", "Toxic", "Gimme More", "Circus"),
    c("Creep", "Karma Police", "Paranoid Android", "High and Dry", "No Surprises")
  ))
)

# 🔹 User Interface (UI)
ui <- fluidPage(
  theme = shinytheme("cosmo"),  # 🌟 Clean theme addition
  titlePanel(tags$strong("🎵 Music Recommendation System")),  # Bold black title
  
  sidebarLayout(
    sidebarPanel(
      selectInput("user", "Select User:", choices = names(Myrecommendations)),
      actionButton("evaluate", "🔍 Show Recommendations"),
      hr()
    ),
    
    mainPanel(
      uiOutput("dynamic_css"),  # 🌟 Area for dynamic CSS
      h3("🎧 Recommended Artists", style="text-align: center; font-size: 36px; font-weight: bold; color: #FFFFFF;"),  # Centered styled white title
      uiOutput("recommendations_ui")
    )
  )
)

# 🔹 Server
server <- function(input, output, session) {
  
  output$recommendations_ui <- renderUI({
    req(input$user)
    
    user_recommendations <- Myrecommendations[[input$user]]
    
    recommended_artists <- artists_info[artists_info$charid %in% user_recommendations, ]
    
    if (nrow(recommended_artists) == 0) {
      return(h4("❌ No recommendations found!", style="color: #FFFFFF; font-weight: bold;"))
    }
    
    # 🎨 Enhanced artist display
    lapply(1:nrow(recommended_artists), function(i) {
      artist <- recommended_artists[i, ]
      
      fluidRow(
        column(3, img(src = artist$image_url, height = "100px", style="border-radius:10px;")),
        column(9,
               h4(artist$name, style="font-weight: bold; color: #FFCC00; font-size: 24px;"),  # Artist name in yellow
               strong("Top Songs:", class = "top-song"),
               tags$ul(lapply(artist$top_songs[[1]], function(song) {
                 tags$li(style="color: #FFFFFF; text-align: left; font-size: 16px;", song)  # Songs in white
               }))
        ),
        hr()
      )
    })
  })
  
  # 🎨 Apply dynamic colors with `tags$style`
  output$dynamic_css <- renderUI({
    tags$style(HTML(paste0(
      "body { ",
      "background-color: #1DB954;  /* Spotify green background */",
      "color: #FFFFFF;  /* Main text in white */",
      "}",
      ".top-song { ",
      "color: #FFFFFF;  /* White color for 'Top Songs' */",
      "text-decoration: underline; ",
      "text-align: left; ",
      "font-size: 18px; ",
      "}"
    )))
  })
}

# 🔹 Run the application
shinyApp(ui = ui, server = server)

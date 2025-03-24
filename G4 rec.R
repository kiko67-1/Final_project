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
user_artists_wide$IuserID = NULL
rownames(user_artists_wide) = paste0("U",userids)
colnames(user_artists_wide) = paste0("I",colnames(user_artists_wide))
user_artists_wide[1:6,1:10]

# Select Top 1000 
visits_byitem=colSums(user_artists_wide[,-1],na.rm = T)
visits_1k = user_artists_wide[,order(visits_byitem,decreasing = T)[1:1000]]

# Select users who has listened to at least 10 artists
num_visits=apply(visits_1k,1,function(x) return(sum(!is.na(x))))
visits_1k = visits_1k[num_visits>10,]
dim(visits_1k)

# Data is centered and scaled
visits_1k=t(scale(t(visits_1k))[,])

# Convert visits_1k into a recommanderlab sparse matrix
visits_1k_rrm=as(as.matrix(visits_1k),"realRatingMatrix")
set.seed(100)

# The matrix is converted into a realRatingMatrix object which stores the data in sparse format 
# (only non-NA values are stored explicitly; NA values are represented by a dot)
r <- visits_1k_rrm

# Understand the data better
as(r[1,], "list")
rowMeans(r[1,])
hist(getRatings(r), breaks=60)
hist(rowCounts(r), breaks=50)
hist(colMeans(r), breaks=20)

# The rating matrix can converted into a data.frame with user/item/rating tuples.
head(as(r, "data.frame"))

################################################################################
# here we have all the different methods
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
################################################################################

# We create recommenders which generates recommendations of songs
p_rr <- Recommender(r[1:700], method = "POPULAR")
ib_rr <- Recommender(r[1:700], method = "IBCF")

# Recommendations in the form of an object of class TopNList
# We create top-5 recommendation lists for 88 users who were not used to learn the model.
recom <- predict(p_rr, r[700:788], n=5)
recom
recom@items
recom@ratings

# The result contains two ordered top-N recommendation lists, one for each user. 
# The recommended items can be inspected as a list
Myrecommendations<-as(recom, "list")

for (i in 1:5){
  print(Myrecommendations[i])
  print(artists$name[artists$charid %in% Myrecommendations[[i]]])
}

# The best 3 recommendations for each list using bestN().
recom3 <- bestN(recom, n = 3)
recom3
Myrecommendations3<-as(recom3, "list")

for (i in 1:3){
  print(Myrecommendations3[i])
  print(artists$name[artists$charid %in% Myrecommendations3[[i]]])
}

### Evaluation of predicted ratings
# Evaluation of a top-N recommender algorithm
set.seed(100)
e <- evaluationScheme(r, method="cross", k=4, given=10, goodRating=1.2)

train=getData(e, "train")
test=getData(e, "known")

ub_r <- Recommender(train, method="UBCF", param=list(nn=50, normalize="center"))
p_r <- Recommender(train, method="POPULAR")
ib_r <- Recommender(train, method="IBCF", param=list(k=50, method="cosine", normalize="center"))
svd_r <- Recommender(train, method="SVD")

# Compute predicted ratings for the known part of the test data  (10 items for each
# user) using the algorithms.
p_p <- predict(p_r, test, type="ratings")
ib_p <- predict(ib_r, test, type="ratings")
ub_p <- predict(ub_r, test, type="ratings")
svd_p <- predict(svd_r, test, type = "ratings")

# Create a hybrid recommender
hybrid_r <- HybridRecommender(
  Recommender(train, method = "POPULAR"),
  Recommender(train, method = "IBCF"),
  Recommender(train, method = "UBCF"),
  weights = c(0.65, 0.25, 0.10)
)
hybrid_p <- predict(hybrid_r, test, type = "ratings")

# Hybrid + cascade aproach of Popular, IBCF and UBCF + UBCF
hcas <- (0.35 * as(p_p, "matrix") + 0.35 * as(ib_p, "matrix") + 0.3 * as(ub_p, "matrix") )
hcas_rrm <- as(hcas, "realRatingMatrix")
hcas_r <- Recommender(hcas_rrm, method="UBCF", param=list(nn=50))
hcas_p <- predict(hcas_r, test, type="ratings")

# Evaluating cascade UBCF and then IBCF on top of the UBCF
cascade_r_rrm <- as(as(p_p, "matrix"), "realRatingMatrix")
cascade_r <- Recommender(cascade_r_rrm, method="UBCF",param=list(nn=50, normalize="center"))
cascade_p <- predict(cascade_r, test, type="ratings")

# check best recommender errors
error <- rbind(
  POPULAR = calcPredictionAccuracy(p_p, getData(e, "unknown")),
  ib = calcPredictionAccuracy(ib_p, getData(e, "unknown")),
  ub = calcPredictionAccuracy(ub_p, getData(e, "unknown")),
  svd = calcPredictionAccuracy(svd_p, getData(e, "unknown")),
  hybrid = calcPredictionAccuracy(hybrid_p, getData(e, "unknown")),
  hcas = calcPredictionAccuracy(hcas_p, getData(e, "unknown")),
  CASCADE = calcPredictionAccuracy(cascade_p, getData(e, "unknown"))
)
error

##########################################################################
## We can verify that "hcas" is the best recommendation algorithm.      ##
## However the evaluate method can't evaluate a non realRatingMatrix.   ##
## Due to this problem we are excluding the "hcas" and "cascade".       ##
##########################################################################

algorithms <- list(
  POPULAR = list(name = "POPULAR", param = NULL),
  IBCF = list(name = "IBCF", param = NULL),
  UBCF = list(name = "UBCF", param = NULL),
  SVD = list(name = "SVD", param = NULL),
  HYBRID = list(name = "HYBRID", param = list(
    recommenders = list(
      POPULAR = list(name = "POPULAR", param = NULL),
      IBCF = list(name = "IBCF", param = NULL),
      UBCF = list(name = "UBCF", param = NULL)
    ),
    weights = c(0.35, 0.35, 0.3)
  ))
)
ev <- evaluate(e, algorithms, n = c(1, 3, 5, 10, 15, 20))
getConfusionMatrix(ev)[[1]]
plot(ev, annotate = TRUE)
plot(ev, "prec/rec", annotate=TRUE)



# Define UI
ui <- fluidPage(
  titlePanel("Music Recommendation System"),
    mainPanel(
      selectInput("user", "Select User:", choices = names(Myrecommendations)),
      actionButton("evaluate", "Select"),
      h3("Top 5 Item based Recommended Artists"),
      verbatimTextOutput("recommendationsIB"),
      h3("Top 5 Popularity Recommended Artists"),
      verbatimTextOutput("recommendationsP"),
      h3("Evaluation Metrics"),
      tableOutput("metrics"),
      h3("ROC Curve"),
      plotOutput("rocCurve")
    
  )
)
# Define Server
server <- function(input, output) {
  output$recommendations <- renderPrint({
    Myrecommendations[[input$user]]
  })
  
  observeEvent(input$evaluate, {
    # Evaluate Model
    e <- evaluationScheme(r, method="cross", k=4, given=10, goodRating=1.2)
    p_p <- predict(p_r, getData(e, "known"), type="ratings") #POPULAR
    ib_p <- predict(ib_r, getData(e, "known"), type="ratings") #IBCF
    error <- calcPredictionAccuracy(ib_p, getData(e, "unknown"))
    
    output$metrics <- renderTable({
      data.frame(Metric = names(error), Value = unlist(error))
    })
    
    output$rocCurve <- renderPlot({
      results <- evaluate(e, method="POPULAR", type = "topNList", n=c(1,3,5,10,15,20))
      plot(results, annotate = TRUE)
    })
  })
}

# Run App
shinyApp(ui = ui, server = server)

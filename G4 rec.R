
################################################################
## Case Study: Build Your Own Recommendation System for artists/Songs #
################################################################


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
# If X and Y are two users, the similarity between X and Y can be defined by:
# sim(X,Y)=X*Y/||X||*||Y||


### Music Recommendation System ###
## Load Data
#Read artist File
artists = read.table("Data/artists_gp4.dat",sep="\t",stringsAsFactors = F,comment.char = "",quote="",header = T)
str(artists)

user_artists = read.table("Data/user_artists_gp4.dat",sep="\t",header = T)
str(user_artists)

### Data Transformation
## Long to wide
# The data provided is in long format. Let's convert to wider format such that each row represent the listened count of a user.

library(tidyr)

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

# Select users who has listened to at least 11 artists
num_visits=apply(visits_1k,1,function(x) return(sum(!is.na(x))))
visits_1k = visits_1k[num_visits>10,]
dim(visits_1k)

# Data is centered and scaled
visits_1k=t(scale(t(visits_1k))[,])

library(recommenderlab)
# Convert visits_1k into a recommanderlab sparse matrix
visits_1k_rrm=as(as.matrix(visits_1k),"realRatingMatrix")
set.seed(100)

# The matrix is converted into a realRatingMatrix object which stores the data in sparse format 
# (only non-NA values are stored explicitly; NA values are represented by a dot)
r <- visits_1k_rrm

# Have a view to the rating sparse matrix
getRatingMatrix(r)

# Understand the data better
as(r[1,], "list")
rowMeans(r[1,])
hist(getRatings(r), breaks=60)
hist(rowCounts(r), breaks=50)
hist(colMeans(r), breaks=20)

# Convert the rating matrix into a list of users with their ratings for closer inspection
as(r, "list")

# The rating matrix can converted into a data.frame with user/item/rating tuples.
head(as(r, "data.frame"))


######################################################################################
# here we have all the different methods
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
######################################################################################



# We create a recommender which generates recommendations solely on the popularity of items (songs)
p_rr <- Recommender(r[1:600], method = "POPULAR")
ib_rr <- Recommender(r[1:600], method = "IBCF")

# Obtain information about the model
names(getModel(p_rr))
getModel(p_rr)

# Recommendations in the form of an object of class TopNList
# We create top-5 recommendation lists for 8 users who were not used to learn the model.
recom <- predict(p_rr, r[600:788], n=5)
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









# Evaluation of predicted ratings

# Evaluation of a top-N recommender algorithm
#e <- evaluationScheme(r, method="split", train=0.8, given=10, goodRating=1.2)
set.seed(100)
e <- evaluationScheme(r, method="cross", k=4, given=10, goodRating=1.2)
e


ub_r <- Recommender(getData(e, "train"), method="UBCF", param=list(method="cosine", nn=50))
p_r <- Recommender(getData(e, "train"), method="POPULAR", param=list(method="cosine", nn=50))
ib_r <- Recommender(getData(e, "train"), method="IBCF", param=list(method="cosine", nn=50))
svd_r <- Recommender(getData(e, "train"), method="SVD", param=list(method="cosine", nn=50))


# Compute predicted ratings for the known part of the test data  (10 items for each
# user) using the two algorithms.
p_p <- predict(p_r, getData(e, "known"), type="ratings")
ib_p <- predict(ib_r, getData(e, "known"), type="ratings")
ub_p <- predict(ub_r, getData(e, "known"), type="ratings")
svd_p <- predict(svd_r, getData(e, "known"), type = "ratings")


hybrid_r <- (0.6 * as(p_p, "matrix") + 0.4 * as(ib_p, "matrix") + 0.0 * as(ub_p, "matrix") )
hybrid_r_rrm <- as(hybrid_r, "realRatingMatrix")
hybrid_p <- Recommender(hybrid_r_rrm, method="UBCF", param=list(method="cosine", nn=50))
hybrid_p <- predict(hybrid_p, getData(e, "known"), type="ratings")





cascade_r_rrm <- as(as(ub_p, "matrix"), "realRatingMatrix")
cascade_r <- Recommender(cascade_r_rrm, method="IBCF", param=list(method="cosine", nn=50))
cascade_p <- predict(cascade_r, getData(e, "known"), type="ratings")
error <- rbind(
  POPULAR = calcPredictionAccuracy(p_p, getData(e, "unknown")),
  ib = calcPredictionAccuracy(ib_p, getData(e, "unknown")),
  #svd = calcPredictionAccuracy(svd_p, getData(e, "unknown")),
  hybrid = calcPredictionAccuracy(hybrid_p, getData(e, "unknown")),
  CASCADE = calcPredictionAccuracy(cascade_p, getData(e, "unknown"))
)
error
#


####### no use ################################################################
# Evaluation of a top-N recommender using the cascade approach
cascade_results <- evaluate(e, method="IBCF", type = "topNList", n=c(1,3,5,10,15,20))
# Confusion Matrix and Plots for Cascade Recommender
getConfusionMatrix(cascade_results)[[1]]
avg(cascade_results)
plot(cascade_results, annotate=TRUE)
plot(cascade_results, "prec/rec", annotate=TRUE)
##############################################################################

?evaluate






# Use the created evaluation scheme to evaluate the recommender method popular. 
# We evaluate top-1, top-3, top-5, top-10, top-15 and top-20 recommendation lists
p_results <- evaluate(e, method="POPULAR", type = "topNList", n=c(1,3,5,10,15,20))
ib_results <- evaluate(e, method="IBCF", type = "topNList", n=c(1,3,5,10,15,20))
ub_results <- evaluate(e, method="UBCF", type = "topNList", n=c(1,3,5,10,15,20))



r_inverse=t(r)
e <- evaluationScheme(r.inverse, method="cross", k=4, given=10, goodRating=1.2)
e
svd_results <- evaluate(e, method="SVD", type = "topNList", n=c(1,3,5,10,15,20))


# confusion matrices for the 1st run
# Average confusion matrices for all the 4 runs
# ROC curve for recommender
# Precision-recall plot

# Popular
getConfusionMatrix(p_results)[[1]]
avg(p_results)
plot(p_results, annotate=TRUE)
plot(p_results, "prec/rec", annotate=TRUE)

# Item based
getConfusionMatrix(ib_results)[[1]]
avg(ib_results)
plot(ib_results, annotate=TRUE)
plot(ib_results, "prec/rec", annotate=TRUE)

# 
getConfusionMatrix(ub_results)[[1]]
avg(ub_results)
plot(svd_results, annotate=TRUE)
plot(ub_results, "prec/rec", annotate=TRUE)



library(shiny)
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


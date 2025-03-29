########################################################################
## Case Study: Build Your Own Recommendation System for artists/Songs ##
########################################################################
# Data #
# The dataset used in this system contains the number of time a user listened music 
# from a particular artist. The dataset contains 92834 relations 
# between 1892 users and 17632 artists.
# OBJECTIVE #
# Predicting ratings and creating personalized recommendations for songs

library(shiny)
library(tidyr)
library(recommenderlab)
library(rvest)
library(bigstatsr)

## Load Data
artists = read.table("Data/artists_gp4.dat",sep="\t",stringsAsFactors = F,comment.char = "",quote="",header = T)
user_artists = read.table("Data/user_artists_gp4.dat",sep="\t",header = T)

### Data Transformation
# Convert to wide format where each row represents the listened count of a user
user_artists_wide <- spread(user_artists,key=artistID,value=weight)
dim(user_artists_wide)

## Create character Id
artists$charid=paste0("I",artists$id)
userids=user_artists_wide$userID
rownames(user_artists_wide) = paste0("U",userids)
colnames(user_artists_wide) = paste0("I",colnames(user_artists_wide))
user_artists_wide$IuserID = NULL
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

### Evaluation of predicted ratings
# Evaluation of a top-N recommender algorithm
set.seed(100)
e <- evaluationScheme(r, method="cross", k=4, given=10, goodRating=1.2)

train=getData(e, "train")
test=getData(e, "known")

ub_r <- Recommender(train, method="UBCF", param=list(nn=50, normalize="center"))
p_r <- Recommender(train, method="POPULAR")
svd_r <- Recommender(train, method="SVD")
svdf_r <- Recommender(train, method="SVDF")
als_r <- Recommender(train, method="ALS")
alsi_r <- Recommender(train, method="ALS_implicit")

# Compute predicted ratings for the known part of the test data  (10 items for each
# user) using the algorithms.
p_p <- predict(p_r, test, type="ratings")
ub_p <- predict(ub_r, test, type="ratings")
svd_p <- predict(svd_r, test, type = "ratings")
svdf_p <- predict(svdf_r, test, type = "ratings")
als_p <- predict(als_r, test, type = "ratings")
alsi_p <- predict(alsi_r, test, type = "ratings")

set.seed(100)
# Create a hybrid recommender
hybrid_r <- HybridRecommender(
  Recommender(train, method = "POPULAR"),
  Recommender(train, method = "UBCF"),
  Recommender(train, method = "SVD"),
  weights = c(0.8, 0.2, 0.0)
)
hybrid_p <- predict(hybrid_r, test, type="ratings")

set.seed(100)
# Hybrid + cascade aproach of Popular, IBCF and UBCF + UBCF
hcas <- (0.8 * as(p_p, "matrix") + 0.2 * as(ub_p, "matrix") )
hcas_rrm <- as(hcas, "realRatingMatrix")
hcas_r <- Recommender(hcas_rrm, method="UBCF", param=list(nn=50))
hcas_p <- predict(hcas_r, test, type="ratings")

# check best recommender errors
error <- rbind(
  POPULAR = calcPredictionAccuracy(p_p, getData(e, "unknown")),
  #ub = calcPredictionAccuracy(ub_p, getData(e, "unknown")),
  svd = calcPredictionAccuracy(svd_p, getData(e, "unknown")),
  svdf = calcPredictionAccuracy(svdf_p, getData(e, "unknown")),
  als = calcPredictionAccuracy(als_p, getData(e, "unknown")),
  #alsi = calcPredictionAccuracy(alsi_p, getData(e, "unknown")),
  hybrid = calcPredictionAccuracy(hybrid_p, getData(e, "unknown"))#,
  #hcas = calcPredictionAccuracy(hcas_p, getData(e, "unknown"))
)
error

set.seed(100)
# re doing an hybrid recommender for best ROC curve
HYBRID <- list( name = "HYBRID", param = list( recommenders = list(
  POPULAR = list(name = "POPULAR", param = NULL),
  UBCF = list(name = "UBCF", param = NULL),
  SVD = list(name = "SVD", param = NULL)
),
weights = c(0.8, 0.2, 0.0),
aggregation_type = "sum"   
))

# checking best recommenders 
algorithms <- list(
  POPULAR = list(name = "POPULAR", param = NULL),
  #UB = list(name = "UBCF", param = NULL),
  SVD = list(name = "SVD", param = NULL),
  SVDF = list(name = "SVDF", param = NULL),
  ALS = list(name = "ALS", param = NULL),
  #ALSI = list(name = "ALS_implicit", param = NULL),
  HYBRID = HYBRID
)
all_results <- evaluate(e, algorithms, n = c(1,50,100,200,300,500,700,1000))

# ROC curve
plot(all_results,"ROC", annotate = TRUE)
plot(all_results, "prec/rec", annotate = TRUE)
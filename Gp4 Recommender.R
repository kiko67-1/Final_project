library(recommenderlab)
library(tidyr)
library(dplyr)

# Load the data
artists <- read.table("DAta/artists_gp4.dat", sep="\t", header=TRUE, stringsAsFactors=FALSE)
user_artists <- read.table("Data/user_artists_gp4.dat", sep="\t", header=TRUE)

# Convert data from long to wide format
user_artists_wide <- spread(user_artists, key=artistID, value=weight)

# Create user IDs and artist IDs
rownames(user_artists_wide) <- paste0("U", user_artists_wide$userID)
user_artists_wide$userID <- NULL
colnames(user_artists_wide) <- paste0("I", colnames(user_artists_wide))

# Normalize the listening counts (scale the data)
user_artists_wide <- scale(user_artists_wide)

# Convert to a recommender matrix
visits_matrix <- as(as.matrix(user_artists_wide), "realRatingMatrix")

# Train a User-Based Collaborative Filtering Model (UBCF) with Cosine Similarity
set.seed(123)
ubcf_model <- Recommender(visits_matrix, method="UBCF", param=list(method="Cosine"))

# Generate top 5 artist recommendations for 10 users
recommendations <- predict(ubcf_model, visits_matrix[1:10], n=5)

# Convert recommendations to a readable format
Myrecommendations <- as(recommendations, "list")

# Print recommendations
for (i in 1:5) {
  print(paste("User", i, "Recommendations:"))
  print(artists$name[artists$id %in% as.numeric(sub("I", "", Myrecommendations[[i]]))])
}

# Evaluate the recommender model using a train-test split
evaluation_scheme <- evaluationScheme(visits_matrix, method="split", train=0.8, given=10, goodRating=1.2)
ubcf_eval <- Recommender(getData(evaluation_scheme, "train"), "UBCF", param=list(method="Cosine"))
predictions <- predict(ubcf_eval, getData(evaluation_scheme, "known"), type="ratings")

# Calculate prediction error
error <- calcPredictionAccuracy(predictions, getData(evaluation_scheme, "unknown"))
print(error)

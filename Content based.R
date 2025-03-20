
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


## Load Required Libraries
library(tidyr)
library(dplyr)
library(tm)  # Text Mining for TF-IDF
library(proxy)  # Similarity calculations
library(pROC)  # ROC Curve
library(caret)  # Confusion Matrix

## Load Data
artists <- read.table("Data/artists_gp4.dat", sep="\t", stringsAsFactors=F, header=T)
user_artists <- read.table("Data/user_artists_gp4.dat", sep="\t", header=T)

## Assume artists have a 'tags' column with textual metadata (or create one for demo purposes)
artists$tags <- paste("rock pop alternative jazz classical electronic hiphop", sample(letters, nrow(artists), replace=T))

## 1. Create TF-IDF Matrix
corpus <- Corpus(VectorSource(artists$tags))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Convert to Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus)
tfidf_matrix <- weightTfIdf(dtm)

## 2. Compute Cosine Similarity
similarity_matrix <- as.matrix(proxy::dist(as.matrix(tfidf_matrix), method="cosine"))
rownames(similarity_matrix) <- artists$name
colnames(similarity_matrix) <- artists$name

## 3. Function to Recommend Similar Artists
recommend_artists <- function(artist_name, top_n=5) {
  if (!(artist_name %in% rownames(similarity_matrix))) {
    return("Artist not found in dataset")
  }
  
  artist_sim <- similarity_matrix[artist_name, ]
  recommended <- sort(artist_sim, decreasing=FALSE)[2:(top_n+1)]  # Exclude self
  return(names(recommended))
}

## 4. Evaluation - Confusion Matrix, ROC Curve, Precision-Recall
# Creating a dummy ground truth and prediction for evaluation
set.seed(123)
ground_truth <- sample(c(1, 0), nrow(similarity_matrix), replace=TRUE)  # 1: relevant, 0: not relevant
predicted_scores <- runif(nrow(similarity_matrix))  # Random scores as predictions
predicted_labels <- ifelse(predicted_scores > 0.5, 1, 0)

# Confusion Matrix
conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(ground_truth))
print(conf_matrix)

# ROC Curve
roc_curve <- roc(ground_truth, predicted_scores)
plot(roc_curve, main="ROC Curve for Artist Recommendations")
auc(roc_curve)

# Precision-Recall Curve
precision <- posPredValue(as.factor(predicted_labels), as.factor(ground_truth))
recall <- sensitivity(as.factor(predicted_labels), as.factor(ground_truth))
plot(recall, precision, type="b", xlab="Recall", ylab="Precision", main="Precision-Recall Curve")

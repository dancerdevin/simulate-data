# Machine learning on Taylor Swift songs
# Devin Fitzpatrick
# devinfitzpatrick@lclark.edu
# 2024-06-27

library(randomForest)
library(dplyr)

# Read in data
swift_data <- read.csv("data/swift-data.csv")
# Show the first six rows of data
head(swift_data)

# Run a linear model with all predictors
linear_model <- lm(peak_position ~ ., data = swift_data)

# Run a random forest model with all predictors
rf_model <- randomForest(peak_position ~ ., data = swift_data)

# Create fold vector
fold <- rep_len(x = 1:5, length.out = nrow(swift_data))

linear_rmse <- numeric(5)
rf_rmse <- numeric(5)

for (i in 1:5) {
  # Split data into training/testing
  training <- swift_data %>%
    filter(fold != i)
  testing <- swift_data %>%
    filter(fold == i)
  
  # Estimate model with training data
  linear_model <- lm(peak_position ~ ., data = training)
  rf_model <- randomForest(peak_position ~ ., data = training)
  
  # Make predictions on testing data
  linear_prediction <- predict(linear_model, newdata = testing)
  rf_prediction <- predict(rf_model, newdata = testing)
  
  # Evaluate models with RMSE
  linear_sqerr <- (testing$peak_position - linear_prediction)^2
  linear_rmse[i] <- sqrt(mean(linear_sqerr))
  
  rf_sqerr <- (testing$peak_position - rf_prediction)^2
  rf_rmse[i] <- sqrt(mean(rf_sqerr))
}

mean(linear_rmse)
mean(rf_rmse)

full_model <- randomForest(peak_position ~ ., data = swift_data)

# load unreleased album into R
new_album <- read.csv("data/swift-new.csv")
head(new_album)

# Predict position excluding track names
new_predict <- predict(full_model, newdata = new_album[, -1])

# Format predictions for printing
song_predict <- data.frame(track_num = 1:12,
                           peak_position = new_predict,
                           track_name = new_album$track_names)
song_predict
peak_position <- song_predict$peak_position
reordered_song_predict <- song_predict[order(peak_position),]
reordered_song_predict
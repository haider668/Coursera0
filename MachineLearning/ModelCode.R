library(dplyr)
library(caret)
library(randomForest)

# Load the data
training_set <- read.csv("pml-training.csv")
final_test <- read.csv("pml-testing.csv")

# Data preprocessing

# Remove columns with more than 90% missing values
NAs_col <- as.data.frame(colSums(is.na(training_set)) / nrow(training_set))
colnames(NAs_col) <- "Proportion_NA"
kept_cols <- rownames(subset(NAs_col, Proportion_NA > 0.9))
training_set <- training_set %>%
  select(-one_of(kept_cols))
final_test <- final_test %>%
  select(-one_of(kept_cols))

# Remove columns with empty cells
empty_cells_df <- data.frame(
  "Column Name" = names(training_set),
  "Empty Cell Count" = sapply(training_set, function(x) sum(x == "")),
  stringsAsFactors = FALSE
)
kept_cols <- rownames(subset(empty_cells_df, `Empty.Cell.Count` > 0))
training_set <- training_set %>%
  select(-one_of(kept_cols))
final_test <- final_test %>%
  select(-one_of(kept_cols))

# Remove unnecessary columns
training_set <- training_set %>%
  select(-matches("^X|timestamp|user_name"))
final_test <- final_test %>%
  select(-matches("^X|timestamp|user_name"))

# Remove rows with any NA values
training_set <- na.omit(training_set)



# Split the data into training and test sets (70% train, 30% test)
set.seed(345)
inTrain <- createDataPartition(training_set$classe, p = 0.70, list = FALSE)
train <- training_set[inTrain, ]
test <- training_set[-inTrain, ]

train$classe <- as.factor(train$classe)
test$classe <- as.factor(test$classe)

# Implement cross-validation with trainControl
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Fit a Random Forest model with cross-validation
rf_model_cv <- train(classe ~ ., data = train, method = "rf", trControl = ctrl)


# Predict on the test set
predictions <- predict(rf_model_cv, newdata = test)

# Calculate accuracy
conf_matrix <- confusionMatrix(predictions, test$classe)
accuracy <- conf_matrix$overall['Accuracy']

# Print the confusion matrix and accuracy
print(conf_matrix)
print(accuracy)

# Ensure the problem_id column is kept for reference
final_test_ids <- final_test$problem_id

# Remove the problem_id column for prediction
final_test_no_id <- final_test %>%
  select(-problem_id)

# Make predictions using the trained model (rf_model_cv)
final_predictions <- predict(rf_model_cv, newdata = final_test_no_id)

# Combine predictions with the problem_id
submission <- data.frame(problem_id = final_test_ids, predicted_class = final_predictions)

# View the submission data
print(submission)

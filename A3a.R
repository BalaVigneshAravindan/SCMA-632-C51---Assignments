install.packages("caret")
install.packages("e1071")
install.packages("rpart")
install.packages("pROC")
install.packages("glmnet")

library(caret)
library(e1071)
library(rpart)
library(pROC)
library(glmnet)

# Load the dataset
data <- read.csv("C:\\Users\\Bala Vignesh.A\\Desktop\\SCMA 632\\HR_DataSet.csv")

# Display the dataset
print(head(data))

# Define the categorical columns and apply encoding
data$Department <- as.factor(data$Department)
data$salary <- as.factor(data$salary)
data$promotion_last_5years <- as.factor(data$promotion_last_5years)

# Convert the target variable to a factor for classification
data$left <- as.factor(data$left)

# Split the dataset into features (X) and target (y)
X <- data[, !names(data) %in% c("left")]
y <- data$left

# Split the data into training and validation sets
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_val <- X[-trainIndex, ]
y_val <- y[-trainIndex]

# Define the training control
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)

# Define the tuning grid for logistic regression
grid_log_reg <- expand.grid(
  alpha = 0,
  lambda = 10^seq(-4, 4, length = 10)
)

# Load the forcats package
library(forcats)

# Rename the factor levels in the target variable
levels(y_train) <- c("No", "Yes")

# Train the logistic regression model
log_reg_model <- train(
  left ~ ., data = cbind(X_train, left = y_train),
  method = "glmnet",
  trControl = train_control,
  tuneGrid = grid_log_reg,
  metric = "ROC",
  family = "binomial"
)

# Predict probabilities on the validation data
y_pred_proba_log_reg <- predict(log_reg_model, X_val, type = "prob")[, 2]
y_pred_log_reg <- ifelse(y_pred_proba_log_reg >= 0.5, 1, 0)

# Confusion Matrix
conf_mat_log_reg <- confusionMatrix(as.factor(y_pred_log_reg), y_val)
print("Confusion Matrix (Logistic Regression):")
print(conf_mat_log_reg)

# AUC
roc_log_reg <- roc(y_val, y_pred_proba_log_reg)
auc_log_reg <- auc(roc_log_reg)
print(paste("AUC (Logistic Regression):", round(auc_log_reg, 2)))

# Define the tuning grid for decision tree
grid_dt <- expand.grid(
  cp = seq(0.01, 0.1, by = 0.01)
)

# Train the decision tree model
dt_model <- train(
  left ~ ., data = cbind(X_train, left = y_train),
  method = "rpart",
  trControl = train_control,
  tuneGrid = grid_dt,
  metric = "ROC"
)

# Predict probabilities on the validation data
y_pred_proba_dt <- predict(dt_model, X_val, type = "prob")[, 2]
y_pred_dt <- ifelse(y_pred_proba_dt >= 0.5, 1, 0)

# Confusion Matrix
conf_mat_dt <- confusionMatrix(as.factor(y_pred_dt), y_val)
print("Confusion Matrix (Decision Tree):")
print(conf_mat_dt)

# AUC
roc_dt <- roc(y_val, y_pred_proba_dt)
auc_dt <- auc(roc_dt)
print(paste("AUC (Decision Tree):", round(auc_dt, 2)))

# Plot the ROC curve for both models
plot(roc_log_reg, col = "blue", main = "ROC Curve")
plot(roc_dt, col = "red", add = TRUE)
legend("bottomright", legend = c("Logistic Regression", "Decision Tree"), col = c("blue", "red"), lty = 1)

# Logistic Regression Metrics
print("Logistic Regression Metrics:")
log_reg_metrics <- data.frame(
  Accuracy = conf_mat_log_reg$overall['Accuracy'],
  Precision = conf_mat_log_reg$byClass['Pos Pred Value'],
  Recall = conf_mat_log_reg$byClass['Sensitivity'],
  F1 = 2 * ((conf_mat_log_reg$byClass['Pos Pred Value'] * conf_mat_log_reg$byClass['Sensitivity']) / (conf_mat_log_reg$byClass['Pos Pred Value'] + conf_mat_log_reg$byClass['Sensitivity']))
)
print(log_reg_metrics)

# Decision Tree Metrics
print("Decision Tree Metrics:")
dt_metrics <- data.frame(
  Accuracy = conf_mat_dt$overall['Accuracy'],
  Precision = conf_mat_dt$byClass['Pos Pred Value'],
  Recall = conf_mat_dt$byClass['Sensitivity'],
  F1 = 2 * ((conf_mat_dt$byClass['Pos Pred Value'] * conf_mat_dt$byClass['Sensitivity']) / (conf_mat_dt$byClass['Pos Pred Value'] + conf_mat_dt$byClass['Sensitivity']))
)
print(dt_metrics)


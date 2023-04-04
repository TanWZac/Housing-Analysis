rm(list = ls())
setwd("C:/Users/twz18/Downloads/Housing-Analysis")
data = read.csv("Housing.csv", header=TRUE)
data

summary(data)
str(data)

library(caret)
train_index <- createDataPartition(data$price, p = 0.8, list= FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

library(randomForest)

randomForest_model <- randomForest(price ~ ., data = train_data)
predictions <- predict(randomForest_model, newdata = test_data)
plot(lm(predictions~test_data$price))


library(ggplot2)

# Create a dataframe with the test data and predictions
df <- data.frame(test_data$price, predictions)

# Plot the data with a linear regression line
ggplot(df, aes(x = test_data.price, y = predictions)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

df <- data.frame(actual_price = test_data$price,
                 predicted_price = predictions)

ggplot(df, aes(x = actual_price, y = predicted_price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Actual Price", y = "Predicted Price", 
       title = "Correlation Plot of Actual vs Predicted Prices")


df <- data.frame(x = test_data$price, y = predictions)

# Create a heatmap using ggplot2
ggplot(df, aes(x, y)) +
  geom_bin2d(bins = 20) +
  scale_fill_gradient(low = "white", high = "red") +
  xlab("Actual Price") +
  ylab("Predicted Price") +
  ggtitle("Heatmap of Actual vs Predicted Prices")

library(xgboost)
# this is for one-hot encoding
binary_form = function(data, col, yes_val = "yes", no_val = "no") {
  data[[col]] = ifelse(data[[col]] == yes_val, 1, ifelse(data[[col]]== no_val, 0, NA))
  return(data)
}

data = binary_form(data, "mainroad")
data = binary_form(data, "guestroom")
data = binary_form(data, "basement")
data = binary_form(data, "hotwaterheating")
data = binary_form(data, "airconditioning")
data = binary_form(data, "prefarea")
data$furnishingstatus = factor(data$furnishingstatus, levels= c("unfurnished", "semi-furnished", "furnished"))
furnishings <- data.frame(model.matrix(~furnishingstatus, data=data)[,-1])

# Remove the original furnishingstatus column
data <- data[, !names(data) %in% "furnishingstatus"]

# Combine the original dataframe with the one-hot encoded columns
data <- cbind(data, furnishings)
summary(data)

train_index <- sample(1:nrow(train_data), 0.7*nrow(train_data))
train_data_xgb <- train_data[train_index, ]
test_data_xgb <- train_data[-train_index, ]

# Define the target variable and the predictors
y_train <- train_data_xgb$price
X_train <- as.matrix(train_data_xgb[, -which(names(train_data_xgb) == "price")])
y_test <- test_data_xgb$price
X_test <- as.matrix(test_data_xgb[, -which(names(test_data_xgb) == "price")])

# Fit the XGBoost model
model_xgb <- xgboost(data = X_train, label = y_train, nrounds = 100, objective = "reg:squarederror")

# Make predictions on the test set
predictions_xgb <- predict(model_xgb, newdata = X_test)

# Evaluate the model
mse_xgb <- mean((predictions_xgb - y_test)^2)
rmse_xgb <- sqrt(mse_xgb)
cat("Root Mean Squared Error:", rmse_xgb, "\n")

# Create confusion matrix
conf_mat_xgb <- table(round(predictions_xgb/1000000), round(y_test/1000000))
conf_mat_xgb

library(ggplot2)

ggplot(data = data.frame(y_test, predictions_xgb), aes(x = y_test, y = predictions_xgb)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("XGBoost Regression Plot") +
  xlab("Actual Price (millions)") + 
  ylab("Predicted Price (millions)")

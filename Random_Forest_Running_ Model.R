## Random Forest Model for predicting mountain running times based on personal 
## Running data
## Code Developed by Diego Schillaci for GIS 335 Final Project, Spring 2025

#Load Packages

library(tidyverse)
library(ranger)
library(ggplot2)


# Import and Prep data for model. 
# In R, each Column needs to be made into an 'object' that can then be used for analysis

run_data <- read_csv("GIS_Data_Final.csv")

model_data <- run_data %>%
  transmute(
    AvgHR = `Avg_Hr(bpm)`,
    Distance = `Distance(mi)`,
    ElevationGain = `Elev_gain(ft)`,
    MovingTime = `Moving_Time(Min)`,
    Terrain = `Terrain_Score(1-5)`,
    ElevFtPerMi = `Elev_gain(ft)` / `Distance(mi)`,
    ElevFtPerMin = `Elev_gain(ft)` / `Moving_Time(Min)`
  ) %>%
  drop_na()

# Split prepped data into training and testing
# ~80% of the data is used for Training and ~20% is used for Testing

set.seed(42)
split_index <- sample(1:nrow(model_data), size = 0.8 * nrow(model_data))
train_data <- model_data[split_index, ]
test_data <- model_data[-split_index, ]

# Train the Random Forest Model using the 'Ranger' Package
rf_model <- ranger(
  formula = MovingTime ~ Distance + ElevationGain + AvgHR + Terrain + ElevFtPerMi + ElevFtPerMin,
  data = train_data,
  num.trees = 500,
  mtry = 2,  
  min.node.size = 8,  
  importance = "impurity",
  seed = 42
)

# Examine variable importance (The higher the # the more important the variable is to
# predicting the results in the model)

print(rf_model$variable.importance)

importance_df <- as.data.frame(rf_model$variable.importance)
colnames(importance_df) <- "Importance"
importance_df$Variable <- rownames(importance_df)

importance_df <- importance_df %>%
  arrange(desc(Importance))

# Simple Plot of Variable Importance
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Model Variable Importance",
    x = "Variable",
    y = "Variable Importance"
  ) +
  theme_minimal()


# Predict on testing data (Resuduals Aka Actuals removed)
preds <- predict(rf_model, data = test_data)$predictions
actuals <- test_data$MovingTime

# Evaluate Error (RMSE & r^2)
rmse <- sqrt(mean((preds - actuals)^2))
r_squared <- cor(preds, actuals)^2
cat("RMSE:", round(rmse, 2), "minutes\n")
cat("R²:", round(r_squared, 3), "\n")

#Predict total moving time of a route based on the trained model
#Input metrics from the desired route and terrain score
predict_moving_time <- function(distance_mi, elev_gain_ft, avg_hr_bpm, terrain_score = 4) {
  
  elev_per_mile <- elev_gain_ft / distance_mi
  estimated_time <- distance_mi * 17
  elev_per_min <- elev_gain_ft / estimated_time
  
  new_data <- data.frame(
    Distance = distance_mi,
    ElevationGain = elev_gain_ft,
    AvgHR = avg_hr_bpm,
    Terrain = terrain_score,
    ElevFtPerMi = elev_per_mile,
    ElevFtPerMin = elev_per_min
  )
  
# Predicts moving time in minutes using the trained model based on - 
# Distance in Miles, Elevation gain and desired AVG heart rate!
  
  predicted_time <- predict(rf_model, data = new_data)$predictions
  return(round(predicted_time, 1))
}

predict_moving_time(distance_mi = 12, elev_gain_ft = 4005, avg_hr_bpm = 142)


# Plot the Results of the model with residuals and Error

results_df <- data.frame(Actual = actuals, Predicted = preds)

ggplot(results_df, aes(x = Actual, y = Predicted)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  annotate("text", x = min(actuals) + 10, y = max(preds) - 10,
           label = paste("RMSE (minutes):", round(rmse, 1), "\nR²:", round(r_squared, 3)),
           hjust = 0, size = 4.5, fontface = "italic") +
  labs(title = "Model Predicted vs. Actual Moving Time",
       x = "Actual Moving Time (min)", y = "Predicted Moving Time (min)") +
  theme_minimal() +
  coord_equal()


# Create a data frame with calculated residual results
# this is done so we can actually plot the values the model gave us

results_df <- data.frame(
  Actual = test_data$MovingTime,
  Predicted = predict(rf_model, data = test_data)$predictions,
  Distance = test_data$Distance
)

results_df <- results_df %>%
  mutate(Error = Predicted - Actual,
         AbsError = abs(Error))

# Plot Absolute Error vs. Distance
# this shows us at what distances the model is most accurate 

ggplot(results_df, aes(x = Distance, y = AbsError)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "red") +
  labs(
    title = "Absolute Prediction Error vs. Run Distance",
    x = "Distance (mi)",
    y = "Absolute Error (min)"
  ) +
  theme_minimal()

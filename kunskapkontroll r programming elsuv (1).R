# Load required libraries
library(tidyverse)
library(caret)
library(readxl) # For reading Excel files

# Read the data
# Set the working directory to the directory where your Excel file is located
setwd("C:/Users/Imthias/Downloads/r program/")
# Read the dataset
data <- read_excel("el_suv_data_uppsamling.xlsx")
str(data)

# Summary och Dataset Printout
summary(data)
# Data preprocessing
# Check for missing values
sum(is.na(data))

# Load required libraries
library(ggplot2)
library(tidyr)


# EDA Visualizations

# Histograms for numeric variables
par(mfrow = c(1, 3))
hist(data$Modellår, main = "Modellår Histogram", xlab = "Modellår", ylab = "Frequency")
hist(data$Miltal, main = "Miltal Histogram", xlab = "Miltal", ylab = "Frequency")
hist(data$Hästkrafter, main = "Hästkrafter Histogram", xlab = "Hästkrafter", ylab = "Frequency")


# Check for outliers
boxplot(data$Pris)
# Identify outliers
outliers <- boxplot(data$Pris, plot = FALSE)$out

# Print outliers
outliers
# Check for outliers
boxplot(data$Pris)

# Winsorize outliers
winsorize <- function(x, trim = 0.05) {
  q <- quantile(x, probs = c(trim, 1 - trim), na.rm = TRUE)
  x[x > q[2]] <- q[2]  # Replace values above the 95th percentile
  x
}

# Apply Winsorization to the 'Pris' variable
data$Pris <- winsorize(data$Pris)

# Re-check for outliers
boxplot(data$Pris)

# Define a function to identify outliers
identify_outliers <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  outliers <- x[x < lower_bound | x > upper_bound]
  outliers
}

# Identify outliers for each feature variable
feature_variables <- c("Modellår", "Miltal", "Hästkrafter")
for (variable in feature_variables) {
  cat("Outliers for", variable, ":\n")
  outliers <- identify_outliers(data[[variable]])
  print(outliers)
}


# Winsorize outliers for Miltal variable
winsorize_miltal <- function(x, trim = 0.05) {
  q <- quantile(x, probs = c(trim, 1 - trim), na.rm = TRUE)
  x[x < q[1]] <- q[1]  # Replace values below the 5th percentile
  x[x > q[2]] <- q[2]  # Replace values above the 95th percentile
  x
}

# Apply Winsorization to the 'Miltal' variable
data$Miltal <- winsorize_miltal(data$Miltal)

# Winsorize outliers for Hästkrafter variable
winsorize_hastkrafter <- function(x, trim = 0.05) {
  q <- quantile(x, probs = c(trim, 1 - trim), na.rm = TRUE)
  x[x > q[2]] <- q[2]  # Replace values above the 95th percentile
  x
}

# Apply Winsorization to the 'Hästkrafter' variable
data$Hästkrafter <- winsorize_hastkrafter(data$Hästkrafter)

# Re-check for outliers
boxplot(data$Miltal)
boxplot(data$Hästkrafter)
# Explore relationships between variables
cor(data[, c("Modellår", "Miltal", "Hästkrafter", "Pris")])  # Correlation matrix
pairs(data[, c("Modellår", "Miltal", "Hästkrafter", "Pris")]) # Pairplot


# Build linear regression model
model <- lm(Pris ~ Modellår + Miltal + Hästkrafter, data = data)

# Print model summary
summary(model)

# Plot diagnostics
plot(model)

# Assess residual plots
par(mfrow = c(2, 2))
plot(model)

# Model evaluation using cross-validation
cv_model <- train(Pris ~ Modellår + Miltal + Hästkrafter, data = data, method = "lm", trControl = trainControl(method = "cv"))

# Print cross-validated RMSE
print(paste("Cross-validated RMSE:", sqrt(cv_model$results$RMSE)))

# Making predictions
predictions <- predict(model, newdata = data)

# Add predictions to the original dataset
data$Predicted_Pris <- predictions

# Visualize actual vs. predicted prices
plot(data$Pris, predictions, main = "Actual vs. Predicted Prices",
     xlab = "Actual Prices", ylab = "Predicted Prices", col = "blue")
abline(0, 1, col = "red")

# Residual plot
residuals <- residuals(model)
plot(predictions, residuals, main = "Residual Plot",
     xlab = "Predicted Prices", ylab = "Residuals", col = "green")



# Explore interactions between predictor variables
data$Modellår_Miltal <- data$Modellår * data$Miltal
data$Modellår_Hästkrafter <- data$Modellår * data$Hästkrafter
data$Miltal_Hästkrafter <- data$Miltal * data$Hästkrafter

# Build a new model with interaction terms
model_interaction <- lm(Pris ~ Modellår + Miltal + Hästkrafter + Modellår_Miltal + Modellår_Hästkrafter + Miltal_Hästkrafter, data = data)

# Print model summary
summary(model_interaction)

# Diagnostic plots for the model with interaction terms
par(mfrow = c(2, 2))

# 1. Residuals vs. Fitted plot
plot(model_interaction, which = 1)

# 2. Normal Q-Q plot
plot(model_interaction, which = 2)

# 3. Scale-Location plot (Square root of standardized residuals vs. fitted values)
plot(model_interaction, which = 3)

# 4. Residuals vs. Leverage plot
plot(model_interaction, which = 5)


# Assess model fit and diagnostic plots
plot(model_interaction)
par(mfrow = c(2, 2))
plot(model_interaction)

# Model evaluation using cross-validation
cv_model_interaction <- train(Pris ~ Modellår + Miltal + Hästkrafter + Modellår_Miltal + Modellår_Hästkrafter + Miltal_Hästkrafter, 
                              data = data, method = "lm", trControl = trainControl(method = "cv"))

# Print cross-validated RMSE
print(paste("Cross-validated RMSE (with interaction terms):", sqrt(cv_model_interaction$results$RMSE)))

# Try alternative regression techniques (e.g., polynomial regression)
# Build polynomial regression models
model_poly <- lm(Pris ~ poly(Modellår, 2) + poly(Miltal, 2) + poly(Hästkrafter, 2), data = data)

# Print model summary
summary(model_poly)

# Model evaluation using cross-validation
cv_model_poly <- train(Pris ~ poly(Modellår, 2) + poly(Miltal, 2) + poly(Hästkrafter, 2), 
                       data = data, method = "lm", trControl = trainControl(method = "cv"))

# Print cross-validated RMSE
print(paste("Cross-validated RMSE (polynomial regression):", sqrt(cv_model_poly$results$RMSE)))

# Explore additional features or transformations
# Try log-transforming the target variable
data$log_Pris <- log(data$Pris)

# Build a model with the log-transformed target variable
model_log <- lm(log_Pris ~ Modellår + Miltal + Hästkrafter, data = data)

# Print model summary
summary(model_log)

# Model evaluation using cross-validation
cv_model_log <- train(log_Pris ~ Modellår + Miltal + Hästkrafter, data = data, method = "lm", trControl = trainControl(method = "cv"))

# Print cross-validated RMSE
print(paste("Cross-validated RMSE (with log-transformed target variable):", sqrt(cv_model_log$results$RMSE)))

abline(h = 0, col = "red")



# Making predictions for the linear regression model
predictions <- predict(model, newdata = data)
# Add predictions to the original dataset
data$Predicted_Pris <- predictions

# Making predictions for the model with interaction terms
predictions_interaction <- predict(model_interaction, newdata = data)
# Add predictions to the original dataset
data$Predicted_Pris_Interaction <- predictions_interaction

# Making predictions for the polynomial regression model
predictions_poly <- predict(model_poly, newdata = data)
# Add predictions to the original dataset
data$Predicted_Pris_Poly <- predictions_poly

# Making predictions for the model with log-transformed target variable
predictions_log <- exp(predict(model_log, newdata = data))
# Add predictions to the original dataset
data$Predicted_Pris_Log <- predictions_log

# Set up a multi-paneled plot
par(mfrow = c(2, 2))

# Quantile density plot of actual and predicted values for linear regression model
plot(density(data$Pris), main = "Linear Regression", xlab = "Price", col = "blue")
lines(density(predictions), col = "red")

# Quantile density plot of actual and predicted values for model with interaction terms
plot(density(data$Pris), main = "Model with Interaction Terms", xlab = "Price", col = "blue")
lines(density(predictions_interaction), col = "red")

# Quantile density plot of actual and predicted values for polynomial regression model
plot(density(data$Pris), main = "Polynomial Regression", xlab = "Price", col = "blue")
lines(density(predictions_poly), col = "red")

# Quantile density plot of actual and predicted values for model with log-transformed target variable
plot(density(data$Pris), main = "Log-transformed Model", xlab = "Price", col = "blue")
lines(density(predictions_log), col = "red")
########################################################################

install.packages("httr")
install.packages("jsonlite")
install.packages("openxlsx")

# Load required libraries
library(jsonlite)
library(readxl)
library(ggplot2)

# Send a request to SCB API and receive JSON response
url <- "https://api.scb.se/OV0104/v1/doris/en/ssd/START/TK/TK1001/TK1001A/PersBilarDrivMedel"
query <- '{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "item",
        "values": [
          "12",
          "1280",
          "1281",
          "1283"
        ]
      }
    },
    {
      "code": "Drivmedel",
      "selection": {
        "filter": "item",
        "values": [
          "100",
          "110",
          "120"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2016M01",
          "2016M02",
          "2016M03",
          "2016M04",
          "2016M05",
          "2016M06",
          "2016M07",
          "2016M08",
          "2016M09",
          "2016M10",
          "2016M11",
          "2016M12",
          "2017M01",
          "2017M02",
          "2017M03",
          "2017M04",
          "2017M05",
          "2017M06",
          "2017M07",
          "2017M08",
          "2017M09",
          "2017M10",
          "2017M11",
          "2017M12",
          "2018M01",
          "2018M02",
          "2018M03",
          "2018M04",
          "2018M05",
          "2018M06",
          "2018M07",
          "2018M08",
          "2018M09",
          "2018M10",
          "2018M11",
          "2018M12",
          "2019M01",
          "2019M02",
          "2019M03",
          "2019M04",
          "2019M05",
          "2019M06",
          "2019M07",
          "2019M08",
          "2019M09",
          "2019M10",
          "2019M11",
          "2019M12",
          "2020M01",
          "2020M02",
          "2020M03",
          "2020M04",
          "2020M05",
          "2020M06",
          "2020M07",
          "2020M08",
          "2020M09",
          "2020M10",
          "2020M11",
          "2020M12",
          "2021M01",
          "2021M02",
          "2021M03",
          "2021M04",
          "2021M05",
          "2021M06",
          "2021M07",
          "2021M08",
          "2021M09",
          "2021M10",
          "2021M11",
          "2021M12",
          "2022M01",
          "2022M02",
          "2022M03",
          "2022M04",
          "2022M05",
          "2022M06",
          "2022M07",
          "2022M08",
          "2022M09",
          "2022M10",
          "2022M11",
          "2022M12",
          "2023M01",
          "2023M02",
          "2023M03",
          "2023M04",
          "2023M05",
          "2023M06",
          "2023M07",
          "2023M08",
          "2023M09",
          "2023M10",
          "2023M11",
          "2023M12",
          "2024M01",
          "2024M02",
          "2024M03"
        ]
      }
    }
  ],
  "response": {
    "format": "json"
  }
}'
response <- httr::POST(url, body = query, encode = "json")
json_data <- httr::content(response, as = "text")

# Convert JSON data to data frame
data <- fromJSON(json_data)

# Assuming data manipulation and visualization steps here

# Export data to Excel
write.xlsx(data, "output.xlsx", rowNames = FALSE)
write.xlsx(data, "output.xlsx", rowNames = FALSE)
print("Data exported to output.xlsx successfully!")


# Load required libraries
library(readxl)
library(ggplot2)

# Read data from the 3rd sheet named "data" in the output.xlsx file
data <- read.xlsx("output.xlsx", sheet = "data")
head(data)
# Filter data based on specified criteria
filtered_data <- subset(data, Region == 12 & Drivmedel %in% c(100, 110, 120) & Tid >= "2016M01" & Tid <= "2014M03")

# Create visualizations
# Example: Bar plot of fuel type distribution
ggplot(filtered_data, aes(x = factor(Drivmedel), fill = factor(Drivmedel))) +
  geom_bar() +
  labs(title = "Fuel Type Distribution",
       x = "Fuel Type",
       y = "Count") +
  scale_fill_discrete(name = "Fuel Type", labels = c("Petrol", "Diesel", "Electricity")) +
  theme_minimal()



# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Read data from the Excel file
data <- read.xlsx("output.xlsx", sheet = "data")  # Replace "data" with the actual sheet name

# Split the "Region.Drivmedel.Tid" column into separate columns
data_split <- data.frame(do.call(rbind, strsplit(as.character(data$Region.Drivmedel.Tid), ", ")))
colnames(data_split) <- c("Region", "Drivmedel", "Tid")

# Combine the split data with the original data
data <- cbind(data_split, data)

# Convert the "Tid" column to Date format
data$Tid <- as.Date(paste0(data$Tid, "01"), format = "%YM%m%d")

# Convert "Region" and "Drivmedel" to numeric
data$Region <- as.numeric(data$Region)
data$Drivmedel <- as.numeric(data$Drivmedel)


# Filter data for Region 12 and specific fuel types
data_region_12_fuel <- filter(data, Region == 12 & Drivmedel %in% c(100, 110, 120))

# Create a stacked line plot with markers for specific fuel types in Region 12
ggplot(data_region_12_fuel, aes(x = Tid, y = values, color = factor(Drivmedel), group = factor(Drivmedel))) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Vehicle Registrations Over Time for Län Code 12",
       x = "Time Period",
       y = "Number of Registrations",
       color = "Fuel Type") +
  scale_color_manual(values = c("red", "blue", "green")) +  # Customize colors if needed
  theme_minimal()

# Filter data based on specified criteria
filtered_data <- subset(data, Region == 12 & Drivmedel %in% c(100, 110, 120) & Tid >= "2016M01" & Tid <= "2024M03")

# Create visualizations
ggplot(filtered_data, aes(x = factor(Drivmedel), fill = factor(Drivmedel))) +
  geom_bar() +
  labs(title = "Fuel Type Distribution",
       x = "Fuel Type",
       y = "Count") +
  scale_fill_discrete(name = "Fuel Type", labels = c("Petrol", "Diesel", "Electricity")) +
  theme_minimal()

str(data)

# Filter data for Region 12 and specific fuel types
data_region_12_fuel <- subset(data, Region == 12 & Drivmedel %in% c(100, 110, 120))

# Create a stacked line plot with markers for specific fuel types in Region 12
ggplot(data_region_12_fuel, aes(x = Tid, y = as.numeric(values), color = factor(Drivmedel), group = factor(Drivmedel))) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Vehicle Registrations Over Time for Län Code 12",
       x = "Time Period",
       y = "Number of Registrations",
       color = "Fuel Type") +
  scale_color_manual(values = c("red", "blue", "green")) +  # Customize colors if needed
  theme_minimal()


library(ggplot2)

# Filter data for Code 12 (Skåne) and Drivmedel 120 (electricity)
skane_electric <- subset(data, Region == 12 & Drivmedel == 120)

# Convert values to numeric
skane_electric$values <- as.numeric(skane_electric$values)

# Create a line plot for electric cars in Skåne with values
ggplot(skane_electric, aes(x = Tid, y = values)) +
  geom_line() +
  geom_point() +
  labs(title = "Electric Car Registrations Over Time in Skåne",
       x = "Time Period",
       y = "Number of Registrations")


library(ggplot2)

# Filter data for Code 12 (Skåne) and three different fuel types (e.g., 100, 110, 120)
skane_three_fuels <- subset(data, Region == 12 & Drivmedel %in% c(100, 110, 120))

# Convert values to numeric
skane_three_fuels$values <- as.numeric(skane_three_fuels$values)

# Create a bar plot for registrations of three fuels in Skåne
ggplot(skane_three_fuels, aes(x = factor(Drivmedel), y = values, fill = factor(Drivmedel))) +
  geom_bar(stat = "identity") +
  labs(title = "Registrations of Three Fuel Types in Skåne",
       x = "Fuel Type",
       y = "Number of Registrations",
       fill = "Fuel Type") +
  scale_fill_manual(values = c("blue", "green", "red")) +  # Customize fill colors
  theme_minimal()

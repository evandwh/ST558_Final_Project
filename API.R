library(plumber)
library(tidymodels)
library(ggplot2)

# Read in data

water_data <- read_csv("Data/water_potability.csv") |>
  mutate(Potability = factor(Potability,
                             levels = c(0, 1),
                             labels = c("Not Potable", "Potable")))

# Fit best model on entire data
#--------------------------------------------------

rf_recipe <-
  recipe(Potability ~ ., data = water_data) |>
  step_impute_median(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors())

rf_spec <-
  rand_forest(
    trees = 500,
    mtry = 3,
    min_n = 5
  ) |>
  set_engine("ranger") |>
  set_mode("classification")

rf_workflow <-
  workflow() |>
  add_recipe(rf_recipe) |>
  add_model(rf_spec)

rf_fit <- fit(rf_workflow, data = water_data)

#--------------------------------------------------
# Means for defaults
#--------------------------------------------------

mean_ph <- mean(water_data$ph, na.rm = TRUE)
mean_hardness <- mean(water_data$Hardness, na.rm = TRUE)
mean_solids <- mean(water_data$Solids, na.rm = TRUE)
mean_chloramines <- mean(water_data$Chloramines, na.rm = TRUE)
mean_sulfate <- mean(water_data$Sulfate, na.rm = TRUE)
mean_conductivity <- mean(water_data$Conductivity, na.rm = TRUE)
mean_organic <- mean(water_data$Organic_carbon, na.rm = TRUE)
mean_trihalo <- mean(water_data$Trihalomethanes, na.rm = TRUE)
mean_turbidity <- mean(water_data$Turbidity, na.rm = TRUE)

#--------------------------------------------------
#* Predict Potability
#* @param ph
#* @param Hardness
#* @param Solids
#* @param Chloramines
#* @param Sulfate
#* @param Conductivity
#* @param Organic_carbon
#* @param Trihalomethanes
#* @param Turbidity
#* @get /pred

function(
    ph = mean_ph,
    Hardness = mean_hardness,
    Solids = mean_solids,
    Chloramines = mean_chloramines,
    Sulfate = mean_sulfate,
    Conductivity = mean_conductivity,
    Organic_carbon = mean_organic,
    Trihalomethanes = mean_trihalo,
    Turbidity = mean_turbidity
){
  
  new_data <- tibble(
    ph = as.numeric(ph),
    Hardness = as.numeric(Hardness),
    Solids = as.numeric(Solids),
    Chloramines = as.numeric(Chloramines),
    Sulfate = as.numeric(Sulfate),
    Conductivity = as.numeric(Conductivity),
    Organic_carbon = as.numeric(Organic_carbon),
    Trihalomethanes = as.numeric(Trihalomethanes),
    Turbidity = as.numeric(Turbidity)
  )
  
  predict(rf_fit, new_data, type = "prob")
}

# Example calls:
# http://localhost:8000/pred
# http://localhost:8000/pred?ph=7
# http://localhost:8000/pred?ph=8&Hardness=150&Solids=20000

#--------------------------------------------------
#* Information
#* @get /info

function(){
  
  list(
    Name = "Evan Whitfield",
    Website = "https://YOUR_GITHUB_USERNAME.github.io/ST558_Final/"
  )
  
}

#--------------------------------------------------
#* Confusion Matrix Plot
#* @png
#* @get /confusion

function(){
  
  preds <-
    predict(rf_fit, water_data) |>
    bind_cols(water_data)
  
  cm <- conf_mat(preds,
                 truth = Potability,
                 estimate = .pred_class)
  
  autoplot(cm, type = "heatmap")
}
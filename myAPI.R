library(plumber)
library(tidymodels)
library(ggplot2)
library(dplyr)
library(readr)

#--------------------------------------------------
# Read in data
#--------------------------------------------------
water_data <- read_csv("Data/water_potability.csv") |>
  mutate(Potability = factor(Potability,
                             levels = c(0, 1),
                             labels = c("Not Potable", "Potable"))) |>
  drop_na()

#--------------------------------------------------
# Fit best model on entire data
#--------------------------------------------------

rf_recipe <-
  recipe(Potability ~ ., data = water_data)

rf_spec <-
  rand_forest(
    trees = 500,
    mtry = 8,
    mode = "classification"
  ) |>
  set_engine("ranger")

rf_workflow <-
  workflow() |>
  add_recipe(rf_recipe) |>
  add_model(rf_spec)

# setting a seed here to make the confusion matrix reproducible
# commit out if needed!
rf_fit <- fit(rf_workflow, data = water_data)

#--------------------------------------------------
# Means for defaults
#--------------------------------------------------

mean_ph <- mean(water_data$ph)
mean_hardness <- mean(water_data$Hardness)
mean_solids <- mean(water_data$Solids)
mean_chloramines <- mean(water_data$Chloramines)
mean_sulfate <- mean(water_data$Sulfate)
mean_conductivity <- mean(water_data$Conductivity)
mean_organic <- mean(water_data$Organic_carbon)
mean_trihalo <- mean(water_data$Trihalomethanes)
mean_turbidity <- mean(water_data$Turbidity)

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
#--------------------------------------------------
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

# -------------------------------------------------
# Example URLs
#
# http://127.0.0.1:8000/pred?ph=7.086&Hardness=195.9681&Solids=21917.4414&Chloramines=7.1343&Sulfate=333.2247&Conductivity=426.5264&Organic_carbon=14.3577&Trihalomethanes=66.4009&Turbidity=3.9697
#
# http://127.0.0.1:8000/pred?ph=4&Hardness=195.9681&Solids=24000&Chloramines=7.1343&Sulfate=333.2247&Conductivity=426.5264&Organic_carbon=14.3577&Trihalomethanes=66.4009&Turbidity=3.9697
#
# http://127.0.0.1:8000/pred?ph=12&Hardness=200&Solids=21917.4414&Chloramines=7.1343&Sulfate=333.2247&Conductivity=426.5264&Organic_carbon=17&Trihalomethanes=68&Turbidity=4
#--------------------------------------------------

# -------------------------------------------------
#* Information
#* @get /info
# -------------------------------------------------
function(){
  
  list(
    Name = "Evan Whitfield",
    Website = "https://YOUR_GITHUB_USERNAME.github.io/ST558_Final/"
  )
  
}

#--------------------------------------------------
#* Confusion Matrix Plot
#* @serializer png
#* @get /confusion
#--------------------------------------------------
function(){
  
  preds <- predict(rf_fit, water_data) |>
    bind_cols(water_data)
  
  cm <- conf_mat(
    preds, 
    truth = Potability, 
    estimate = .pred_class)
  
  p <- ggplot(as.data.frame(cm$table), 
              aes(x = Truth, y = Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "white")
  
  print(p)
  }
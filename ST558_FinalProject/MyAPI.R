library(tidyverse)


#Read in the data

diabetes_data <- read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")


diabetes_data<- diabetes_data |>
  mutate(Diabetes_binary = factor(Diabetes_binary, levels = c(0,1),
                                  labels = c("no diabetes", 
                                             "diabetes")),
         HighBP = factor(HighBP, levels = c(0,1),
                         labels = c("not high", "high BP")),
         HighChol = factor(HighChol, levels = c(0,1),
                           labels = c("not high", "high cholesterol")),
         CholCheck = factor(CholCheck, levels = c(0,1),
                            labels = c("no cholesterol check", "yes cholesterol check")),
         Smoker = factor(Smoker, levels = c(0,1), 
                         labels = c("no", "yes")),
         Stroke = factor(Stroke, levels = c(0,1), 
                         labels = c("no", "yes")),
         HeartDiseaseorAttack = factor(HeartDiseaseorAttack, 
                                       levels = c(0,1), 
                                       labels = c("no", "yes")),
         PhysActivity = factor(PhysActivity, levels = c(0,1), 
                               labels = c("no", "yes")),
         Fruits = factor(Fruits, levels = c(0,1), 
                         labels = c("no", "yes")),
         Veggies = factor(Veggies, levels = c(0,1), 
                          labels = c("no", "yes")),
         HvyAlcoholConsump = factor(HvyAlcoholConsump, levels =
                                      c(0,1), 
                                    labels = c("no", "yes")),
         AnyHealthcare = factor(AnyHealthcare, levels = c(0,1), 
                                labels = c("no", "yes")),
         NoDocbcCost = factor(NoDocbcCost, levels = c(0,1), 
                              labels = c("no", "yes")),
         GenHlth = factor(GenHlth, levels = c(1,2,3,4,5),
                          labels = c("excellent", "very good",
                                     "good", "fair", "poor")),
         DiffWalk = factor(DiffWalk, levels = c(0,1), 
                           labels = c("no", "yes")),
         Sex = factor(Sex, levels = c(0,1), 
                      labels = c("female", "male")),
         Age = as.factor(Age),
         Education = as.factor(Education),
         Income = as.factor(Income)
  )

# Load the best model from previous sections
load("rf_final_fit.RData")
load("API_FinalProject/rf_final_wkf.RData")


library(plumber)

#* @apiTitle ST558 Final Project API
#* @apiDescription Create API to show results of best model created to predicting diabetes.

#* Show info message
#* @param name first message
#* @param url second message
#* @get /info
function(name = "Ella Gruchacz", url = "https://github.com/egruch/Final-Project") {
  list(paste0("My Name: ",name),
       paste0("Link to my rendered site: ", url))
}
#http://localhost:8001/info

#* Print Prediction
#* @param 
#* @param 
#* @param 
#* @param 
#* @param 
#* @get /pred
function(HighBP = "high", HighChol = "high", BMI = 32, 
         PhysActivity = "no", Age = "11") {
  pred_df <- data.frame(HighBP,
                        HighChol,
                        BMI,
                        PhysActivity,
                        Age)
  pred <- predict(extract_workflow(rf_final_fit), new_data = pred_df,type = "class")
  print(pred)
}
#http://localhost:8001/pred

#* Return Confusion Matrix
#* @serializer png
#* @get /confusion
function() {
  pred_values <- predict(extract_workflow(rf_final_fit), diabetes_data, type = "class")
  pred_values <- pred_values$.pred_class
  actual_values <- diabetes_data$Diabetes_binary
  
  cf <- caret::confusionMatrix(data=pred_values,
                               reference=actual_values)
  plt <- as.data.frame(cf$table)
  plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
  
  ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
    geom_tile() + geom_text(aes(label=Freq)) +
    scale_fill_gradient(low="white", high="blue") +
    labs(x = "Reference",y = "Prediction", title = "Confusion Matrix of Diabetes Prediction")
}

#http://localhost:8001/confusion
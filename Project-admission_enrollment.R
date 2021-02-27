#PROJECT - Admissions Enrollment into University 

#MultipleLinear Regression (Backward Elimination)
#GRE Score, TOEFL Score, University Rating, SOP, LOR, CGPA, Research are Independet Variables
#Chance of Admit is Dependent Variable
#Importing data
dataset = read.csv("Admission_Enrollment.csv", header = TRUE, sep = ",")
#Cleaning the data
dataset = dataset[,-c(1)] 
#Check for missing values and fill it with the mean of the resp column
dataset$GRE.Score = ifelse(is.na(dataset$GRE.Score), 
                     ave(dataset$GRE.Score, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$GRE.Score)

head(dataset)
tail(dataset)

#  Encoding Categorical data 
#dataset$Research = factor(dataset$Research, 
#                        levels = c("No", "Yes"), 
#                        labels = c(0,1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Chance.of.Admit, SplitRatio = 9/10)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature-Scaling no required - deg measurable 

#Fitting Multiple Linear Regression into Training Set
regressor = lm(formula = Chance.of.Admit ~.,
               data = training_set)
summary(regressor)
#Insights 1 - The P-Value of University Rating and SOP are 0.27 and 0.53 - Not significant 
#GRE Score, LOR and CGPA are the most Statistically Significant Variables
#TOEFL Score, Research are the next most significant variables 
#Multiple R-squared = 0.829;  Adjusted R Squared - 0.826
#Eliminating SOP (P-Value = 0.53) and Rerunning the Model 

#Building Optimal Model using Backward Elimination - Eliminating SOP
regressor = lm(formula = Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating + LOR + CGPA + Research,
               data = training_set)
summary(regressor)
#Adjusted R Squred- 0.8264 #Elimination of University Rating and Rerunning the Model 

#Rebuilding the MultipleLinear Regression Model - Eliminating University Rating 
regressor = lm(formula = Chance.of.Admit ~ GRE.Score + TOEFL.Score + LOR + CGPA + Research,
               data = training_set)
summary(regressor)
#Only variables with Significance Level 2,3 present 
#Adjusted R-squared- 0.826
#Updated - This is the Final MultiLinear Model

#Rebuilding the MultipleLinear Regression Model - Eliminating TOEFL Score
regressor = lm(formula = Chance.of.Admit ~ GRE.Score + LOR + CGPA + Research,
               data = training_set)
summary(regressor)
#Adjusted R- Squared - 0.822 => Implying the previous Model was a better fit

y_pred = predict(regressor, newdata = test_set)
#y_pred in console - confirm #Very close- good model

final_data = cbind(test_set,y_pred) #Combining Predicted Value to Original data
write.csv(final_data,"final_data.csv")

#Visualizing the Predicted results 
library(ggplot2)
ggplot(final_data, aes(x = GRE.Score, y = y_pred))+
  geom_point(color = "red")+
  geom_smooth(method = "lm")+
  xlab("GRE Score")+
  ylab("Prediction of Chance of Admission")+
  ggtitle("Prediction Curve of Admission")


#PREDICTION OF A STUDENT TO GET ADMIT - LOGISTIC REGRESSION 
#Logistic Regression based on GRE.Score and Research, and Chance of Admit as the outcome
#Importing the data 
dataset <- read.csv("Admission_Enrollment.csv")
#Cleaning the data
dataset = dataset[,c(2,6,9)] 
dataset$Chance.of.Admit = ifelse(dataset$Chance.of.Admit > 0.5, 1,0) #Converting Chance of Admit into 0 and 1

#splitting the dataset into training set and Test set 
#install.packages("caTools")
library(caTools) #or tick mark in the packages section
set.seed(123) 
split = sample.split(dataset$Chance.of.Admit, SplitRatio = 4/5) 
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling on GRE SCORE & LOR
training_set[, 1:2] = scale(training_set[,1:2])
test_set[,1:2] = scale(test_set[,1:2])

#Fitting Logistic Regression to the Training Set 
classifier = glm(formula = Chance.of.Admit ~.,
                 family = binomial, 
                 data = training_set)

#Predict the test set results 
prob_pred = predict(classifier, type = "response", newdata = test_set[-3]) 
y_pred = ifelse(prob_pred > 0.5, 1,0) 

#Making the Confusion Matrix
cm = table(test_set[,3], y_pred)
#Insights: 94 are correct predictions, 6 are incorrect predictions in test set

#Visualising the Training set 
#install.packages("ElemStatLearn")
library(ElemStatLearn)
set = training_set
#Building the grid 
X1 = seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01) 
X2 = seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01) #-1, +1 so our points are not squeezed in graph
grid_set = expand.grid(X1, X2) #expand.grid is used to create a data frame with all possible combination of arg passed
colnames(grid_set) = c("GRE.Score", "LOR")
prob_set = predict(classifier, type = "response", newdata = grid_set) #response will give us result of pred probab in terms of resp var
y_grid = ifelse(prob_set > 0.5, 1,0) 
plot(set[, -3],
     main = "Admission Prediction with Logistic Regression (Training Set)",
     xlab = "GRE Score", ylab = "Letter of Recommendation (LOR)", 
     xlim = range(X1), ylim = range(X2)) 
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "Tomato")) #Background color
points(set, pch = 21, bg = ifelse(set[,3] == 1, "green4", "red3")) #point plot and color

#Visualising the Test set 
#install.packages("ElemStatLearn")
#library(ElemStatLearn)
set = test_set
#Building the grid 
X1 = seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01) #Expanding range
X2 = seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01) #-1, +1 so our points are not squeezed in graph
grid_set = expand.grid(X1, X2) #expand.grid is used to create a data frame with all possible combination of arg passed
colnames(grid_set) = c("GRE.Score", "LOR")
prob_set = predict(classifier, type = "response", newdata = grid_set) #response will give us result of pred probab in terms of resp var
y_grid = ifelse(prob_set > 0.5, 1,0) 
plot(set[, -3],
     main = "Admission Prediction with Logistic Regression (Test Set)",
     xlab = "GRE Score", ylab = "Letter of Recommendation (LOR)", 
     xlim = range(X1), ylim = range(X2)) 
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "Springgreen3", "Tomato")) #Background color
points(set, pch = 21, bg = ifelse(set[,3] == 1, "green4", "red3")) #point plot and color

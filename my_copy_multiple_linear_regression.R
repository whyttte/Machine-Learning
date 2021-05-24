# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')
# dataset = dataset[,2:3]

# Enconding Categorical Data
dataset$State = factor(dataset$State,
                         levels = c('California', 'Florida', 'New York'),
                         labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(12345)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set[, 2:3])
# test_set[, 2:3] = scale(test_set[, 2:3])

# Fitting Multiple Linear Regression to the Training set
regressorMultiple = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State
, data =training_set)
# you can also write it as regressorMultiple = lm(formula = Profit ~ .)
summary(regressorMultiple)

# Predicting test set results
y_pred = predict(regressorMultiple, newdata = test_set)

# Building the optimal model using Backward Elimination
regressorMultiple = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State
                       , data =training_set)
summary(regressorMultiple)

# remove State
regressorMultiple = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend 
                       , data =training_set)
summary(regressorMultiple)

# remove Administration
regressorMultiple = lm(formula = Profit ~ R.D.Spend + Marketing.Spend
                       , data =training_set)
summary(regressorMultiple)

# remove Marketing Spend
regressorMultiple = lm(formula = Profit ~ R.D.Spend
                       , data =training_set)
summary(regressorMultiple)

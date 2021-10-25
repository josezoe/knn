# Social network database to classify people who Purchased seing social mediadd
# Use KNN
# Classification template

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = Social_Network_Ads[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting K-NN to the Training set and predicting the test set result
library(class)

y_pred=knn(train=training_set[,-3],test=test_set[,-3],cl=training_set[,3],k=5)
y_pred

# y_pred
# [1] 0 0 0 0 0 1 1 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0
# [44] 0 0 0 0 0 0 0 0 0 1 1 1 0 1 0 0 1 0 0 0 1 1 0 1 1 1 1 1 1 1 0 0 0 1 0 0 1 0 1 0 1 0 1
# [87] 1 0 0 1 1 0 1 0 1 1 1 1 0 1
# Levels: 0 1
summary(y_pred)
# > summary(y_pred)
# 0  1 
# 65 35


# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm


# cm
# y_pred
# 0  1
# 0 59  5
# 1  6 30




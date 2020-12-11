#An Introduction to Data Anaylsis and Mining, IUB, Spring 2018
#Instructor: Hasan Kurban

#1. Naive Bayes via "caret package"
install.packages("caret")
library("caret")
head(iris)
# In this part, we first train a naive bayes classifier over training data,
# and then use the model over test data 
set.seed(1234)  
# Creating training  and testing  data sets: Randomly picking 100 data points from Iris data set
# as training data and the rest of 50 data points will be used as test data.
rndSample <- sample(1:nrow(iris),100)
tr <- iris[rndSample, ]  # training data: 100 data points
ts <- iris[-rndSample, ] #  testing data: 50 data points
?train
# Training a Naive Bayes classifier 
model = train(tr[,1:4],tr[,5],'nb',trControl=trainControl(method='cv',number=10))
# Using model to make prediction
predict(model$finalModel,ts[,1:4])
#confusion matrix
table(predict(model$finalModel,ts[,1:4])$class,ts[,5])

#2. Naive Bayes via "e1071 package"
#default distribution for continous variables is a normal distribution.
install.packages("e1071")
library(e1071)
#training
model2 <- naiveBayes(tr$Species ~ ., data = tr)
model2
# Using the model to make predictions
predict(model2,ts[,1:4])
#confusion matrix
table(predict(model2,ts[,1:4]),ts[,5])
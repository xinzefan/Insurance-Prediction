install.packages("e1071")
library(e1071)
install.packages("data.table")
install.packages("cvTools")
library(cvTools)
library(data.table)
train <- read.csv("Desktop/365/train.csv")
test <- read.csv("Desktop/365/test.csv")
replaceMissing <- function(data){
  data[data == -1] <- mean(data)
  return(data)
}
for (i in length(train[1,])){
  train[,i] <- replaceMissing(train[,i])
}
for (i in length(test[1,])){
  test[,i] <- replaceMissing(test[,i])
}
# NB
cl <- as.factor(train$target)
test_id <- test$id
modelc1 <- naiveBayes(cl ~., data = train)
target <-predict(modelc1,test,type = "raw")
tclass <- predict(modelc1,test)
temp <- target[,2]
final <- data.frame(test_id,temp)
result <- rbind(test_id, target[,2])
write.csv(final,file = "desktop/365/results.csv")
####
train2 <- cvFolds(NROW(train),K = 5)
subt1 <- train[train2$subsets[train2$which!=1],]
subt2 <- train[train2$subsets[train2$which!=2],]
subt3 <- train[train2$subsets[train2$which!=3],]
subt4 <- train[train2$subsets[train2$which!=4],]
subt5 <- train[train2$subsets[train2$which!=5],]
######
cl1 <- as.factor(subt1$target)
mo1 <- naiveBayes(cl1 ~., data = subt1)
t1 <-predict(mo1,test,type = "raw")
#####
cl2 <- as.factor(subt2$target)
mo2 <- naiveBayes(cl2 ~., data = subt2)
t2 <-predict(mo2,test,type = "raw")
####
cl3 <- as.factor(subt3$target)
mo3 <- naiveBayes(cl3 ~., data = subt3)
t3 <-predict(mo3,test,type = "raw")
####
cl4 <- as.factor(subt4$target)
mo4 <- naiveBayes(cl4 ~., data = subt4)
t4 <-predict(mo4,test,type = "raw")
####
cl5 <- as.factor(subt5$target)
mo5 <- naiveBayes(cl5 ~., data = subt5)
t5 <-predict(mo5,test,type = "raw")
#####
new_tar <- (t1[,2] + t2[,2] + t3[,2] + t4[,2] +t5[,2])/5
new <- data.frame(test_id, new_tar)
write.csv(new,file = "desktop/365/newres.csv")

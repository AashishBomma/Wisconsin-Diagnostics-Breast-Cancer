train_data <- read.csv(file="C:/Users/vbrahm7/Documents/DM_Assignment_2/trainX.csv", header = F)
train_y <- read.csv(file = "C:/Users/vbrahm7/Documents/DM_Assignment_2/trainY.csv", header = F)
test_data <- read.csv(file = "C:/Users/vbrahm7/Documents/DM_Assignment_2/testX.csv", header = F)
test_y <- read.csv(file = "C:/Users/vbrahm7/Documents/DM_Assignment_2/testY.csv", header = F)
colnames(train_data) <- c("radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean","compactness_mean", "concavity_mean", "concave_mean", "symmetry_mean", "fractal_mean","radius_sd", "texture_sd", "perimeter_sd", "area_sd", "smoothness_sd","compactness_sd", "concavity_sd", "concave_sd", "symmetry_sd", "fractal_sd","radius_max", "texture_max", "perimeter_max", "area_max", "smoothness_max","compactness_max", "concavity_max", "concave_max", "symmetry_max", "fractal_max")
colnames(test_data) <- c("radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean","compactness_mean", "concavity_mean", "concave_mean", "symmetry_mean", "fractal_mean","radius_sd", "texture_sd", "perimeter_sd", "area_sd", "smoothness_sd","compactness_sd", "concavity_sd", "concave_sd", "symmetry_sd", "fractal_sd","radius_max", "texture_max", "perimeter_max", "area_max", "smoothness_max","compactness_max", "concavity_max", "concave_max", "symmetry_max", "fractal_max")
colnames(train_y) <- "malignant"
colnames(test_y) <- "malignant"
final_train_data <- cbind.data.frame(train_data,train_y)
final_test_data <- cbind.data.frame(test_data,test_y)


#1a
str(final_train_data)
str(final_test_data)

is.na(final_train_data)
is.na(final_test_data)
## No missing values in the train_data_complete and test_data_complete


##Radius, Perimeter, Area and Concavity could be the major predictors of diagnosis

cor(final_train_data)
cor(final_test_data)


boxplot(final_train_data)
boxplot(final_test_data)
##There are outliers in the table. 

##Tree based models are resistant towards outliers and multicollinearity, so we use create a decision tree model.


#1b
library(rpart)
dtm <- rpart(malignant~., data= final_train_data, control = rpart.control(cp=-1, minsplit = 2))
print(dtm)

library(rpart.plot)
leaves<-length(unique(dtm$where))
print(leaves)

#1c)
#install.packages("varImp")
#library(varImp)
#install.packages("earth")
#library(earth)
major_predictor<- dtm$variable.importance
major_predictor
## concave_mean, perimeter_max, concave_max are the major predictors as these are the top three important variables
## We did predict that the concave and the perimeter could be the major predictors.  

#1d)
print(dtm)

## Strong rule 1: smoothness_mean>=0.08027 & perimeter_max>=97.155 & texture_max>=20.875 & concave_mean>=0.05592
## support = (145/455)*100 = 31.21%
## confidence = (142/143)*100 = 99.3%

## Strong rule 2: smoothness_mean< 0.10865 & perimeter_max< 97.155 & texture_max>=20.875 & concave_mean>=0.05592
## support = (4/455)*100 = 0.88%
## confidence = (4/7)*100 = 57.1%

#1e)

library(rpart)
##training data accuracy
predict_train_data<-predict(dtm, new_final_data = final_train_data)
table_train_data<- table(predict_train_data,final_train_data$malignant)
accuracy_train_data<- sum(diag(table_train_data))/sum(table_train_data)
print(accuracy_train_data)

##test data accuracy
predict_test_data<-predict(dtm,final_test_data)
table_test_data<-table(predict_test_data,final_test_data$malignant)
accuracy_test_data<-sum(diag(table_test_data))/sum(table_test_data)
print(accuracy_test_data)

#1f)

printcp(dtm)
plotcp(dtm)

## In order to predict the Y-lables we can construct the decision tree(best possible), if we take cp value to be 0.023(the first value below the horizontal dotted line).
## doubt ( how to evalute its performance)

best_dtm <- rpart(malignant~., data= final_train_data, control = rpart.control(cp=0.023, minsplit = 2))
print(best_dtm)


#1g)

rpart.plot(best_dtm)
rules_best_dtm<-rpart.rules(best_dtm)
print(rules_best_dtm)


#2

zoo_data <- read.csv(file="C:/Users/vbrahm7/Documents/DM_Assignment_2/zoo.csv", header = T)
zoo_train_data <- read.csv(file = "C:/Users/vbrahm7/Documents/DM_Assignment_2/zoo1.csv", header = T)
zoo_test_data <- read.csv(file = "C:/Users/vbrahm7/Documents/DM_Assignment_2/zoo2.csv", header = T)
str(zoo_train_data)
library(dlookr)
describe(zoo_data)
unique(zoo_data$animal)
unique(zoo_data$type)
summary(zoo_data)

#2a)
train_zoo_data <- zoo_train_data[ ,-1]
library(rpart)
zoo_dtm <- rpart(type ~ venomous + legs + predator , data= zoo_train_data, control = rpart.control(cp=0.01, minsplit = 2))
print(zoo_dtm)
zoo_test_dtm <- rpart(type ~ venomous + legs + predator , data= zoo_test_data, control = rpart.control(cp=0.01, minsplit = 2))
print(zoo_test_dtm)
## After examining the data, we went with venomous, legs and predator for the classification scheme. 

#2b)
library(rpart.plot)
rpart.plot(zoo_dtm)
rpart.plot(zoo_test_dtm)
zoo_train_leaves<-length(unique(zoo_dtm$where))
print(zoo_train_leaves)
## The zoo tree has 7 leaves.
zoo_test_leaves<-length(unique(zoo_test_dtm$where))
print(zoo_test_leaves)
## The zoo test tree has 5 leaves

## accuracy of zoo train data
predict_zoo_train_data<-predict(zoo_dtm, new_final_data = zoo_train_data,type= "class")
table_zoo_train_data<- table(predict_zoo_train_data,zoo_train_data$type)
accuracy_zoo_train_data<- sum(diag(table_zoo_train_data))/sum(table_zoo_train_data)
print(accuracy_zoo_train_data)

## accuracy of zoo test data
predict_zoo_test_data<-predict(zoo_test_dtm, new_final_data = zoo_test_data,type= "class")
table_zoo_test_data<- table(predict_zoo_test_data,zoo_test_data$type)
accuracy_zoo_test_data<- sum(diag(table_zoo_test_data))/sum(table_zoo_test_data)
print(accuracy_zoo_test_data)

#2c)
library(rpart)

## we are taking control values of minbucket=4,cp=-1,minsplit=4
zoo_dtm_4 <- rpart(type ~ venomous + legs + predator , data= zoo_train_data, control = rpart.control(minbucket = 4,cp=-1, minsplit = 4))
print(zoo_dtm_4)
zoo_test_dtm_4 <- rpart(type ~ venomous + legs + predator , data= zoo_test_data, control = rpart.control(minbucket =4,cp=-1, minsplit = 4))
print(zoo_test_dtm_4)

## accuracy for the above control values for above train data:
predict_zoo_train_data_4<-predict(zoo_dtm_4, new_final_data = zoo_train_data,type= "class")
table_zoo_train_data_4<- table(predict_zoo_train_data_4,zoo_train_data$type)
accuracy_zoo_train_data_4<- sum(diag(table_zoo_train_data_4))/sum(table_zoo_train_data_4)
print(accuracy_zoo_train_data_4)

## accuracy for the above control values for above test data:
predict_zoo_test_data_4<-predict(zoo_test_dtm_4, new_final_data = zoo_test_data,type= "class")
table_zoo_test_data_4<- table(predict_zoo_test_data_4,zoo_test_data$type)
accuracy_zoo_test_data_4<- sum(diag(table_zoo_test_data_4))/sum(table_zoo_test_data_4)
print(accuracy_zoo_test_data_4)

## we are taking control values of minbucket=2,cp=0.69,minsplit=3
zoo_dtm_2 <- rpart(type ~ venomous + legs + predator , data= zoo_train_data, control = rpart.control(minbucket = 2,cp=0.69, minsplit = 3))
print(zoo_dtm_2)
zoo_test_dtm_2 <- rpart(type ~ venomous + legs + predator , data= zoo_test_data, control = rpart.control(minbucket = 2,cp=0.69, minsplit = 3))
print(zoo_test_dtm_2)

## accuracy for the above control values for above train data:
predict_zoo_train_data_2<-predict(zoo_dtm_2, new_final_data = zoo_train_data,type= "class")
table_zoo_train_data_2<- table(predict_zoo_train_data_2,zoo_train_data$type)
accuracy_zoo_train_data_2<- sum(diag(table_zoo_train_data_2))/sum(table_zoo_train_data_2)
print(accuracy_zoo_train_data_2)

## accuracy for the above control values for above test data:
predict_zoo_test_data_2<-predict(zoo_test_dtm_2, new_final_data = zoo_test_data,type= "class")
table_zoo_test_data_2<- table(predict_zoo_test_data_2,zoo_test_data$type)
accuracy_zoo_test_data_2<- sum(diag(table_zoo_test_data_2))/sum(table_zoo_test_data_2)
print(accuracy_zoo_test_data_2)

#2d)
##accuracy for type = milk
zoo_dtm_milk <- rpart(type ~ milk , data= zoo_data, control = rpart.control(cp=0.01, minsplit = 2))
print(zoo_dtm_milk)
predict_zoo_data_milk<-predict(zoo_dtm_milk, new_final_data = zoo_data,type= "class")
table_zoo_data_milk<- table(predict_zoo_data_milk,zoo_data$type)
accuracy_zoo_data_milk<- sum(diag(table_zoo_data_milk))/sum(table_zoo_data_milk)
print(accuracy_zoo_data_milk)

##accuracy for type = feathers
zoo_dtm_feather <- rpart(type ~ feathers , data= zoo_data, control = rpart.control(cp=0.01, minsplit = 2))
print(zoo_dtm_feather)
predict_zoo_data_feather<-predict(zoo_dtm_feather, new_final_data = zoo_data,type= "class")
table_zoo_data_feather<- table(predict_zoo_data_feather,zoo_data$type)
accuracy_zoo_data_feather<- sum(diag(table_zoo_data_feather))/sum(table_zoo_data_feather)
print(accuracy_zoo_data_feather)

##accuracy for type = aquatic
zoo_dtm_aquatic <- rpart(type ~ aquatic , data= zoo_data, control = rpart.control(cp=0.01, minsplit = 2))
print(zoo_dtm_aquatic)
predict_zoo_data_aquatic<-predict(zoo_dtm_aquatic, new_final_data = zoo_data,type= "class")
table_zoo_data_aquatic<- table(predict_zoo_data_aquatic,zoo_data$type)
accuracy_zoo_data_aquatic<- sum(diag(table_zoo_data_aquatic))/sum(table_zoo_data_aquatic)
print(accuracy_zoo_data_aquatic)


## The tree with attributes “milk”, “feathers” yeilds the best results.
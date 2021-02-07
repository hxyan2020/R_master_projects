### R INDIVIDUAL COURSEWORK SVM+DTRF_Haixiang 
## Question 1
# install packages
install.packages("ISLR")
install.packages("e1071")
install.packages("caret")
install.packages("pROC")
install.packages("ROCR")
library(ISLR)
library(caret)
library(e1071)
library(tree)
library(pROC)
library(randomForest)
library(ROCR)


# load data
data(OJ)
attach(OJ)

# data tidiness 
OJ=OJ[complete.cases(OJ), ]    #check if there is any missing data
OJ

# split data with 70% training and 30% test data
trainIndex <- createDataPartition(OJ$Purchase, p=0.7,list=FALSE, times = 1)

train<-OJ[trainIndex,]   #training data 
test<-OJ[-trainIndex,]   #test data 
trainlabel<-OJ[trainIndex]   #training label
testlabel<-OJ[-trainIndex]   #test label

# specify the cost vector
set.seed(1234)
cost<-c(0.01,0.1,1,10)

#########################################################################
#(1) fit support vector classifier
set.seed(122)
svm_linear = tune(svm,Purchase~.,data=train,
                 ranges=list(cost=cost),kernel='linear')

# summaty of accuracy rates with corresponding cost values
summary(svm_linear)
# plot
plot(svm_linear)

#########################################################################
#(2) fit support vector machine with radial kernel 
svm_radial = tune(svm,Purchase~.,data=train,
                ranges=list(cost=cost),kernel='radial',gamma=0.25)   #set gamma to default

# summaty of accuracy rates with corresponding cost values
summary(svm_radial)
#plot
plot(svm_radial)

#########################################################################
#(3) fit support vector machine with polynomial kernel 
svm_poly = tune(svm,Purchase~.,data=train,
                ranges=list(cost=cost),kernel='polynomial',degree=2)   #specify cost and degree parameters

# summaty of accuracy rates with corresponding cost values
summary(svm_poly)
#plot
plot(svm_poly)

#########################################################################
#(4) compute test prediction and accuracy rate for each of the above SVM methods
pred1 = predict(svm_linear$best.model,test,decision.values=TRUE)
testrate1 = mean(test$Purchase != pred1)
pred2 = predict(svm_radial$best.model,test,decision.values=TRUE)
testrate2 = mean(test$Purchase != pred2)
pred3  = predict(svm_poly$best.model,test,decision.values=TRUE)
testrate3 = mean(test$Purchase != pred3)

# ROC function
rocplot =function (pred , truth , order,...){
  predob = prediction (pred , truth,label.ordering = order)
  perf = performance (predob,"tpr", "fpr")
  plot(perf,...)}

#source("rocplot.R")

# ROC plotting
roc_linear <- roc(response = test$Purchase, predictor =as.numeric(pred1))
roc_radial <- roc(response = test$Purchase, predictor =as.numeric(pred2))
roc_poly <- roc(response = test$Purchase, predictor =as.numeric(pred3))

plot(roc_linear,col = c("red"))
plot(roc_radial,col = c("blue"), add=TRUE)
plot(roc_poly,col = c("green"), add=TRUE)

# set up legend details 
legend("bottomright", legend = c("Linear","Polynomial","Radial"), lty = c(1), col = c("red","blue","green"))

#########################################################################
#(5) decision tree
# install.packages
install.packages("tree")
library("tree")
install.packages("randomForest")
library("randomForest")
install.packages("gbm")
library("gbm")
install.packages("rpart")
library("rpart")
install.packages("e1071")
library("e1071")
install.packages("rpart.plot")
library("rpart.plot")
install.packages("AUC")
library(AUC)

# (5.1) train the model without cross validation or pruning
set.seed(13243)
OJ_tree = tree(Purchase ~., train)
summary(OJ_tree)

# plot the tree without cross validation or pruning
plot(OJ_tree)
text(OJ_tree,pretty=1,cex=0.8)

# evaluate the error rate
testrate_dt = mean(predict(OJ_tree, test, type='class')!=test$Purchase)
testrate_dt

# (5.2) train the model with cross-validation (10-fold, repeat 3 times)
fitcontrol=trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3)
set.seed(1)
OJ_tree_cv=train(train[,-1],
                  train[,1],
                  method = "rpart",
                  trControl = fitcontrol)
OJ_tree_cv

# see the prediction result
pred_dt_cv = predict(OJ_tree_cv, test)

# plot the tree after cross validation
plot(OJ_tree_cv$finalModel)
text(OJ_tree_cv$finalModel,pretty=1,cex=.8)

# (5.3) find the optimal tree size 
#tree pruning
OJ_cv = cv.tree(OJ_tree,FUN=prune.misclass)
plot(OJ_cv)

# visualize the best tree size by examining deviation
plot(OJ_cv$size, OJ_cv$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")

# prune the tree according to the best tree size based on above analysis
prunedTree_best = prune.misclass(OJ_tree,best = OJ_cv$size[which.min(OJ_cv$dev)])
pred_dt_best = predict(prunedTree_best, test)
testrate_dt_best = mean(predict(prunedTree_best, test, type='class')!=test$Purchase)
testrate_dt_best

# (5.4) prune the tree with terminal nodes of 5
prunedTree_five = prune.misclass(OJ_tree,best = 5)
pred_dt_five = predict(prunedTree_five, test)
testrate_dt_five = mean(predict(prunedTree_five, test, type='class')!=test$Purchase)
testrate_dt_five

# plot the pruned tree with terminal nodes of 5 
plot(prunedTree_five)
text(prunedTree_five,pretty=1,cex=0.8)

#########################################################################
#(6) random forest
# train the model under random forest method
set.seed(2)
mtryGrid=expand.grid(mtry=c(1,2,3,4,5,6))    # specify mtry as a vector
OJ_rf=train(Purchase~.,data=train,method="rf",
            metric="Accuracy",
            tuneGrid=mtryGrid)
OJ_rf   

# prediction with random forest
pred_rf = predict(OJ_rf, test, type='prob')
pred_rf

#variable importance
#importance(OJ_rf)   
varImp(OJ_rf)

#plot variable importance
plot(varImp(OJ_rf))

#########################################################################
#(7) ROC plot for decision tree and random forest
dt_roc = rocplot(pred_dt_five[,2],test[,1],order=c("CH","MM"),col="black",lwd=2,cex.lab=1.5,cex.axis=1.5,main="Test set")
rf_roc = rocplot(pred_rf[,2],test[,1],order=c("CH","MM"),add=TRUE,col="blue",lwd=2,cex.lab=1.5,cex.axis=1.5)

# AUC values
auc(roc(pred_dt_five[,2],test[,1]))
auc(roc(pred_rf[,2],test[,1]))

# set up legend details
legend("bottomright",
       legend = c("Decision Tree","Random Forest"),
       col=c("black","blue"),cex=0.5,lty=1,lwd=2)

# set the axis scales
x=seq(0,1,0.01); y=x
# add reference line 
lines(x,y,lwd =2, col =" red",lty=2)


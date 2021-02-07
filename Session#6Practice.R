### R EXERCISE 6 DECISION TREE
##1. Heart Data to Predict Whether a Patient Has Heart Disease 
#(1) install.packages("caret")
install.packages("caret")
library("caret")
install.packages("tree")
library("tree")
install.packages("randomForest")
library("randomForest")
install.packages("gbm")
library("gbm")

#(2) load data
heart=read.csv("Heart.csv",header = TRUE)

#(3) data tydiness: check missing values
sum(is.na(heart))   #function is.na
heart=heart[complete.cases(heart),]   #remove missing values    #function: complete

#(4) create training/test split
set.seed(345)
train.index=createDataPartition(heart[,ncol(heart)],p=0.7,list=FALSE)
train=heart[train.index,]
test=heart[-train.index,]


##2. Decision Tree 
#(1) Train a decision tree
heart.tree=tree(AHD ~ . , train)   #function: tree

summary(heart.tree)

#(2) have a look at how the tree is plitted 
heart.tree

#(3) plot the decision tree 
plot(heart.tree)
text(heart.tree, pretty=0,cex=0.7)

#try different value for "pretty" 
plot(heart.tree)
text(heart.tree,pretty=1,cex=0.7)

plot(heart.tree)
text(heart.tree,pretty=NULL,cex=0.7)

#(4) Test error
pred=predict(heart.tree,test[,-ncol(test)],type="class")
mean(pred==test[,ncol(test)])   #accuracy rate 


##3. Prune the Tree 
#(1) use classification error rate to guide cross validation and get the best alpha
set.seed(203)
heart.cv=cv.tree(heart.tree,FUN=prune.misclass)   #cv.tree to get the best alpha;  #FUN:= =prune.misclass to indicate we want the classification error rate to guide the cross validation process 
heart.cv

#(2) Plot the cross-validation results
par(mfrow=c(1,2))
#plot CV error rate against T (size of the tree)
plot(heart.cv$size,heart.cv$dev,type="b",
     xlab="number of leaves of the tree",ylab="CV error rate%",
     cex.lab=1.5,cex.axis=1.5)
#plot CV error rate agianst alpha 
plot(heart.cv$k,heart.cv$dev,type="b",
     xlab=expression(alpha),ylab="CV error rate%",
     cex.lab=1.5,cex.axis=1.5)

#(3) prune the tree and plot
heart.prune=prune.misclass(heart.tree,best=7)
plot(heart.prune)
text(heart.prune,pretty=0)

#(4) predict the test instances
pred.prune=predict(heart.prune,test[,-ncol(test)],type="class")
mean(pred.prune==test[,ncol(test)])


##4. Use caret library to build decision trees
install.packages("gbm")
library("gbm")
install.packages("rpart")
library("rpart")
install.packages("e1071")
library("e1071")
install.packages("rpart.plot")
library("rpart.plot")
#(1) set up the traincontrol parameter beforehand 
fitcontrol=trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3)

#(2) train the data using rpart
set.seed(1)
heart.rpart=train(train[,-ncol(heart)],
                  train[,ncol(heart)],
                  method = "rpart",
                  tuneLength=5,
                  trControl = fitcontrol)
heart.rpart

#(3) to look at the details of this tree
print(heart.rpart$finalModel)

#(4) plot 
plot(heart.rpart$finalModel)
text(heart.rpart$finalModel, cex=.8)

#(5) use "rattle" package to produce fancy trees
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("rattle")
library("rattle")
fancyRpartPlot(heart.rpart$finalModel)

#[Alternaive] Instead of using the default parameters of cp, we can specify our own values
fitcontrol=trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3)

set.seed(1)
cpGrid=expand.grid(cp=c(0.01,0.02,0.03))   #specify our own values for cp
heart.rparts=train(train[,-ncol(heart)],
                   train[,ncol(heart)],
                   method = "rpart",
                   tuneGrid=cpGrid,
                   trControl = fitcontrol)

heart.rparts


##5.Bagging
#(1) run the bagging
set.seed(103)
heart.bag=randomForest(AHD~.,data=train,mtry=13,importance=TRUE,ntree=500)  #set mtry=13 when use bagging
heart.bag

#(2) get the prediction accuracy with bagging 
pred.bag=predict(heart.bag,newdata=test[,-ncol(test)])
mean(pred.bag==test[,ncol(test)])


##6.Random Forest
#(1) run the random forest
set.seed(921)
heart.rf=randomForest(AHD~.,data=train,mtry=6,importance=TRUE,ntree=500)  #set mtry=6 to use random forest
heart.rf

#(2) predict the prediction accuracy with random forest 
pred.rf=predict(heart.rf,newdata=test[,-ncol(test)])
mean(pred.rf==test[,ncol(test)])

#(3) view importance of variables
importance(heart.rf)
varImpPlot(heart.rf)


##7.Use Caret Library for Random Forest
#(1) set up taincontrol parameters beforehand
fitControl=trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3)

#(2) train the model under random forest method
set.seed(2)
rfFit=train(AHD~.,data=train,method="rf",metric="Accuracy",
            trControl=fitControl,tuneLength=5)

rfFit

#(3) plot
plot(rfFit)

#(4) prediction accuracy
rfFit$finalModel

#(5) variable importance
varImp(rfFit)

#(6) plot variable importance
plot(varImp(rfFit))


##8. Boosting 
#(1)set training and test data 
set.seed(18)
train[,ncol(train)]=ifelse(train$AHD=="No",0,1)
test[,ncol(test)]=ifelse(test$AHD=="No",0,1)

#(2) fit into the model boosting
heart.boost=gbm(AHD~.,data=train,distribution="bernoulli",n.trees=5000,
                interaction.depth=2,shrinkage=0.01)
summary(heart.boost)

#(3) prediction and accuracy rate 
pred.boost=predict(heart.boost,newdata=test[,-ncol(test)],n.trees=5000,type="response")
pred.boost=ifelse(pred.boost<0.5,0,1)

mean(pred.boost==test[,ncol(test)])


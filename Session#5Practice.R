### R EXERCISE 5 LDA
##1. Performance Measures With Imbalanced Data
#(1) load data
library(ISLR)
summary(Caravan)

#(2) standardise the data
Caravan_scale=scale(Caravan [,-86])   #excluding the label 

#(3) test sample index
index=1:1000   #take the first 1000 out of 5822 individuals as teh test data 

#(4) get training and test set
train.X=Caravan_scale[-index,]   
test.X=Caravan_scale[index,]
train.Y=Caravan$Purchase[-index]   #label is purchase
test.Y=Caravan$Purchase[index]

#(5) run kNN function to predict 
#load the package that contains function "knn" 
library("class")

set.seed (198)
knn.pred=knn(train.X,test.X,train.Y,k=1)   #1NN

#(6) error rate 
mean(test.Y!=knn.pred)
#The error rate is just around 12%, which is good. However, considering the imbalance feature of this data: we
#can get an error rate of 6% if we predict all test observations as No. 
#Thus the error rate is no longer a good measure to assess the quality of the model. In this dataset, 
#we care more about the accuracy of predicting people would like to buy the insurance. If without any 
#if we do our research first, then we could only visit customers who are likely to buy the insurance, 
#which saves time and resources.

#(7) the accuracy prediction rate by prediction first (instead of sampling all respondents)
table(knn.pred,test.Y)   #improved accuracy 



#---------——------------------------------------------------------
##2. Performance Measurement With Sensitivity, Specificity, and Accuracy
#(1) self-defined functions to calculate specificity and sensitivity
performance.measure<-function(pred,truth,pos,neg){   #define a function with 4 inputs 
#pred: predicted labels (factor)
#truth: true labels (factor)
#pos: positive level
# neg: negative level

  #(1.1) get confusion table
  confusion=table(pred,truth)
  #(1.2) get true neg, true pos, false neg, false pos
  tn=confusion[neg,neg]
  tp=confusion[pos,pos]
  fn=confusion[neg,pos]
  fp=confusion[pos,neg]
  
  #(1.3) calculate sensitivity
  sens=tp/(tp+fn)
  #(1.4) calculate specificity
  spec=tn/(tn+fp)
  
  #(1.5) put the two values in a list
  measures=list(sensitivity=sens,specificity=spec)
  #(1.6) return the list as function output
  return(measures)
}

#(2) now use the previously saved "performance measurement.R" file to measure the data 
#(2.1) load data
library(caret)

#(2.2) fit into the knn model to get the predictions (the inputs for calculating sensitivity and specificity)
set.seed(47)
knn3_pred=knn3Train(train.X, test.X, train.Y, k = 9, prob=TRUE)

#(2.3) calculate sensitivity and specificity
source("performance-measure.R")   #load another R.file
pos=levels(test.Y)[2]; neg=levels(test.Y)[1]    #use[1] to locate the data 
measures=performance.measure(as.factor(knn3_pred),test.Y,pos,neg)  #function "as.factor" (ingredients: the knn model, the test label, positive prediction, negative prediction)
measures

#(2.4) now pull out sensitivity and specificity individually from the measure matrix above
sensitivity(as.factor(knn3_pred),test.Y,pos)
specificity(as.factor(knn3_pred),test.Y,neg)



#---------——------------------------------------------------------
##3. ROC Curve
#(1) load package
install.packages("ROCR")
library(ROCR)

#(2) extract the data needed for plotting ROC curve
att=attributes(knn3_pred)$prob    #get the probability (predictions rates out of the model "knn3_pred"); then feed into next line 
pred.ROCR = prediction(att[,2], (test.Y))   #extract the value from the above variable "att" and do the prediction
roc.curve = performance(pred.ROCR,"tpr","fpr")   #tpr = true positive rate (sensitivity);  #fpr = false positive rate (1- specificity)

#(3) plot
plot(roc.curve,lwd=2,cex.lab=1.5,cex.axis=1.5, font.lab=2,
     xlab="False positive rate (1-specificity)",
     ylab="True positive rate (sensitivity)")

#(4) add reference line on which accuracy = 0.5 
x=seq(0,1,0.01); y=x   #set the space on x-axis
lines(x,y,lwd =2, col =" red",lty=2)    #the reference line    #lty, lwd = line type, line width



#---------——------------------------------------------------------
##4. [Alternative] Use pROC Package To Draw The ROC Curve
#under this package, no need to set the reference line 
#(1) set up train control
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,    # 5-fold CV
  repeats = 5,     # repeated five times
  summaryFunction = twoClassSummary,   #???
  classProbs = TRUE)

#(2) training process using the method "knn" 
set.seed(5)
knnFit=train(train.X,train.Y, method = "knn",
             trControl = fitControl,
             metric = "ROC",   
             preProcess = c("center","scale"),
             tuneLength=5)
knnFit

#(3) now use the trained/tuned model to predict 
knn.pred <- predict(knnFit,test.X)   #feed in the model "knnFit" and the FEATURES "test.X"
confusionMatrix(knn.pred,test.Y)  

knn.probs <- predict(knnFit,test.X,type="prob")   #get the probability data from the result of prediction 
head(knn.probs)   

#(4) load the package 
install.packages("pROC")
library("pROC")

#(5) use the function "roc" to get the line data
knn.ROC <- roc(predictor=knn.probs$No,    #the inputs/independent variable?
               response=test.Y,    #the test label 
               levels=rev(levels(test.Y)))    #???
#fucntion "levels" provides access to the levels attribute of a variable. The first form returns the value of the levels of its argument and the second sets the attribute.

#(6) setting direction: controls < cases
knn.ROC$auc  #???

#(7) plot
plot(knn.ROC,main="ROC curve")
knn.ROC$auc



#---------——------------------------------------------------------
##5. Assess the classification performances of kNN and LDA 
#(1) sload package and data 
library(caret)

data(GermanCredit)
GermanCredit[,c("Purpose.Vacation","Personal.Female.Single")] <- list(NULL)   #Delete two variables where all values are the same for both classes

#(2) create training and test sets
set.seed(12)
trainIndex = createDataPartition(GermanCredit$Class, p = 0.7, list = FALSE, times = 1)
train.feature=GermanCredit[trainIndex,-10] # training features
train.label=GermanCredit$Class[trainIndex] # training labels
test.feature=GermanCredit[-trainIndex,-10] # test features
test.label=GermanCredit$Class[-trainIndex] # test labels

#(3) first, use knn and cross validation to tune the model 
#set up train control
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE)

#training process
set.seed(5)
knnFit=train(train.feature,train.label, method = "knn",
             trControl = fitControl,
             metric = "ROC",
             preProcess = c("center","scale"),
             tuneLength=10)
knnFit

#(4) second, use LDA to do the same thing again 
ldaFit=train(train.feature,train.label, method = "lda",
             trControl = trainControl(method = "none"))
ldaFit$finalModel

#(4) use KNN model to predict and get the confusionMatrix
knn.pred <- predict(knnFit,test.feature)
confusionMatrix(knn.pred,test.label)

#(5) get the probability data from the above prediction (under KNN model)
knn.probs <- predict(knnFit,test.feature,type="prob")
head(knn.probs)

#(6) load the package 
library(pROC)

#(7) use the function "roc" to get the line data (under KNN model )
knn.ROC <- roc(predictor=knn.probs$Bad,
               response=test.label,
               levels=rev(levels(test.label)))

#(8) setting direction: controls < cases 
knn.ROC$auc

#(9) plot the ROC curve deom KNN model 
plot(knn.ROC,main="ROC curve")

#(10) (repeat above with LDA) use KNN model to predict and get the confusionMatrix
lda.pred <- predict(ldaFit,test.feature)
confusionMatrix(lda.pred,test.label)

#(11) get the probability data from the above prediction (under LDA model)
lda.probs <- predict(ldaFit,test.feature,type="prob")
head(lda.probs)

#(12) use the function "roc" to get the line data (under LDA model )
lda.ROC <- roc(predictor=lda.probs$Bad,
               response=test.label,
               levels=rev(levels(test.label)))

#(13) setting direction: controls < cases 
lda.ROC$auc

#(14) add the LDA line to the previous KNN line 
lines(lda.ROC,col="blue") #mind the function here is "lines", no longer "plot"

#(15) add legend and other accesaries 
legend("bottomright",legend=c("kNN","LDA"),
       col=c("black","blue"),lty=c(1,1),cex=1,text.font=2)



#---------——------------------------------------------------------
##6. Improve The Classification Using SMOTE
#method used: upsampling the minority class
#(1) see the class distribution to make sure this dataset is imbalanced
table(GermanCredit$Class)

#(2) knn with smote
# set up train control
fitControls <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  sampling="smote")   #only this one line will do 

# training process
set.seed(5)
knnFits=train(train.feature,train.label, method = "knn",
              trControl = fitControls,
              metric = "ROC",
              preProcess = c("center","scale"),
              tuneLength=10)
knnFits

#(3) (repeat the above with lda) lda with SMOTE
set.seed(5)
ldaFits=train(train.feature,train.label, method = "lda",
              trControl = trainControl(sampling="smote"))
ldaFits

ldaFits$finalModel

#(4) prediction with tuned/trained knn model 
knn.preds <- predict(knnFits,test.feature)
confusionMatrix(knn.preds,test.label)

#(5) get the probability data from the above prediciton (under KNN model)
knn.probss <- predict(knnFits,test.feature,type="prob")
head(knn.probss)

#(6) get the ROC curve information (under KNN model)
knn.ROCs <- roc(predictor=knn.probss$Bad,
                response=test.label,
                levels=rev(levels(test.label)))

#(7)# Setting direction: controls < cases
knn.ROCs$auc

#(8) plot ROC curve under KNN model 
plot(knn.ROCs,main="ROC curve")

#(9) (repeat the above with LDA) prediction by the tuned LDA model & get the confusionMatrix
lda.preds <- predict(ldaFits,test.feature)
confusionMatrix(lda.preds,test.label)

#(10) get the probability data from the above prediciton (under LDA model)
lda.probss <- predict(ldaFits,test.feature,type="prob")
head(lda.probss)

#(11) get the ROC curve information (under LDA model)
lda.ROCs <- roc(predictor=lda.probss$Bad,
                response=test.label,
                levels=rev(levels(test.label)))

#(12) Setting direction: controls < cases
lda.ROCs$auc

#(13) plot ROC curve under LDA model 
lines(lda.ROCs,col="blue")
legend("bottomright",legend=c("kNN","LDA"),
       col=c("black","blue"),lty=c(1,1),cex=1,text.font=2)

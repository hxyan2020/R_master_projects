###R EXERCISE 3 KNN(2)
##1. Calculate Classificaiton Accuracy
#(1) activate package
library(class)

#(2) get training data
n=25 # the number of training data in each class
NN=dim(iris3)[1]# the total number of observations in each class
set.seed(983)
index_s=sample(1:NN,n)   #function "sample": sampling 25 data out of the entire class randomly
index_c=sample(1:NN,n)
index_v=sample(1:NN,n)

train_rand = rbind(iris3[index_s,,1], iris3[index_c,,2], iris3[index_v,,3])

#(3) get test data
test_rand = rbind(iris3[-index_s,,1], iris3[-index_c,,2], iris3[-index_v,,3])

#(4) get class factor for training data
train_label= factor(c(rep("s",n), rep("c",n), rep("v",n)))

#(5) get class factor for test data
test_label_true=factor(c(rep("s",NN-n), rep("c",NN-n), rep("v",NN-n)))

#(6) classification using knn, with k=5
kk=5 # number of nearest neighbours
set.seed(275)
knn_pred=knn(train=train_rand,test=test_rand,cl=train_label, k=kk, prob=FALSE)

#(7) classification accuracy 
table(knn_pred,cl)   
#count correct predictions and divide by the total predictions
(22+25+25)/75
#[Alternative] instead of mannually count, could do this
sum(diag(table(knn_pred,cl)))/nrow(test)   #nrow(test) is the total number of prediction;   
#diag = diagnal values in the table; 
#sum = sum all the diagnal values = correct predictions



#----------------------------------------------------------------
##2. LOOCV To Choose The Value Of "k" Using The Training Data
?knn.cv  #under package "class", the function of LOOCV 

#(1)  get training data
n=25 
NN=dim(iris3)[1]
set.seed(983)
index_s=sample(1:NN,n)
index_c=sample(1:NN,n)
index_v=sample(1:NN,n)

train_rand = rbind(iris3[index_s,,1], iris3[index_c,,2], iris3[index_v,,3])

#(2) get test data
test_rand = rbind(iris3[-index_s,,1], iris3[-index_c,,2], iris3[-index_v,,3])

#(3) get class factor for training data
train_label= factor(c(rep("s",n), rep("c",n), rep("v",n)))

#(4) get class factor for test data
test_label_true=factor(c(rep("s",NN-n), rep("c",NN-n), rep("v",NN-n)))

#(5) evaluate k=1,3,5,7,9 on the training data (set up the vector for loop)
kk=c(1,3,5,7,9)
# initialise acc to store the mean accuracy of LOOCV for five "k"
acc=vector("numeric",length=length(kk))   #set the space for vector "acc" to 5 

#looping the 5 "k"
set.seed(649)
for(ii in 1:length(kk)){
  knn_pred=knn.cv(train=train_rand, cl=train_label, k = kk[ii])   #the output of knn_pred is a vector of string
  acc[ii]=mean(knn_pred==train_label)   #use function "mean" to get the accuracy rate (by comparing to training label)
}

#(6) present the accuracy rate for each of the 5 "k" and choose the biggest
acc

kk_LOOCV=kk[which.max(acc)]   #function "which.max" 

#(7) use the "k" that have to biggest accuracy rate for the prediction
set.seed(275)
knn_pred_test=knn(train=train_rand,test=test_rand,cl=train_label, k=kk_LOOCV, prob=FALSE)
acc_test=mean(knn_pred_test==test_label_true)
acc_test   #the accuracy rate (highest) for the TEST data



#----------------------------------------------------------------
##3. LOOCV For Regession (plot the MSE for each "k")
#(1) Load data
Advertising=read.csv("Advertising.csv",header=TRUE)

#(2) get training data (features & labels)
set.seed(1)
n=100
ind.train=sample(nrow(Advertising),n)
train.feature=Advertising[ind.train,2:4]
train.y=Advertising[ind.train,5]

#(3) get test data (features & labels)
test.feature=Advertising[-ind.train,2:4]
test.y=Advertising[-ind.train,5]

#(4) call the package that contains the regression function 
library(FNN)

#(*) test on when k=3 using function "knn.reg"
knn3=knn.reg(train.feature, test.feature, train.y, k = 3)
knn3$pred

#(*) MSE
sum((knn3$pred-test.y)^2)/100   #dont forget "sum"

#(5) load package to plot
library(caret)

#(5) get the MSE data for each "k" using the train function with methods of "knn" and "cv"
set.seed(1)
knnreg.caret = train(    #function "train" returns the MSE number for each "k" 
  train.feature,train.y,
  method = "knn",    #just specify the function used is "knn"
  trControl = trainControl(method = "cv", number = 5),   #"cv" = cross validation   #number = 5-fold
  tuneGrid = expand.grid(k = seq(1, 21, by = 2))    #no need to create a vector for different "k", just expand the x-axis
)

knnreg.caret$modelType   #???

#(6) plot 
plot(knnreg.caret)   #???

knnreg.caret$finalModel

#(7) calculate MSE
sum((predict(knnreg.caret,test.feature)-test.y)^2)/100 



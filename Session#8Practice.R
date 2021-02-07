### R EXERCISE 8 Neutral Network
##1. Neural Network for Classification 
#(1) install packages
install.packages("ISLR")
library(ISLR)
install.packages("nnet")
library(nnet)
attach(Smarket)

#(2) separate training and test 
train=(Year<2005)    #the entire dataset
train.X= Smarket [train ,2:7]    #training features
train.Y=Direction[train]      #training labels
test.X= Smarket [!train ,2:7]    #test features 
test.Y=Direction[!train]      #test labels

#(3) convert training labels to one hot vector 
train.Y=class.ind(train.Y)    #function "class.ind" to convert the training labels to one-hot vector

#(4) fit a neural net with 10 hidden units
set.seed(2)
nnModel1=nnet(train.X, train.Y, size = 10, rang=0.1, maxit = 500,entropy=TRUE)

#(5) transform probabilities to class labels "down" and "up" 
pred1_prob=predict(nnModel1,test.X)
pred1=rep("Down",length(test.Y))   #first label all test instances as down 
pred1[pred1_prob[,2]>0.5]="Up"   #replace "down" for "up" 

#(6) prediction accuracy 
mean(test.Y==pred1)    #let the already known test labels compare with the empirical labels

#(7) pair different initial values with corresponding accuracy rate 
r=c(0.01,0.1,0.2,0.3,0.4)   #initial values
acc=vector("numeric",length(r))
set.seed(2)
for(ii in 1:length(r)){    #for loop to try each initial value
  nnModel1=nnet(train.X, train.Y, size = 10, rang=r[ii], maxit = 500,entropy=TRUE)
  pred1_prob=predict(nnModel1,test.X)
  pred1=rep("Down",length(test.Y))
  pred1[pred1_prob[,2]>0.5]="Up"
  acc[ii]=mean(test.Y==pred1)    #get the accuracy rate for this particular initial value
}

#(8) plot accuracy rate against initial value 
plot(c(1:5),acc,lty=1,lwd=2,type="b")   #plot initial value against accuracy

#(9) pair different number of hiden units with corresponding accuracy rate 
r=c(2,3,5,7,9,10,15,20)
acc=vector("numeric",length(r))
set.seed(2)
for(ii in 1:length(r)){
  nnModel1=nnet(train.X, train.Y, size = r[ii], rang=0.01, maxit = 500,entropy=TRUE)
  pred1_prob=predict(nnModel1,test.X)
  pred1=rep("Down",length(test.Y))
  pred1[pred1_prob[,2]>0.5]="Up"
  acc[ii]=mean(test.Y==pred1)}  

#(10) plot accuracy rate agianst number of hidden units
plot(c(1:length(r)),acc,lty=1,lwd=2,type="b")


##2. Neural Network in Keras
#(1) install packages
install.packages("devtools")
devtools::install_github("rstudio/keras")
library(keras)
install_keras()
library(keras)

#(2) separate taining data with test data
train=(Year<2005)
train.X= Smarket [train ,2:7]
train.Y=Direction[train]
test.X= Smarket [!train ,2:7]
test.Y=Direction[!train]

#(3) transform labels to one hot vectors
train.Y=ifelse(train.Y=="Up",0,1)
test.Y=ifelse(test.Y=="Up",0,1)
train.Y=to_categorical(train.Y, 2)
test.Y=to_categorical(test.Y, 2)

#(4) set up keras model
model <- keras_model_sequential()   #specify which keras model in use
model %>%    #this sign links all the commands under this model 
  layer_dense(units = 6, activation = 'tanh', input_shape = ncol(train.X)) %>%  #input_shape: number of input units
  #units = 6: number of hidden units 
  layer_dense(units = 2, activation = 'softmax')   #number of output units
summary(model)

#(5) compile the model
model %>% compile(    #function: compile
  loss = 'categorical_crossentropy',   
  optimizer = optimizer_rmsprop(),    #specify hwo to solve the minimizing function to reduce the loss 
  metrics = c('accuracy')
)

#(6) fit the model
set.seed(2)
history <- model %>% fit(
  as.matrix(train.X), train.Y,    #function "as.metrix" to transform it to matrix
  epochs = 10,
  validation_split = 0.3
)

#(7) plot 
plot(history)

#(8) get accuracy on test data
model %>% evaluate(as.matrix(test.X), test.Y)   #function: evaluate to get the accuracy rates 


##3. Neural Networks for Dimension Reduction (Using PCA)
#(1) activate the library
library(ISLR)

#(2) get gene data
nci.labs=NCI60$labs
nci.data=NCI60$data

#(3) train the PCA model 
pr.out=prcomp(nci.data, scale=TRUE)   #feed in features

#(4) set up plotting characters beforehand 
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))

#(5) plot 
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,
     xlab="Z1",ylab="Z2")

#(6) Neural network with one hidden layer
model <- keras_model_sequential()   
model %>%
  layer_dense(units = 2, activation = 'linear', input_shape = ncol(nci.data),name = "subspace") %>%
  layer_dense(units = ncol(nci.data), activation = 'linear')
summary(model)

#(7) compile the model
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = "adam"
)
history <- model %>% fit(
  as.matrix(nci.data), as.matrix(nci.data),
  epochs = 80
)

#(8) Neural network with one hidden layer
subspace <- keras_model(inputs = model$input, outputs = get_layer(model, "subspace")$output)
projection1 <- predict(subspace, nci.data)   #project the input layer to subspace layer (lower dimension) 

#(9) plot 
plot(projection1, col=Cols(nci.labs), pch=19,
     xlab="W1",ylab="W2")




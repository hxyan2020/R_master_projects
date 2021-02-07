### R EXERCISE 4 LDA
##1. LDA Function And Prediction Accuracy
#(1) import package that contains LDA function 
install.packages(MASS)
library(MASS)

#(2) get the taining data
n=25 
NN=dim(iris)[1]/3

set.seed(983)
index_s=sample(which(iris$Species=="setosa"),n)
index_c=sample(which(iris$Species=="versicolor"),n)
index_v=sample(which(iris$Species=="virginica"),n)

train_rand = rbind(iris[index_s,], iris[index_c,], iris[index_v,])

#(3) get the test data
test_rand = rbind(iris[-c(index_s,index_c,index_v),])

#(4) get class factor for training data
train_label= factor(c(rep("s",n), rep("c",n), rep("v",n)))

#(5) get class factor for test data
test_label_true=factor(c(rep("s",NN-n), rep("c",NN-n), rep("v",NN-n)))

#(6) fit the LDA model 
fit1=lda(Species~.,data=train_rand)   #coloumn 5 is Species the LABEL, getting from the data "train_rand" 
fit2=lda(train_rand[,-5],train_rand[,5])   #same result as above 
fit1   #the better fit (better direction)
#The prior probabilities are estimated as the class proportions for the training set.
#Coefficients of linear discriminants are the two directions we find using LDA.

#(7) predict the label with the beeter directio
pred=predict(fit1,test_rand[,-5])   #-5 means all other features (other than Species, which is the label to predict)
pred

#(8) get the prediction accuracy rate 
acc = mean(test_rand[,5]==pred$class)   #condition: the label predicted (pred$class) = the original label 
acc



#---------——------------------------------------------------------
##2. [Alternative]LDA Function Using Caret Package
#(1) import the "caret"package that contains LDA function 
library(caret)

#(2) fit the LDA model 
ldaFit=train(train_rand[,-5],train_rand[,5],method="lda",   #method here is not "knn"
             trControl=trainControl(method = "none"))
ldaFit$finalModel

#(3) use the trained model to predict 
pred2=predict(ldaFit,test_rand[,-5])

#(4) get the accuracy rate 
acc2=mean(pred2==test_rand[,5])
acc2

#The "lda" function uses all C - 1 directions for classification, so there’s no parameter to be tuned in the
#training phase. However, we don’t have to use all C ≠ 1 directions. The lda2 function in the caret package
#allows us to tune this parameter, i.e. the number of directions to use for classification.

#(5) use the mothod "lda2" to train the model 
#set up the "fitControl" specification beforehand
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",  #repeat cross validation 
  number = 10,  #10-fold
  repeats = 5)   #repeat 5 times 

#the lda2 method to train the model 
set.seed(836)
lda2Fit=train(train_rand[,-5],train_rand[,5],method="lda2",   #method here is "lda2"
              trControl=fitControl)

#(6) use the trained model to predict
pred3=predict(lda2Fit,test_rand[,-5])

#(7) calculate accuracy rate 
acc3=mean(pred3==test_rand[,5])
acc3



#---------——------------------------------------------------------
##3. Use Shiny To Show Different Projection Of Dataset 
#(1) activate package
library(shiny)
library(shinythemes)

#(2) set up the ui details
#(2.1) theme
ui <- fluidPage(theme = shinytheme("lumen"),
                #(2.2) title
                titlePanel("Projection of iris data"),
                #(2.3) sidebar
                sidebarLayout(
                  sidebarPanel(
                    #(2.4) select type of projection to plot
                    selectInput(inputId = "type", label = strong("Projection type"),
                                choices = c("PCA","LDA"),
                                selected = "PCA")
                  ),
                  #(2.5) Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "projection")
                  )
                )
)

#(3) define server logic required to draw a projection plot ----
server <- function(input, output) {
  output$projection <- renderPlot({
    #data 
    data(iris)
    if(input$type=="LDA"){
      library(MASS)
      fit=lda(Species~.,data=iris)
      iris_proj=as.matrix(iris[,-5])%*%(fit$scaling)
    }else{
      pr.out = prcomp(iris[,-5], scale = TRUE)
      iris_proj=pr.out$x[,1:2]
    }
    #colours
    cols<- c("steelblue1", "hotpink", "mediumpurple")
    #
    pchs<-c(1,2,3)
    #projection plot
    plot(iris_proj[1:50,1],iris_proj[1:50,2],pch=1,
         xlim=c(-12,10),xlab=" ",ylab=" ",
         cex.lab=1.5, cex=1.5,
         cex.axis=1.5, font.lab=2,col="steelblue1")
    #title 
    if(input$type=="LDA"){
      title(xlab="LD1",ylab="LD2")
    }else{
      title(xlab="PC1",ylab="PC2")
    }
    #points 
    points(iris_proj[51:100,1],iris_proj[51:100,2],pch=2,col="hotpink")
    points(iris_proj[101:150,1],iris_proj[101:150,2],pch=3,col="mediumpurple")
    #legend 
    legend("bottomright",legend=c("setosa","versicolor","virginica"),
           col=cols,pch=pchs,cex=1,text.font=2)
  })
}

#(4) call the function 
shinyApp(ui = ui, server = server)



#---------——------------------------------------------------------
##4. LDA as a dimension reduction/distance metric learning method
#(1) load package and the dataset
library(caret)
data(GermanCredit)

str(GermanCredit[, 1:10])  # classify two status: good or bad

#(2) Data tidiness 
#delete two variables where all values are the same for both classes
GermanCredit[,c("Purpose.Vacation","Personal.Female.Single")] <- list(NULL)   #function "list(NULL)

#(3) create training and test sets
set.seed(12)
trainIndex = createDataPartition(GermanCredit$Class, p = 0.7, list = FALSE, times = 1)   #???
# training features
train.feature=GermanCredit[trainIndex,-10]   #apart from "class", all other information are features
# training labels
train.label=GermanCredit$Class[trainIndex] 
# test features
test.feature=GermanCredit[-trainIndex,-10] 
# test labels
test.label=GermanCredit$Class[-trainIndex]    #label is the "class"

#(4) set up train control for later use 
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,     #10-fold CV
  repeats = 5)    #repeated five times

#(5) training process (fit the model)
set.seed(5)
knnFit=train(train.feature,train.label, method = "knn",
             trControl = fitControl,
             metric = "Accuracy",    #metric = performance measurement
             preProcess = c("center","scale"),    #???
             tuneLength=10)    #try 10 "k" to tune 
knnFit

#(6) Plot
plot(knnFit)

#(7) once the model is tuned with 10-fold cross validation, now begins the test process
pred=predict(knnFit,test.feature)   #fit in the test features, use the tuned model "knnfit" to predict

#(8) accuracy rate with ONLY knn method
acc=mean(pred==test.label)
acc

#(9) not use LDA to reduce dimension first, then use knn to predict: see if accuracy rate improves 
fit=lda(train.feature,train.label)  #use LDA to tune the model (feed in training label and feature)

#(10) Warning in lda.default(x, grouping, ...): variables are collinear (ignore first)
train.feature.proj=as.matrix(train.feature)%*%(fit$scaling)   #function "as.matrix" 
test.feature.proj=as.matrix(test.feature)%*%(fit$scaling)    #???

#(11) now try using knn again to predict
# set up train control
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5)
# training process
set.seed(5)
knnFit=train(train.feature.proj,train.label, method = "knn",
             trControl = fitControl,
             metric = "Accuracy",
             preProcess = c("center","scale"),
             tuneLength=10)
knnFit

#(12) plot 
plot(knnFit)

#(13) test process after LDA dimension reduction 
pred=predict(knnFit,test.feature.proj)

#(14) new accuracy rate
acc=mean(pred==test.label)
acc    #accuracy rate improved



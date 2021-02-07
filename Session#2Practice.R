### R EXERCISE 2: KNN METHOD
#1. The Basic KNN Method 
#(1) Install packages
install.packages("class")   #the KNN function is inside this
install.packages("caret")
install.packages("FNN")

library("caret")
library("FNN")  #everytime you run this script, need to run this "library" function (no need to reinstall)
library("class")


#(2) get training and test set
#use the first 25 data from each layer (75 data in total) as training data
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])  #[row, column, class]  
#use the next 25 data from each layer (75 data in total) as test data
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])

#(3) get class factor
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))    
#rep = repeat: to repeat the string "s" 25 times, then "c" concatenate them, stored in variable "cl" 

#(4) classification using knn, with k=3
set.seed(47)  #use set.seed before knn() to make the results reproducible
knn_pred=knn(train, test, cl, k = 3, prob=TRUE)  #to get the probability of being classified into the class
# see the attributes of the knn model
attributes(knn_pred)



#----------------------------------------------------------------------
##2. KNN Method With Different "k": 
# when k = 5
knn_pred=knn(train, test, cl, k = 5, prob=FALSE)
knn_pred

attributes(knn_pred)  
#probabilities associated with the predicted class, i.e. only the LARGEST probabilities are returned

#to get another prediction
knn_pred2=knn(train, test, cl, k = 5, prob=FALSE)

#when k = 3
set.seed(47)
knn3_pred=knn3Train(train, test, cl, k = 3, prob=TRUE)
attributes(knn3_pred)

#[Alternative] Use build-in function "knn1" to get 1NN 
set.seed(47)
knn1_pred=knn1(train, test, cl)
knn1_pred

#[Alternative] use function "knn3Train"to see the prediction probability for all classes
set.seed(47)
knn3_pred=knn3Train(train, test, cl, k = 3, prob=TRUE)
attributes(knn3_pred)



#----------------------------------------------------------------------
#3.Standardise Data 
#make sure the Euclidean distance not dominated by the variables with large scales.
#(1) use the messy data (before standardizing) > Suppose we change Sepal.width to mm while others to m
iris_c=iris[,1:4]   #get the data with 4 feaatures
iris_c[,2]=iris[,2]*10     #change the feature Sepal.width to m
iris_c[,-2]=iris[,c(1,3,4)]/10     #other features to mm

summary(iris_c)

#(2) get training data
n=25 # the number of training data in each class
NN=dim(iris)[1]/3 # the total number of observations in each class (training + test) 

set.seed(983)
index_s=sample(which(iris$Species=="setosa"),n)  #function "sample" is to make sure the sampling is random
index_c=sample(which(iris$Species=="versicolor"),n)
index_v=sample(which(iris$Species=="virginica"),n)

train_rand_c = rbind(iris_c[index_s,], iris_c[index_c,], iris_c[index_v,])   #iris_c is the entire data, index_c is the data for training

#(3) get test data
test_rand_c = rbind(iris_c[-c(index_s,index_c,index_v),])  #use - the minus sign to represent the rest of data

#(4) class factor for training data
train_label= factor(c(rep("s",n), rep("c",n), rep("v",n)))

#(5) get class factor for test data
test_label_true=factor(c(rep("s",NN-n), rep("c",NN-n), rep("v",NN-n))) #NN-n 

#(6) classification using knn, with k=5
kk=5 # number of nearest neighbours
set.seed(275)
knn_pred=knn(train=train_rand_c,test=test_rand_c,cl=train_label, k=kk, prob=FALSE)
knn_pred

#(7)now use the data being standardised (function of "scale")
iris_s=scale(iris_c)  

#(8) get training and test set for standardised data
train_rand_s = rbind(iris_s[index_s,], iris_s[index_c,], iris_s[index_v,])
test_rand_s = rbind(iris_s[-c(index_s,index_c,index_v),])

#(9) get class factor for training data (standardised data)
train_label= factor(c(rep("s",n), rep("c",n), rep("v",n)))

#(10) get class factor for test data (standardised data)
test_label_true=factor(c(rep("s",NN-n), rep("c",NN-n), rep("v",NN-n)))

#(11) classification using knn, with k=5 (standardised data)
kk=5 
set.seed(275)
knn_pred=knn(train=train_rand_s,test=test_rand_s,cl=train_label, k=kk, prob=FALSE)
knn_pred



#----------------------------------------------------------------------
##4.Draw Classification Boundary
#(1) use the dataset "default"
install.packages("ISLR")
library(ISLR)

#(2) load data
str(Default)
summary(Default)

#(3) standardise the data using function "scale"
Default[,c(3,4)]=scale(Default[,c(3,4)])

#(4) separate the data by label "yes" or "no"
no=Default[Default$default=="No",]
yes=Default[Default$default=="Yes",]

#(5) scatter plot of the training data
plot(no$balance,no$income,pch=1,col="deepskyblue1",
     xlab="Balance",ylab="Income",cex.lab=1.5,cex.axis=1.5,font.lab=2)
points(yes$balance,yes$income,pch=3,col="red")    #present the points uding function "points"
legend(2.8,2.5, legend=c("No", "Yes"),
       col=c("deepskyblue1", "red"),pch=c(1,3),cex=1,text.font=4)

#(6) set up train.feature, train.label
train.feature=Default[,c(3,4)]   #use 2 columns in the dataset as deatures
train.label=Default$default    

#(7) set up the grid
new.x1=seq(-2,3.5,by=0.05)   #set the x axis
new.x2=seq(-2.5,3,by=0.05)   #set the y axis
new=expand.grid(new.x1,new.x2)   #funxtion "expand.grid" = concatenation

#(8) tun the knn classification function and get predictions when knn = 5
set.seed(12)
pred = knn(train.feature, new, train.label, k=5, prob=TRUE)

#(9) get P(Y=1|new)  given the test data "new", the probability of being classified into class #1
prob = attr(pred, "prob")
prob = ifelse(pred=="Yes", prob, 1-prob)

#(10) plot contour P(Y=1|X)=0.5 using function "contour"   
# the contour lies on when P = o.5
contour(new.x1, new.x2, matrix(prob, nrow = length(new.x1),ncol = length(new.x2)),   #fit into the grid generated 
        levels=0.5,label="", axes=FALSE, add=TRUE)



#----------------------------------------------------------------------
##6. Visualize KNN with Shiny 
#(1) import package
install.packages("shiny")
library("shiny")

#(2) define UI for app that draws a histogram using function "fluidPage" ----
ui <- fluidPage(
  #(2.1) App title ----
  titlePanel("Find k nearest neighbours"),
  #(2.2) Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "k",
                  label = "Number of nearest neighbours:",
                  min = 1,
                  max = 11,
                  value = 30,
                  step=2)   #make sure the knn chosen is odd number
    ),
    
    #(2.3) Main panel for displaying outputs using function "mainPanel"----
    mainPanel(
      # Output: scatter plot ----
      plotOutput(outputId = "sccaterPlot")
    )
  )
)

#(3) define server logic required to draw a histogram ----
  server <- function(input, output) {
    output$sccaterPlot <- renderPlot({   
      #(3.1) load training data
      set.seed(10)
      library(MASS)
      class1=mvrnorm(n = 30, mu=c(1,1), Sigma=matrix(c(1,0,0,1),2,2))
      class2=mvrnorm(n = 30, mu=c(2,2), Sigma=matrix(c(1,0,0,1),2,2))
      
      #(3.2) get train.feature and train.label 
      train.feature=rbind(class1,class2)
      train.label=c(rep(1,30),rep(2,30))
      #(3.3) scatter plot of the training data
      plot(train.feature,col=ifelse(train.label==1, "blue", "red"),
           pch=ifelse(train.label==1, 16, 17),xlab="x1",ylab="x2",cex=1.5)
      points(2.1,1.5,pch=15,col="green",cex=1.5) # test point
      
      #(3.4) find k nearest neighbours
      library("FNN")
      test.feature=t(c(2.1,1.5))
      k=input$k
      knearest=get.knnx(train.feature,test.feature, k)
      neighbour=train.feature[knearest$nn.index,]
      if(k==1){
        points(t(neighbour),pch=0,col="black",cex=1.8)}else{
          points((neighbour),pch=0,col="black",cex=1.8)
        }
    })
  }

#(4) run the Shiny app 
shinyApp(ui = ui, server = server)



#----------------------------------------------------------------------
##7. Visualize Boundary Classification in KNN
#(1) activate package
library(shiny)

#(2) define UI for app that draws a histogram ----
ui <- fluidPage(
  #(2.1) App title ----
  titlePanel("Classification boundary of k nearest neighbours"),
  #(2.2) Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "k",
                  label = "Number of nearest neighbours:",
                  min = 1,
                  max = 51,
                  value = 30,
                  step=5)
    ),
    #(2.3) Main panel for displaying outputs ----
    mainPanel(
      # Output: scatter plot ----
      plotOutput(outputId = "sccaterPlot")
    )
  )
)

#(3) define server logic required to draw a histogram ----
  server <- function(input, output) {
    output$sccaterPlot <- renderPlot({
      library(class)
      #(3.1) load training data
      simulate=read.table("simulate.txt",header=TRUE)
      train.feature=simulate[,-3]
      train.label=simulate[,3]
      ##(3.2) scatter plot of the training data
      plot(train.feature,col=ifelse(train.label==1, "blue", "red"),
           pch=ifelse(train.label==1, 16, 17))
      #(3,3) generate test data
      new.x1=seq(-3,4.5,by=0.1)
      new.x2=seq(-2,4,by=0.1)
      new=expand.grid(new.x1,new.x2)
      #(3,4) get predictions from 15-NN
      k=input$k
      pred = knn(train.feature, new, train.label,k, prob=TRUE)
      #(3.5) get P(Y=1|new)
      prob = attr(pred, "prob")
      prob = ifelse(pred=="1", prob, 1-prob)
      #(3.6) get contour P(Y=1|X)=0.5
      contour(new.x1, new.x2, matrix(prob, nrow = length(new.x1),ncol = length(new.x2)),
              levels=0.5,label="", axes=FALSE, add=TRUE)
    })
  }

#(4) call the app
shinyApp(ui = ui, server = server)

### R EXERCISE 7 SUPPORT VECTOR MACHINE
##1. Support Vector Machine
#(1) #install.packages("e1071")
install.packages(e1071)
library("e1071")

#(2) load data
data(iris)
attach(iris)

#(3) specify classification mode
model = svm(iris[,-5], iris[,5])   #feature first, label second
summary(model)

#(4) check the defult value of gamma 
#in this script gamma and sigma are interchangeable 
model$gamma

#(5) test with train data
pred = predict(model, iris[,-5])  #model is specified as SVM

#(6) Check accuracy
table(pred, iris[,5])

#(7) compute decision values/classification result
x = iris[, -5] #Check
pred = predict(model, x, decision.values = TRUE)  #function: predict > same as in other models
attr(pred, "decision.values")[1:4,]  

pred[1:4]

#(8) visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])

#[Alternative] show the probability
model = svm(iris[,-5], iris[,5], probability = TRUE)
pred = predict(model, iris[,-5], decision.values = TRUE, probability = TRUE)
attr(pred, "decision.values")[1:4,]

attr(pred, "probabilities")[1:4,]


##2 Tune the Parameters by Cross-validatiaon
#(1) tune 2 parameters: gamma and cost
set.seed(392)
tune.out = tune(svm, iris[,-5], iris[,5],
                ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),   #tune the parameters
                tunecontrol = tune.control(sampling = "cross"),cross=10)   #under tunecontrol, specify using cross-validation method, 10-fold validation
summary(tune.out)

#(2) plot
plot(tune.out)

#(3) use the best model to predict the training instances
tune.out$best.model

pred=predict(tune.out$best.model,iris[,-5])
table(pred, iris[,5])


##3 Use SVM in Caret
#(1) call the library
library("caret")

#(2) load german credit data from caret package
data(GermanCredit)     #classify two status: good or bad

#(3) data tydiness
str(GermanCredit[, 1:10])  #show the first 10 columns

#Delete two variables where all values are the same for both classes
GermanCredit[,c("Purpose.Vacation","Personal.Female.Single")] <- list(NULL)   #function list(NULL) to delete 

#(4) Get training and test sets
set.seed(12)
#speficy train index
trainIndex = createDataPartition(GermanCredit$Class, p = 0.7, list = FALSE, times = 1)
train=GermanCredit[trainIndex,] # training set
test=GermanCredit[-trainIndex,-10] # test set

#(5) train the model 
#set up fitcontrol 
fitControl=trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3)
#train the data
set.seed(2333)
svm.Radial=train(Class ~., data = train, method = "svmRadial",   #method could also be svmLinear, svmPoly
                 trControl=fitControl,
                 preProcess = c("center", "scale"),
                 tuneLength = 5)
svm.Radial

#(6) plot
plot(svm.Radial)

#[Alternative]If we would like to tune both parameters, C and sigma.
#use expand.grid to include more paramters that need to be trained
grid_radial=expand.grid(sigma = c(0.01, 0.1, 1,10),
                        C = c(0.01, 0.1, 1, 10))
#set up fitcontrol 
fitControl=trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3)
#train the data
set.seed(2333)
svm.Radialg=train(Class ~., data = train, method = "svmRadial",
                  trControl=fitControl,
                  preProcess = c("center", "scale"),
                  tuneGrid = grid_radial)  #specify that more than one parameters
svm.Radialg

#plot
plot(svm.Radialg)


##4 Visualise SVM
#(1) load shiny and packages
library(shiny)
library(shinythemes)
library(e1071)

#(2) Define UI for app that draws a histogram
ui <- fluidPage(
  #(2.1) App title 
  titlePanel("The effect of C in linear SVM"),
  #(2.2) Sidebar layout with input and output definitions 
  sidebarLayout(
    #(2.2.1) Sidebar panel for inputs 
    sidebarPanel(
      # Input: Slider for the number of bins
      sliderInput(inputId = "C",
                  label = "Value of C in Lagrangian formulation:",
                  min = 1,
                  max = 50,
                  value = 30,
                  step=1)
    ),
    
    #(2.2.2) Main panel for displaying outputs 
    mainPanel(
      # Output: scatter plot 
      plotOutput(outputId = "svmPlot")
    )
  )
)

#(3) Define server logic required to draw a histogram 
  server <- function(input, output) {
    output$svmPlot <- renderPlot({
      
      #(3.1) load training data
      set.seed(10111)
      x = matrix(rnorm(40), 20, 2)
      y = rep(c(-1, 1), c(10, 10))
      x[y == 1, ] = x[y == 1, ] + 1
      
      #(3.2) fit linear SVM
      library(e1071)
      dat = data.frame(x, y = as.factor(y))
      c=input$C
      svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = c, scale = FALSE)  #specify the kernel is linear kernel 
      
      #(3.3) plot support vectors
      make.grid = function(x, n = 75) {
        grange = apply(x, 2, range)
        x1 = seq(from = grange[1, 1], to = grange[2, 1], length = n)
        x2 = seq(from = grange[1, 2], to = grange[2, 2], length = n)
        expand.grid(X1 = x1, X2 = x2)
      }
      xgrid = make.grid(x)
      ygrid = predict(svmfit, xgrid)
      
      #(3.4) plot margin and classification boundary
      beta = drop(t(svmfit$coefs) %*% x[svmfit$index, ])
      beta0 = svmfit$rho
      
      plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = 0.2)
      points(x, col = y + 3, pch = 19)
      points(x[svmfit$index, ], pch = 5, cex = 2)
      #reference lines
      abline(beta0/beta[2], -beta[1]/beta[2])
      abline((beta0 - 1)/beta[2], -beta[1]/beta[2], lty = 2)
      abline((beta0 + 1)/beta[2], -beta[1]/beta[2], lty = 2)
    })
  }
#(4) call the shiny application 
shinyApp(ui = ui, server = server)

#[Alternative] Visualise SVM with different kernels.
#(1) load libraries
  library(shiny)
  library(shinythemes)
  library(e1071)

#(2) Define UI for app that draws a histogram 
  ui <- fluidPage(
    #(2.1) App title 
    titlePanel("The effect of kernels in SVM"),
    #(2.2) Sidebar layout with input and output definitions 
    sidebarLayout(
      # Sidebar panel for inputs 
      sidebarPanel(
        selectInput(inputId = "kernel", label = strong("Kernel"),  #select kernel 
                    choices = c("linear","polynomial","RBF"),
                    selected = "linear"),
        sliderInput(inputId = "C",   #select C
                    label = "Value of C in Lagrangian formulation:",
                    min = 1,
                    max = 50,
                    value = 30,
                    step=1),
        sliderInput(inputId = "gamma",   #select gamma 
                    label = "Value of Gamma:",
                    min = 0.01,
                    max = 1,
                    value = 30,
                    step=0.02),
        sliderInput(inputId = "degree",   #select degree for polynomial kernel 
                    label = "Value of Degree:",
                    min = 1,
                    max = 6,
                    value = 30,
                    step=1)
      ),
      
      # Main panel for displaying outputs 
      mainPanel(
        # Output: scatter plot 
        p("Linear: C"),
        p("Polynomial: C, degree, gamma"),
        p("RBF: C, gamma"),
        plotOutput(outputId = "svmPlot")
      )
    )
  )

#(3) Define server logic required to draw a projection plot 
  server <- function(input, output) {
    output$svmPlot <- renderPlot({
      
      #(3.1) load data
      load(url("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/ESL.mixture.rda"))
      10   #able to load data from web 
      dat = data.frame(y = factor(ESL.mixture$y), ESL.mixture$x)
      
      #(3.2) fit svm
      k=input$kernel
      c=input$C; gamma=input$gamma; degree=input$degree
      if(k=="linear"){    #option: linear kernel or polynomial kernel or radial kernel (3 options)
        fit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "linear", cost = c)
      }else if(k=="polynomial"){
        fit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "poly", degree=degree, gamma=gamma, cost }else{
          fit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = c, gamma=gamma)
        }
      
      #(3.3) plot classification boundary
      px1=ESL.mixture$px1; px2=ESL.mixture$px2
      x=ESL.mixture$x; y=ESL.mixture$y
      xgrid = expand.grid(X1 = px1, X2 = px2)
      ygrid = predict(fit, xgrid)
      
      plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2)
      points(x, col = y + 1, pch = 19)
      func = predict(fit, xgrid, decision.values = TRUE)
      func = attributes(func)$decision
      
      xgrid = expand.grid(X1 = px1, X2 = px2)
      ygrid = predict(fit, xgrid)
      
      plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2)
      points(x, col = y + 1, pch = 19)
      contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
    })
  }

#(4) call the shiny application 
shinyApp(ui = ui, server = server)      



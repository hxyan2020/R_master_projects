### R EXERCISE 1
## 1. The Pairs Plot For The Iris Data
#(1)load data
data()    #get a list of all usable datasets in R
help(iris)
str(iris)  #structure of the data

table(iris$Species)  #Species is one of the variables

summary(iris)

#(2)set colours and symbols
cols <- c("steelblue1", "hotpink", "mediumpurple") #set colours; use "c" concatenation 
pchs <- c(1,2,3)  #set symbols; use "c" concatenation

#(3)plot 
pairs(iris[,1:4], pch = pchs[iris$Species], cex = 1,  #pch is to call the symbols defined earlier;   #function "paris" to set pairs plot 
      col = cols[iris$Species],    #cant just write cols(need to specify the data this function is applying to)
      lower.panel=NULL)   #cex tells you how big to magnify the text, the default is 1


#(4)legend
par(xpd = TRUE) # enables you to add a legend outside of the plot region
legend("bottomleft",legend=c("setosa","versicolor","virginica"),
       col=cols,pch=pchs,cex=0.5,text.font=2)


#-------------------------------------------------------
##2. Alternative To The Iris Pairing Methods using dev.new 
#(1)open a new plot device
dev.new(width=5, height=4, unit="in")

#(2)plot (with specifications previously set)
pairs(iris[,1:4], pch = pchs[iris$Species], cex = 1,
      col = cols[iris$Species],
      lower.panel=NULL)

#(3)legend
par(xpd = TRUE) # enables you to add a legend outside of the plot region
legend("bottomleft",legend=c("setosa","versicolor","virginica"),
       col=cols,pch=pchs,cex=1,text.font=4)



#-------------------------------------------------------
##3. Alternative 2 To The Iris Pairing Methods using pdf
#(1) use pdf() to open a new pdf device
pdf("iris-plot.pdf",width=7, height=5)  #automatically saved to the current folder

#(2)plot
pairs(iris[,1:4], pch = pchs[iris$Species], cex = 1,
      col = cols[iris$Species],
      lower.panel=NULL)

#(3)legend
par(xpd = TRUE) 
legend("bottomleft",legend=c("setosa","versicolor","virginica"),
       col=cols,pch=pchs,cex=1,text.font=4)

#(4)close the device
dev.off()



#-------------------------------------------------------
##4. Alternative To Add Legend Using "oma"
#(1) oma allows you to create some spare space for the legend
pairs(iris[,1:4], pch = pchs[iris$Species], cex = 1, 
      col = cols[iris$Species], oma = c(4,4,6,12))   # here we make the right margin quite large

#(2) legend
par(xpd = TRUE) 
legend(0.85, 0.3,legend=c("setosa","versicolor","virginica"),
       col=cols,pch=pchs,cex=1,text.font=2)



#-------------------------------------------------------
##5. Create Subset From Data
#(1) import data 
Default=read.table("Default.txt",header=TRUE)
str(Default) # structure of the Default data

table(Default$default) 

summary(Default)  
#The first variable default indicates whether the customer is default or not, 
#the second variable student indicates whether the customer is a student or not.

#(2) create subset of data (100 observations), 50 default:"yes", 50 default="no"
# get the first 50 default=No
default_no_all=Default[Default$default=="No",]   #get all default="no"
default_no_50_1=default_no_all[1:50,]

#[Alternative] to create subset of data
default_no_50_2=Default[Default$default=="No",][1:50,]
#use function "sum" to check whether the 2 methods give the same result
sum(default_no_50_1[,3]!=default_no_50_2[,3])

# get the first 50 default=Yes
default_yes_50 = Default[Default$default=="Yes",][1:50,]

#(3) combine default_no_50 and default_yes_50
default_no_50 = default_no_50_2
default_50 = rbind(default_no_50,default_yes_50)

#(4) [Alternative] Use random generator to get random data, instead of the first 50 of "Yes" or "No"
#generate random items 
?set.seed
#tTo reproduce the random sampling results, so we have to add set.seed( ) before any random 
#sampling function. 

#The number in the bracket can be any integer. Next time if you want to obtain
#the exactly the same random sampling result, use the same integer.

set.seed(375) # make the random sampling reproducible
default_no_rand=Default[sample(which(Default$default=="No"),50),]
default_yes_rand=Default[sample(which(Default$default=="Yes"),50),]   #gunction "which" 
?which
which(Default$default=="Yes")

#concatenate the 50 "Yes" and 50 "No" 
?sample
default_rand = rbind(default_no_rand,default_yes_rand)

#(5) [Alternative] to make life easier
n=50   #set this number upfront, so to save time if not 50 next time
set.seed(375) 
default_no_rand=Default[sample(which(Default$default=="No"),n),]
default_yes_rand=Default[sample(which(Default$default=="Yes"),n),]
default_rand = rbind(default_no_rand,default_yes_rand)

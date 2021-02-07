### R EXERCISE 9 Unsupervised Learning (Part 1)
##1. Market Basket Analysis 
#(1) install packages
install.packages("readxl")
library(readxl)
library(plyr)
library(dplyr)

install.packages("arules")
library(arules)

install.packages("arulesViz")
library(arulesViz)

#(2) load data
retail=read_excel('Online_retail.xlsx')

#(3) data tidiness: remove missing values
retail=retail[complete.cases(retail), ]

#(4) data tidiness: transform data to proper formats
retail$InvoiceNo=gsub("C5", "5", retail$InvoiceNo)
retail$InvoiceNo=as.numeric(as.character(retail$InvoiceNo))
retail$Description=gsub(",", " ", retail$Description)
retail$Description = as.factor(retail$Description)
retail$Country = as.factor(retail$Country)
retail$Date=as.Date(retail$InvoiceDate)
retail$Time=format(retail$InvoiceDate,"%H:%M:%S")

#(5) transform the data from data frame to transactions
transaction <- ddply(retail,c("InvoiceNo","Date"),
                     function(df1)paste(df1$Description,
                                        collapse = ","))
transaction=transaction[,3]
write.table(transaction,"transactions.csv", quote = FALSE,
            row.names = FALSE,col.names = FALSE)

#(6) read the transaction data for association rules analysis
transc = read.transactions('transactions.csv', header=FALSE,
                           format = 'basket', sep=",",
                           quote="",rm.duplicates = TRUE)

summary(transc)

#(7) get the plot of the most frequently bought items
itemFrequencyPlot(transc, topN=20, type='absolute')

#(8) generate assosication rules
association.rules <- apriori(transc, parameter = list(supp=0.01, conf=0.8,maxlen=10))

summary(association.rules)
inspect(association.rules[1:5])

#(9) to know what customers buy before buying WOODEN STAR CHRISTMAS SCANDINAVIAN
woodenR.association.rules=apriori(transc, parameter = list(supp=0.01, conf=0.8),
                                  appearance = list(default="lhs",rhs="WOODEN STAR CHRISTMAS SCANDINAVIAN"))
                           
inspect(woodenR.association.rules)

#(10) to know customers bought SET/6 RED SPOTTY PAPER CUPS first also bought
redL.association.rules=apriori(transc, parameter = list(supp=0.01, conf=0.7),
                               appearance = list(default="rhs",lhs="SET/6 RED SPOTTY PAPER CUPS"))

inspect(redL.association.rules)

#(11) get interactive plots
rules=head(association.rules, n = 10, by = "confidence")
plot(rules, method = "graph", engine = "htmlwidget")


##2. Model-based clustering
#(1) install packages
install.packages("MASS")
library(MASS)
install.packages("mclust")
library(mclust)

#(2) overview of data
str(geyser)

#(3) plot 
plot(geyser[,2:1], col = "red", pch = 16,
     ylab = "Waiting time", xlab = "Eruption time",
     cex=1.5,cex.axis=1.5,cex.lab=1.5)

#(4) apply clustering model to the data
mod1=Mclust(geyser)

summary(mod1,parameters = TRUE)

#(5) plot the clustering, uncertainty and density
par(mfrow = c(2, 2))
plot(mod1,what="BIC")
plot(mod1,what="classification")
plot(mod1,what="uncertainty")
plot(mod1,what="density")

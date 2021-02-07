### R EXERCISE 10 UNSUPERVISED LEARNING (PART TWO)
##1. Independent component analysis 
#(1) get source matrix
S <- cbind(sin((1:1000)/20), rep((((1:200)-100)/100), 5))   #first signal source and second signal source

#(2) get mixing matrix
M <- matrix(c(0.291, 0.6557, -0.5439, 0.5572), 2, 2)

#(3) plot source signals
par(mfrow = c(1, 2))
plot(1:1000, S[,1], type = "l",xlab = "source 1",ylab="",cex.lab=1.5,cex.axis=1.5,lwd=2)
plot(1:1000, S[,2], type = "l", xlab = "source 2",ylab="",cex.lab=1.5,cex.axis=1.5,lwd=2)

#(4) mix the source signals to get the observed signals X
X <- S %*% M   #mix the 2 source signals together
par(mfrow = c(1, 2))
plot(1:1000, X[,1], type = "l",xlab = "observation 1",ylab="",cex.lab=1.5,cex.axis=1.5,lwd=2)
plot(1:1000, X[,2], type = "l", xlab = "observation 2",ylab="",cex.lab=1.5,cex.axis=1.5,lwd=2)

#(5) now apply ICA to see the difference 
install.packages("fastICA")
library(fastICA)

set.seed(20)
latent = fastICA(X, 2, fun = "logcosh", alpha = 1,   #2 means 2 undelying sourses
                 row.norm = FALSE, maxit = 200,
                 tol = 0.0001, verbose = TRUE)

#(6) the estimated source signals are in S, resulting deom ICA
par(mfrow = c(1, 2))
plot(1:1000, latent$S[,1], type = "l", xlab = "Esource 1",ylab="",cex.lab=1.5,cex.axis=1.5,lwd=2)
plot(1:1000, latent$S[,2], type = "l", xlab = "Esource 1",ylab="",cex.lab=1.5,cex.axis=1.5,lwd=2)


##2. Kernel Principal Component Analysis 
#(1) load data
flame=read.table("flame.txt")

#(2) plot the original data "flame"
plot(flame[flame$V3==1,1],flame[flame$V3==1,2],cex=1.5,
     col="red",pch=16,xlim=c(0.5,14.2),ylim=c(14.45,27.80),
     xlab="X1",ylab="X2")
points(flame[flame$V3==2,1],flame[flame$V3==2,2],cex=1.5,col="blue",pch=17)

#(3) apply PCA, which cannot separate the two clusters
pr.out=prcomp(flame[,1:2], scale=TRUE)    #function: prcomp
plot(pr.out$x[flame$V3==1,1:2],col="red",pch=16,cex=1.5,
     xlab="PC1",ylab="PC2",xlim=c(-2.65,1.69),ylim=c(-2.06,1.77))
points(pr.out$x[flame$V3==2,1:2],cex=1.5,col="blue",pch=17)

#(4) now use kernel PCA
install.packages("kernlab")
library(kernlab)

par(mfrow = c(1, 4))
#(4.1) try sigma=0.01
kpc=kpca(~.,data=flame[,-3],kernel="rbfdot",    #function: kpca   #flame[,-3] is the label   #using RBF kernel method
         kpar=list(sigma=0.01),features=2)
plot(rotated(kpc)[flame$V3==1,1:2],col="red",pch=16,cex=1.5,
     xlab="KPC1",ylab="KPC2",xlim=c(-8.80,10.06),ylim=c(-7.0,10.70),
     main="sigma=0.01")
points(rotated(kpc)[flame$V3==2,1:2],cex=1.5,col="blue",pch=17)

#(4.2) sigma=0.1
kpc=kpca(~.,data=flame[,-3],kernel="rbfdot",
         kpar=list(sigma=0.1),features=2)   #under kpar = list() to specify value of sigma
plot(rotated(kpc)[flame$V3==1,1:2],col="red",pch=16,cex=1.5,
     xlab="KPC1",ylab="KPC2",xlim=c(-8.80,10.06),ylim=c(-7.0,10.70),
     main="sigma=0.1")
points(rotated(kpc)[flame$V3==2,1:2],cex=1.5,col="blue",pch=17)

#(4.3) sigma=0.5
kpc=kpca(~.,data=flame[,-3],kernel="rbfdot",
         kpar=list(sigma=0.5),features=2)
plot(rotated(kpc)[flame$V3==1,1:2],col="red",pch=16,cex=1.5,
     xlab="KPC1",ylab="KPC2",xlim=c(-8.80,10.06),ylim=c(-7.0,10.70),
     main="sigma=0.5")
points(rotated(kpc)[flame$V3==2,1:2],cex=1.5,col="blue",pch=17)

#(4.4) sigma=1
kpc=kpca(~.,data=flame[,-3],kernel="rbfdot",
         kpar=list(sigma=1),features=2)
plot(rotated(kpc)[flame$V3==1,1:2],col="red",pch=16,cex=1.5,
     xlab="KPC1",ylab="KPC2",xlim=c(-8.80,10.06),ylim=c(-7.0,10.70),
     main="sigma=1")
points(rotated(kpc)[flame$V3==2,1:2],cex=1.5,col="blue",pch=17)


##3. Isometric Feature Mapping 
#(1) install packages 
install.packages("RDRToolbox")
library(RDRToolbox)
library(rgl)
library(KODAMA)

#(2) get the swiss roll data
x=swissroll()   #data available in R package

#(3) plot the original data
labels=c(rep(1,250),rep(2,250),rep(3,250),rep(4,250))
cols=c("black","red","blue","green")
open3d()   #plot it in 3D
plot3d(x, col=cols[as.numeric(as.factor(labels))],box=FALSE,size=3)
rgl.postscript("swissroll.pdf", fmt="pdf")

#(4) apply isomap
swissIsomap = Isomap(data=x, dims=2, k=10)   #to reduce the dimension to 2, knn=3
plotDR(data=swissIsomap$dim2, labels=(labels),axesLabels=c("", ""), 
       legend=TRUE) 
title(main="k=10")

#(5) get residual plot
swissResidual = Isomap(data=x, dims=1:3, k=10, plotResiduals=TRUE)


##4. EXERCISE: Dimension Reduction Using NCI60 Cancer cell Line Microarray Data
#(1) install packages
library(ISLR)
library(rgl)

#(2) get gene data
nci.labs=NCI60$labs 
nci.data=NCI60$data

#(3) apply PCA (cant separate the clusters)
pr.out=prcomp(nci.data, scale=TRUE)

#(4) PC plots
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,
     xlab="PC1",ylab="PC2",cex.lab=1.5,cex.axis=1.5)

#(5) 3D plot
open3d()
plot3d(pr.out$x[,1:2], col=Cols(nci.labs),box=FALSE,size=3,
       xlab="PC1",ylab="PC2",zlab="PC3")
rgl.postscript("gene-pca-3d.pdf", fmt="pdf")

#(6) apply Kernel principal component analysis and plot it
library(kernlab)

kpc=kpca(nci.data,kernel="rbfdot",
         kpar=list(sigma=0.0005),features=2)   #try sigma=0.005
plot(rotated(kpc), col=Cols(nci.labs), pch=19,
     xlab="KPC1",ylab="KPC2",cex.lab=1.5,cex.axis=1.5,main="sigma=0.0005")

#(7) 3D plot
kpc=kpca(nci.data,kernel="rbfdot",
         kpar=list(sigma=0.0005),features=3)

open3d()
plot3d(rotated(kpc), col=Cols(nci.labs),box=FALSE,size=3,
       xlab="KPC1",ylab="KPC2",zlab="KPC3")
rgl.postscript("gene-kpca-3d.pdf", fmt="pdf")

#(7) Isometric feature mapping and plot it 
library(RDRToolbox)

swissIsomap = Isomap(data=nci.data, dims=2, k=10)
plot(swissIsomap$dim2, col=Cols(nci.labs), pch=19,
     xlab="ISOF1",ylab="ISOF2",cex.lab=1.5,cex.axis=1.5,main="k=10")

#(8) 3D plot
swissIsomap = Isomap(data=nci.data, dims=3, k=10)
open3d()
plot3d(swissIsomap$dim3, col=Cols(nci.labs),box=FALSE,size=3,
       xlab="ISOF1",ylab="ISOF2",zlab="ISOF3")
rgl.postscript("gene-isomap-3d.pdf", fmt="pdf")

#(9) Independent component analysis and plot it 
library(fastICA)

set.seed(20)
latent = fastICA((nci.data), 2, fun = "logcosh", alpha = 1,
                 row.norm = FALSE, maxit = 200, method="C",
                 tol = 0.001, verbose = TRUE)
plot(latent$S, col=Cols(nci.labs), pch=19,
     xlab="IC1",ylab="IC2",cex.lab=1.5,cex.axis=1.5)

#(10) 3D plot
latent = fastICA((nci.data), 3, fun = "logcosh", alpha = 1,
                 row.norm = FALSE, maxit = 200, method="C",
                 tol = 0.001, verbose = TRUE)
open3d()
plot3d(latent$S, col=Cols(nci.labs),box=FALSE,size=3,
       xlab="ISOF1",ylab="ISOF2",zlab="ISOF3")
rgl.postscript("gene-ica-3d.pdf", fmt="pdf")



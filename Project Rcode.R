t = read.table("Project Data.txt", sep = "\t", header = F)
colnames(t) = c("country", "sp100", "sp200", "sp400", "sp800", "sp1500", "sp3000", 
                "marathon")

t
rownames(t) = t$country
(t1 = t[, -1])
###draw box plot for each one
boxplot(t1$sp100,main = "Track Record for 100m",xlab = "sec", ylab = "sp100",col = "red",
        horizontal = TRUE,notch = TRUE)
boxplot(t1$sp200,main = "Track Record for 200m",xlab = "sec", ylab = "sp200",col = "brown",
        horizontal = TRUE,notch = TRUE)
boxplot(t1$sp400,main = "Track Record for 400m",xlab = "sec", ylab = "sp400",col = "yellow",
        horizontal = TRUE,notch = TRUE)
boxplot(t1$sp800,main = "Track Record for 800m",xlab = "min", ylab = "sp800",col = "pink",
        horizontal = TRUE,notch = TRUE)
boxplot(t1$sp1500,main = "Track Record for 1500m",xlab = "min", ylab = "sp1500",col = "grey",
        horizontal = TRUE,notch = TRUE)
boxplot(t1$sp3000,main = "Track Record for 3000m",xlab = "min", ylab = "sp300",col = "blue",
        horizontal = TRUE,notch = TRUE)
boxplot(t1$marathon,main = "Track Record for marathon",xlab = "min", ylab = "marathon",col = "green",
        horizontal = TRUE,notch = TRUE)
b1=boxplot(t1$sp100,t1$sp200,t1$sp400,main="Track Record for sec",
          names=c("100m","200m","400m"),xlab="variaties",ylab="sec",
          col=c("red","brown","yellow"))
b1
b2=boxplot(t1$sp800,t1$sp1500,t1$sp3000,t1$marathon,main="Track Record for min",
        names=c("800m","1500m","3000m","marathon"),xlab="variaties",ylab="min",
        col=c("pink","grey","blue","green"))
b2
# compute the mean and covariance of the sample
mu = apply(t1, 2, mean)
print(mu)
sigma = var(t1)
print(sigma)
sd=sqrt(diag(sigma))
print(sd)
# compute D^2/df
d2_f = mahalanobis(t1, mu, sigma)/ncol(t1)
# with the threshold value of 2.5, find out the outliers
d2_f[d2_f > 2.5]
plot(d2_f,xlab="country",ylab="time",main="Track Record",col="orange")
# compute the Principle Component
projectdataPCA=prcomp(t1,center = TRUE,scale. = TRUE)
print(projectdataPCA)
pc1=projectdataPCA$x[,1]
###print(pc1)
pc2=projectdataPCA$x[,2]
###print(pc1)
pc3=projectdataPCA$x[,3]
###print(pc3)
pc4=projectdataPCA$x[,4]
###print(pc4)
pc5=projectdataPCA$x[,5]
###print(pc5)
pc6=projectdataPCA$x[,6]
###print(pc6)
pc7=projectdataPCA$x[,7]
###print(pc7)
biplot(projectdataPCA,main="bi-plot")
# compute the P.C. using correlation matrix
r = cor(t1)
print(r)
library(corrplot)
corrplot(r,main="corrplot")
pc = princomp(covmat = r)
summary(pc, loadings = T)
###trace1=sum(diag(r))
###r1<-eigen(r)
# Variance explained by Principal Component
###lam<-r1$values
###pov1=lam/trace1
# We will do the Principal Component Analysisby "prcomp" method
###t1PCA <- prcomp(t1,center = TRUE,scale. = TRUE)
###print(t1PCA)
###t1FactLoad<-t1PCA$rotation*t(matrix(rep(t1PCA$sdev,p),p,p))  
###print(t1FactLoad)  
###t1Var<-1-t(apply(t1FactLoad^2,1,cumsum))  
###print(t1Var) 
###CumProp=(cumsum(t1PCA$sdev^2))/sum(t1PCA$sdev^2)
###print(CumProp)
# draw a scree plot, showing that only 2 P.C. could explain more than 90%
# variance as Cumulative Proportions indicated
screeplot(pc, type = "lines", main = "Track Records")
# take the number of factors as 2 and compute the factor scores of countries
f = factanal(t1, factors = 2, scores = "regression")
f$scores
f
# the score of factor 1
(sc1 = as.matrix(f$scores[, 1], ncol = 1))
# the score of factor 2
(sc2 = as.matrix(f$scores[, 2], ncol = 1))
plot(sc1,sc2,main="Factors")
f1<-projectdataPCA$sdev[1]*projectdataPCA$rotation[,1]
f2<-projectdataPCA$sdev[2]*projectdataPCA$rotation[,2]
plot(f1,f2,xlab="Factor 1", ylab="Factor 2",main="Factors with varieties",
     xlim=c(min(f1)-.1*abs(min(f1)), max(f1)+.1*abs(max(f1))), ylim=c(min(f2)-.1*abs(min(f2)), max(f2)+.1*abs(max(f2))))
names2 = c("sp100","sp200","sp400","sp800","sp1500","sp3000","marathon")
text(f1,f2,labels=names2,adj = c(0.3,-.8))
# the outliers in terms of factor 1
mu1 = apply(sc1, 2, mean)
sigma1 = var(sc1)
d2_f1 = mahalanobis(sc1, mu1, sigma1)
d2_f1[d2_f1 > 2.5]
# the outliers in terms of factor 2
mu2 = mean(sc2)
sigma2 = var(sc2)
d2_f2 = mahalanobis(sc2, mu2, sigma2)
d2_f2[d2_f2 > 2.5]


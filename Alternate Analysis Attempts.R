#Classification Tree with rpart ####
library(rpart)

# grow tree 
fit <- rpart(travel.pattern ~ aadt,
             method="class", data=fil.data)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Travel Pattern")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "tree.ps", 
     title = "Classification Tree for Kyphosis")

# prune the tree 
pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Travel Pattern")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "ptree.ps", 
     title = "Pruned Classification Tree for Travel Pattern")
#Regression Tree ####
#Regression Tree Example
library(rpart)

# grow tree 
fit <- rpart(aadt~travel.pattern, 
             method="anova", data=fil.data)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Travel Pattern")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "tree2.ps", 
     title = "Regression Tree for Travel Pattern ")

#Factorial Logistic Regression for All####

summary(glm(travel.pattern ~ aadt, data = fil.data, family = binomial))

#Discriminant Function Analysis ####

#Linear Discriminant Analysis with Jacknifed Prediction 

library(MASS)
fit <- lda(travel.pattern ~ aadt, data=fil.data, 
           na.action="na.omit", CV=TRUE)
fit # show results

plot(fit)

# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(fil.data$travel.pattern, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

#Visualizing the Results

# Scatter plot using the 1st two discriminant dimensions 
plot(fit) # fit from lda

# Panels of histograms and overlayed density plots
# for 1st discriminant function
plot(fit, dimen=1, type="both") # fit from lda

# Exploratory Graph for LDA or QDA
library(klaR)
partimat(travel.pattern ~ aadt + sawdt, data=fil.data,method="lda")

# Scatterplot for 3 Group Problem 
pairs(mydata[c("x1","x2","x3")], main="My Title ", pch=22, 
      bg=c("red", "yellow", "blue")[unclass(mydata$G)])

#Principal Components ####

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
library(dplyr)
pca.data <- select(fil.data, aadt, sadt, sawdt, wadt)
fit <- princomp(pca.data, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

# Varimax Rotated Principal Components
# retaining 5 components 
library(psych)
fit <- principal(pca.data, nfactors=5, rotate="varimax")
fit # print results

#Exploratory Factor Analysis ####

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
fit <- factanal(fil.data, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(mydata),cex=.7) # add variable names


# Import ####

fil.data <- readRDS("fil.data.rds")
central <- readRDS("central.rds")
eastern <- readRDS("eastern.rds")
noreast <- readRDS("noreast.rds")
norwest <- readRDS("norwest.rds")
west <- readRDS("west.rds")

#select for the four highway types in the dataset
library(dplyr)
king <- filter(fil.data, hwy.type == "King")
secon <- filter(fil.data, hwy.type == "Sec")
fwy <- filter(fil.data, hwy.type == "Fwy")
tert <- filter(fil.data, hwy.type == "Tert")

#grouping data by regions: Central, Eastern, Northeastern, Northwestern, West
central <- filter(fil.data, reg == "CR")
eastern <- filter(fil.data, reg == "ER")
noreast <- filter(fil.data, reg == "NE")
norwest <- filter(fil.data, reg == "NW")
west <- filter(fil.data, reg == "SW")

#Linear Models for All Regions ####
  
#par(mfrow=c(2,2))

##Fit for All Regions

###Strength of Association  


lmfit0 <- lm (aadt ~ travel.pattern, data=fil.data)
summary(lmfit0)
plot(lmfit0)
anova(lmfit0)

rsq0 <- summary(lmfit0)$r.squared
rsq0
sqrt(rsq0)

print(lmfit$fitted)

cor(fil.data$aadt, lmfit0$fitted)

plot(x = lmfit0$fitted, y = fil.data$aadt,
     xlab = "Fitted AADT", ylab = "Observed AADT")
abline(lm(fil.data$aadt ~ lmfit0$fitted,), col="red")

###One-way ANOVA
library(heplots) # for eta
model.aov <- aov(aadt ~ travel.pattern, data = fil.data)
summary(model.aov)


#for possible later extraction:
#fitted(lmmodel1)
#coef(lmmodel1)
#residuals(lmmodel1)
#names(lmmodel1)

#Model Building with AADT and Traffic Pattern

library(MASS) 
library(leaps) 
library(glmnet)   

model_ulm <- lm (aadt ~ travel.pattern, data=fil.data)
summary(model_ulm)  
  
#Dividing Data into testing and training sets

rn_train <- sample(nrow(fil.data), 
                   floor(nrow(fil.data)*0.7))
train <- fil.data[rn_train,]
test <- fil.data[-rn_train,]

prediction <- predict(model_ulm, interval="prediction", 
                      newdata =test)
Errors <- prediction[,"fit"] - test$aadt
hist(Errors)
rmse <- sqrt(sum((prediction[,"fit"] - test$aadt)^2)/nrow(test))
rel_change <- 1 - ((test$aadt- abs(Errors)) / test$aadt)
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

#Stepwise Regression

#Forward stepwise regression:

numeric.travel <- fil.data
numeric.travel$travel.pattern <- as.numeric(numeric.travel$travel.pattern)

library(MASS) 
library(leaps) 
full <- lm(aadt~travel.pattern, data=fil.data) 
null <- lm(aadt~1,data=fil.data)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), 
                 direction= "forward", trace=TRUE)
summary(stepF)

#Backward stepwise regression:

full <- lm(aadt~travel.pattern, data=fil.data) 
stepB <- stepAIC(full, direction= "backward", trace=TRUE)
summary(stepB)


#10 Fold Cross Validation
library(caret)

folds <- createFolds(fil.data$aadt, k=10)

for (f in folds){
  train <- fil.data[-f,] 
  test <- fil.data[f,]
}

model_mlr <- lm(aadt ~ travel.pattern, data=train) 

prediction <- predict(model_mlr, interval="prediction", 
                      newdata =test)
Errors <- prediction[,"fit"] - test$aadt
hist(Errors)
rmse <- sqrt(sum((prediction[,"fit"] - test$aadt)^2)/nrow(test))
rel_change <- 1 - ((test$aadt- abs(Errors)) / test$aadt)
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

#Best Combination of Travel Patterns for All Regions

library(leaps)
subsets<-regsubsets(aadt~travel.pattern,data=fil.data,
                    nbest=1,)
sub.sum <- summary(subsets)
as.data.frame(sub.sum$outmat)

#Linear Models for Central Region ####

#par(mfrow=c(2,2))

##Fit for Central

###Strength of Association  


lmfit1 <- lm (aadt ~ travel.pattern, data=central)
summary(lmfit1)
plot(lmfit1)
anova(lmfit1)

rsq0 <- summary(lmfit1)$r.squared
rsq0
sqrt(rsq0)

print(lmfit1$fitted)

cor(central$aadt, lmfit1$fitted)

plot(x = lmfit1$fitted, y = central$aadt,
     xlab = "Fitted AADT", ylab = "Observed AADT")
abline(lm(central$aadt ~ lmfit1$fitted,), col="red")

###One-way ANOVA
library(heplots) # for eta
model.aov <- aov(aadt ~ travel.pattern, data = central)
summary(model.aov)


#for possible later extraction:
#fitted(lmmodel1)
#coef(lmmodel1)
#residuals(lmmodel1)
#names(lmmodel1)

#Model Building with AADT and Traffic Pattern

library(MASS) 
library(leaps) 
library(glmnet)   

model_ulm <- lm (aadt ~ travel.pattern, data=central)
summary(model_ulm)  

#Dividing Data into testing and training sets

rn_train <- sample(nrow(central), 
                   floor(nrow(central)*0.7))
train <- central[rn_train,]
test <- central[-rn_train,]

prediction <- predict(model_ulm, interval="prediction", 
                      newdata =test)
Errors <- prediction[,"fit"] - test$aadt
hist(Errors)
rmse <- sqrt(sum((prediction[,"fit"] - test$aadt)^2)/nrow(test))
rel_change <- 1 - ((test$aadt- abs(Errors)) / test$aadt)
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

#Stepwise Regression

#Forward stepwise regression:

numeric.travel <- central
numeric.travel$travel.pattern <- as.numeric(numeric.travel$travel.pattern)

library(MASS) 
library(leaps) 
full <- lm(aadt~travel.pattern, data=central) 
null <- lm(aadt~1,data=central)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), 
                 direction= "forward", trace=TRUE)
summary(stepF)

#Backward stepwise regression:

full <- lm(aadt~travel.pattern, data=central) 
stepB <- stepAIC(full, direction= "backward", trace=TRUE)
summary(stepB)


#10 Fold Cross Validation
library(caret)

folds <- createFolds(central$aadt, k=10)

for (f in folds){
  train <- central[-f,] 
  test <- central[f,]
}

model_mlr <- lm(aadt ~ travel.pattern, data=train) 

prediction <- predict(model_mlr, interval="prediction", 
                      newdata =test)
Errors <- prediction[,"fit"] - test$aadt
hist(Errors)
rmse <- sqrt(sum((prediction[,"fit"] - test$aadt)^2)/nrow(test))
rel_change <- 1 - ((test$aadt- abs(Errors)) / test$aadt)
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

#Best Combination of Travel Patterns for Central Region

library(leaps)
subsets<-regsubsets(aadt~travel.pattern,data=central,
                    nbest=1,)
sub.sum <- summary(subsets)
as.data.frame(sub.sum$outmat)

#Linear Models for Eastern Region ####

#par(mfrow=c(2,2))

##Fit for eastern

###Strength of Association  


lmfit2 <- lm (aadt ~ travel.pattern, data=eastern)
summary(lmfit2)
plot(lmfit2)
anova(lmfit2)

rsq0 <- summary(lmfit2)$r.squared
rsq0
sqrt(rsq0)

print(lmfit$fitted)

cor(eastern$aadt, lmfit2$fitted)

plot(x = lmfit2$fitted, y = eastern$aadt,
     xlab = "Fitted AADT", ylab = "Observed AADT")
abline(lm(eastern$aadt ~ lmfit2$fitted,), col="red")

###One-way ANOVA
library(heplots) # for eta
model.aov <- aov(aadt ~ travel.pattern, data = eastern)
summary(model.aov)


#for possible later extraction:
#fitted(lmmodel1)
#coef(lmmodel1)
#residuals(lmmodel1)
#names(lmmodel1)

#Model Building with AADT and Traffic Pattern

library(MASS) 
library(leaps) 
library(glmnet)   

model_ulm <- lm (aadt ~ travel.pattern, data=eastern)
summary(model_ulm)  

#Dividing Data into testing and training sets

rn_train <- sample(nrow(eastern), 
                   floor(nrow(eastern)*0.7))
train <- eastern[rn_train,]
test <- eastern[-rn_train,]

prediction <- predict(model_ulm, interval="prediction", 
                      newdata =test)
Errors <- prediction[,"fit"] - test$aadt
hist(Errors)
rmse <- sqrt(sum((prediction[,"fit"] - test$aadt)^2)/nrow(test))
rel_change <- 1 - ((test$aadt- abs(Errors)) / test$aadt)
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

#Stepwise Regression

#Forward stepwise regression:

numeric.travel <- eastern
numeric.travel$travel.pattern <- as.numeric(numeric.travel$travel.pattern)

library(MASS) 
library(leaps) 
full <- lm(aadt~travel.pattern, data=eastern) 
null <- lm(aadt~1,data=eastern)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), 
                 direction= "forward", trace=TRUE)
summary(stepF)

#Backward stepwise regression:

full <- lm(aadt~travel.pattern, data=eastern) 
stepB <- stepAIC(full, direction= "backward", trace=TRUE)
summary(stepB)


#10 Fold Cross Validation
library(caret)

folds <- createFolds(eastern$aadt, k=10)

for (f in folds){
  train <- eastern[-f,] 
  test <- eastern[f,]
}

model_mlr <- lm(aadt ~ travel.pattern, data=train) 

prediction <- predict(model_mlr, interval="prediction", 
                      newdata =test)
Errors <- prediction[,"fit"] - test$aadt
hist(Errors)
rmse <- sqrt(sum((prediction[,"fit"] - test$aadt)^2)/nrow(test))
rel_change <- 1 - ((test$aadt- abs(Errors)) / test$aadt)
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

#Best Combination of Travel Patterns for eastern Region

library(leaps)
subsets<-regsubsets(aadt~travel.pattern,data=eastern,
                    nbest=1,)
sub.sum <- summary(subsets)
as.data.frame(sub.sum$outmat)

#Linear Models for Northeast Region ####

#par(mfrow=c(2,2))

##Fit for noreast

###Strength of Association  

lmfit3 <- lm (aadt ~ travel.pattern, data=noreast)
summary(lmfit3)
plot(lmfit3)
anova(lmfit3)

rsq0 <- summary(lmfit3)$r.squared
rsq0
sqrt(rsq0)

print(lmfit$fitted)

cor(noreast$aadt, lmfit3$fitted)

plot(x = lmfit3$fitted, y = noreast$aadt,
     xlab = "Fitted AADT", ylab = "Observed AADT")
abline(lm(noreast$aadt ~ lmfit3$fitted,), col="red")

###One-way ANOVA
library(heplots) # for eta
model.aov <- aov(aadt ~ travel.pattern, data = noreast)
summary(model.aov)


#for possible later extraction:
#fitted(lmmodel1)
#coef(lmmodel1)
#residuals(lmmodel1)
#names(lmmodel1)

#Model Building with AADT and Traffic Pattern

library(MASS) 
library(leaps) 
library(glmnet)   

model_ulm <- lm (aadt ~ travel.pattern, data=noreast)
summary(model_ulm)  

#Dividing Data into testing and training sets

rn_train <- sample(nrow(noreast), 
                   floor(nrow(noreast)*0.7))
train <- noreast[rn_train,]
test <- noreast[-rn_train,]

prediction <- predict(model_ulm, interval="prediction", 
                      newdata =test)
Errors <- prediction[,"fit"] - test$aadt
hist(Errors)
rmse <- sqrt(sum((prediction[,"fit"] - test$aadt)^2)/nrow(test))
rel_change <- 1 - ((test$aadt- abs(Errors)) / test$aadt)
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

#Stepwise Regression

#Forward stepwise regression:

numeric.travel <- noreast
numeric.travel$travel.pattern <- as.numeric(numeric.travel$travel.pattern)

library(MASS) 
library(leaps) 
full <- lm(aadt~travel.pattern, data=noreast) 
null <- lm(aadt~1,data=noreast)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), 
                 direction= "forward", trace=TRUE)
summary(stepF)

#Backward stepwise regression:

full <- lm(aadt~travel.pattern, data=noreast) 
stepB <- stepAIC(full, direction= "backward", trace=TRUE)
summary(stepB)


#10 Fold Cross Validation
library(caret)

folds <- createFolds(noreast$aadt, k=10)

for (f in folds){
  train <- noreast[-f,] 
  test <- noreast[f,]
}

model_mlr <- lm(aadt ~ travel.pattern, data=train) 

prediction <- predict(model_mlr, interval="prediction", 
                      newdata =test)
Errors <- prediction[,"fit"] - test$aadt
hist(Errors)
rmse <- sqrt(sum((prediction[,"fit"] - test$aadt)^2)/nrow(test))
rel_change <- 1 - ((test$aadt- abs(Errors)) / test$aadt)
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

#Best Combination of Travel Patterns for noreast Region

library(leaps)
subsets<-regsubsets(aadt~travel.pattern,data=noreast,
                    nbest=1,)
sub.sum <- summary(subsets)
as.data.frame(sub.sum$outmat)

#Linear Models for Northwest Region ####

#par(mfrow=c(2,2))

##Fit for norwest

###Strength of Association  


lmfit4 <- lm (aadt ~ travel.pattern, data=norwest)
summary(lmfit4)
plot(lmfit4)
anova(lmfit4)

rsq0 <- summary(lmfit4)$r.squared
rsq0
sqrt(rsq0)

print(lmfit$fitted)

cor(norwest$aadt, lmfit4$fitted)

plot(x = lmfit4$fitted, y = norwest$aadt,
     xlab = "Fitted AADT", ylab = "Observed AADT")
abline(lm(norwest$aadt ~ lmfit4$fitted,), col="red")

###One-way ANOVA
library(heplots) # for eta
model.aov <- aov(aadt ~ travel.pattern, data = norwest)
summary(model.aov)


#for possible later extraction:
#fitted(lmmodel1)
#coef(lmmodel1)
#residuals(lmmodel1)
#names(lmmodel1)

#Model Building with AADT and Traffic Pattern

library(MASS) 
library(leaps) 
library(glmnet)   

model_ulm <- lm (aadt ~ travel.pattern, data=norwest)
summary(model_ulm)  

#Dividing Data into testing and training sets

rn_train <- sample(nrow(norwest), 
                   floor(nrow(norwest)*0.7))
train <- norwest[rn_train,]
test <- norwest[-rn_train,]

prediction <- predict(model_ulm, interval="prediction", 
                      newdata =test)
Errors <- prediction[,"fit"] - test$aadt
hist(Errors)
rmse <- sqrt(sum((prediction[,"fit"] - test$aadt)^2)/nrow(test))
rel_change <- 1 - ((test$aadt- abs(Errors)) / test$aadt)
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

#Stepwise Regression

#Forward stepwise regression:

numeric.travel <- norwest
numeric.travel$travel.pattern <- as.numeric(numeric.travel$travel.pattern)

library(MASS) 
library(leaps) 
full <- lm(aadt~travel.pattern, data=norwest) 
null <- lm(aadt~1,data=norwest)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), 
                 direction= "forward", trace=TRUE)
summary(stepF)

#Backward stepwise regression:

full <- lm(aadt~travel.pattern, data=norwest) 
stepB <- stepAIC(full, direction= "backward", trace=TRUE)
summary(stepB)


#10 Fold Cross Validation
library(caret)

folds <- createFolds(norwest$aadt, k=10)

for (f in folds){
  train <- norwest[-f,] 
  test <- norwest[f,]
}

model_mlr <- lm(aadt ~ travel.pattern, data=train) 

prediction <- predict(model_mlr, interval="prediction", 
                      newdata =test)
Errors <- prediction[,"fit"] - test$aadt
hist(Errors)
rmse <- sqrt(sum((prediction[,"fit"] - test$aadt)^2)/nrow(test))
rel_change <- 1 - ((test$aadt- abs(Errors)) / test$aadt)
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

#Best Combination of Travel Patterns for norwest Region

library(leaps)
subsets<-regsubsets(aadt~travel.pattern,data=norwest,
                    nbest=1,)
sub.sum <- summary(subsets)
as.data.frame(sub.sum$outmat)

#Linear Models for West Region ####

#par(mfrow=c(2,2))

##Fit for west

###Strength of Association  


lmfit3 <- lm (aadt ~ travel.pattern, data=west)
summary(lmfit3)
plot(lmfit3)
anova(lmfit3)

rsq0 <- summary(lmfit3)$r.squared
rsq0
sqrt(rsq0)

print(lmfit$fitted)

cor(west$aadt, lmfit3$fitted)

plot(x = lmfit3$fitted, y = west$aadt,
     xlab = "Fitted AADT", ylab = "Observed AADT")
abline(lm(west$aadt ~ lmfit3$fitted,), col="red")

###One-way ANOVA
library(heplots) # for eta
model.aov <- aov(aadt ~ travel.pattern, data = west)
summary(model.aov)


#for possible later extraction:
#fitted(lmmodel1)
#coef(lmmodel1)
#residuals(lmmodel1)
#names(lmmodel1)

#Model Building with AADT and Traffic Pattern

library(MASS) 
library(leaps) 
library(glmnet)   

model_ulm <- lm (aadt ~ travel.pattern, data=west)
summary(model_ulm)  

#Dividing Data into testing and training sets

rn_train <- sample(nrow(west), 
                   floor(nrow(west)*0.7))
train <- west[rn_train,]
test <- west[-rn_train,]

prediction <- predict(model_ulm, interval="prediction", 
                      newdata =test)
Errors <- prediction[,"fit"] - test$aadt
hist(Errors)
rmse <- sqrt(sum((prediction[,"fit"] - test$aadt)^2)/nrow(test))
rel_change <- 1 - ((test$aadt- abs(Errors)) / test$aadt)
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

#Stepwise Regression

#Forward stepwise regression:

numeric.travel <- west
numeric.travel$travel.pattern <- as.numeric(numeric.travel$travel.pattern)

library(MASS) 
library(leaps) 
full <- lm(aadt~travel.pattern, data=west) 
null <- lm(aadt~1,data=west)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), 
                 direction= "forward", trace=TRUE)
summary(stepF)

#Backward stepwise regression:

full <- lm(aadt~travel.pattern, data=west) 
stepB <- stepAIC(full, direction= "backward", trace=TRUE)
summary(stepB)


#10 Fold Cross Validation
library(caret)

folds <- createFolds(west$aadt, k=10)

for (f in folds){
  train <- west[-f,] 
  test <- west[f,]
}

model_mlr <- lm(aadt ~ travel.pattern, data=train) 

prediction <- predict(model_mlr, interval="prediction", 
                      newdata =test)
Errors <- prediction[,"fit"] - test$aadt
hist(Errors)
rmse <- sqrt(sum((prediction[,"fit"] - test$aadt)^2)/nrow(test))
rel_change <- 1 - ((test$aadt- abs(Errors)) / test$aadt)
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

#Best Combination of Travel Patterns for west Region

library(leaps)
subsets<-regsubsets(aadt~travel.pattern,data=west,
                    nbest=1,)
sub.sum <- summary(subsets)
as.data.frame(sub.sum$outmat)

#Decision Tree Classification for All ####
  
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
fit <- rpart(travel.pattern~aadt, 
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

#ANOVA ####

anova(lm(aadt ~ travel.pattern, data = fil.data))

c.tree<- ctree(travel.pattern ~ aadt, data = fil.data)
print(c.tree)
plot(c.tree, type="simple")
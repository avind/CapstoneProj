#west Region

#Linear Models for west Region

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

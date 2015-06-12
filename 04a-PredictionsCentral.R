#Central Region

#Linear Models for Central Region

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

print(lmfit$fitted)

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

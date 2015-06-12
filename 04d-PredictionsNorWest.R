#Northwest Region

#Linear Models for Northwest Region

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

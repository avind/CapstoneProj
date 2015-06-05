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

##4: Predictions

------------------
------------------
  
  
#Linear Models for each Region 
  
par(mfrow=c(2,2))

lmfit0 <- lm (aadt ~ travel.pattern, data=fil.data)
summary(lmfit0)
plot(lmfit0)
anova(lmfit0)

lmfit1 <- lm (aadt ~ travel.pattern, data=fwy)
summary(lmfit1)
plot(lmfit1)
anova(lmfit1)

#for possible later extraction:
#fitted(lmmodel1)
#coef(lmmodel1)
#residuals(lmmodel1)
#names(lmmodel1)

lmfit2 <- lm (aadt ~ travel.pattern, data=fwy)
summary(lmfit2)
plot(lmfit2)

lmfit3<- lm (aadt ~ travel.pattern, data=fwy)
summary(lmfit3)
plot(lmfit3)

lmfit4<- lm (aadt ~ travel.pattern, data=fwy)
summary(lmfit4)
plot(lmfit4)

lmfit5<- lm (aadt ~ travel.pattern, data=fwy)
summary(lmfit5)
plot(lmfit5)

------------------
------------------

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
full <- lm(aadt~travel.pattern, data=fwy) 
null <- lm(aadt~1,data=fwy)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), 
                 direction= "forward", trace=TRUE)

summary(stepF)


#Backward stepwise regression:

full <- lm(aadt~travel.pattern, data=fwy) 
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

------------------
------------------

#Decision Tree Classification
  
  
------------------
------------------




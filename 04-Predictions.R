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



library(dplyr)
library(corrplot)

cordata <- fil.data %>%
  group_by(reg) %>%
  select(year, aadt)

dhvcor <- select(fil.data, dhv.percent, aadt, sadt, sawdt, wadt)
cor(cordata)
cor(dhvcor)
corrplot(cor(cordata))
corrplot(cor(dhvcor))
plot(cordata)


fil.data2 <- fil.data

fil.data2 <- within(fil.data2, {
  travel.patternC <- C(travel.pattern, aadt)
  print(attributes(travel.patternC))
})

fwy %>% 
  ggvis(~travel.pattern, ~aadt) %>%
  layer_histograms() %>%
  layer_model_predictions()

ggplot(model_ulm)


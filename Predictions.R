fil.data <- readRDS("fil.data.rds")
fil.data <- readRDS("central.rds")
fil.data <- readRDS("eastern.rds")
fil.data <- readRDS("noreast.rds")
fil.data <- readRDS("norwest.rds")
fil.data <- readRDS("west.rds")

##4: Predictions

------------------
------------------
  
  
#Linear Models for each Region 
  
par(mfrow=c(2,2))

lmfit1 <- lm (aadt ~ travel.pattern, data=fwy)
summary(lmfit1)
plot(lmfit1)
anova(lmfit1)

covfit(lmfit1)

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


model_ulm <- lm (aadt ~ travel.pattern, data=fil.data)
summary(model_ulm)
plot(model_ulm)

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


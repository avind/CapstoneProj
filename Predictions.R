fil.data <- readRDS("fil.data.rds")
central <- readRDS("central.rds")
eastern <- readRDS("eastern.rds")
noreast <- readRDS("noreast.rds")
norwest <- readRDS("norwest.rds")
west <- readRDS("west.rds")

#select for the four highway types in the dataset
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


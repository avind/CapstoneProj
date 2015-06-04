fil.data <- readRDS("fil.data.rds")
fil.data <- readRDS("central.rds")
fil.data <- readRDS("eastern.rds")
fil.data <- readRDS("noreast.rds")
fil.data <- readRDS("norwest.rds")
fil.data <- readRDS("west.rds")

##4: Predictions


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

#Linear Models for each Region 

par(mfrow=c(2,2))

lmmodel1 <- lm (aadt ~ travel.pattern, data=fwy)
summary(lmmodel1)
plot(lmmodel1)
anova(lmmodel1)

covfit(lmmodel1)

#for possible later extraction:
  #fitted(lmmodel1)
  #coef(lmmodel1)
  #residuals(lmmodel1)
  #names(lmmodel1)

lmmodel2 <- lm (aadt ~ travel.pattern, data=fwy)
summary(lmmodel2)
plot(lmmodel2)

lmmodel3<- lm (aadt ~ travel.pattern, data=fwy)
summary(lmmodel3)
plot(lmmodel3)

lmmodel4<- lm (aadt ~ travel.pattern, data=fwy)
summary(lmmodel4)
plot(lmmodel4)

lmmodel5<- lm (aadt ~ travel.pattern, data=fwy)
summary(lmmodel5)
plot(lmmodel5)

fwy %>% 
  ggvis(~travel.pattern, ~aadt) %>%
  layer_histograms() %>%
  layer_model_predictions()

ggplot(model_ulm)


#<!-- rank data 

#{r, include=FALSE}
# add this chunk to end of mycode.rmd
#file.rename(from="scripts/mycode.md", 
            to="README.md")


##GIS Mapping with ORN


#> orndbf <- read.dbf("C:/Users/Alex/ORN_SEGMENT_WITH_ADDRESS.dbf")
#> str(orndbf)



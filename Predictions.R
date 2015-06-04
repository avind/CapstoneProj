##4: Predictions


library(dplyr)
library(corrplot)

cordata <- select(fil.data, aadt, sadt, sawdt, wadt)
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



model_ulm <- lm (aadt ~ travel.pattern, data=fwy)
summary(model_ulm)
plot(model_ulm)

fwy %>% 
  ggvis(~travel.pattern, ~aadt) %>%
  layer_histograms() %>%
  layer_model_predictions()

ggplot(model_ulm)
```


#<!-- rank data 

#{r, include=FALSE}
# add this chunk to end of mycode.rmd
#file.rename(from="scripts/mycode.md", 
            to="README.md")


##GIS Mapping with ORN


#> orndbf <- read.dbf("C:/Users/Alex/ORN_SEGMENT_WITH_ADDRESS.dbf")
#> str(orndbf)



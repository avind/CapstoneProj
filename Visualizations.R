fil.data <- readRDS("fil.data.rds")
fil.data <- readRDS("central.rds")
fil.data <- readRDS("eastern.rds")
fil.data <- readRDS("noreast.rds")
fil.data <- readRDS("norwest.rds")
fil.data <- readRDS("west.rds")


##3: Visualizations

library(ggvis)
library(ggplot2)


fwy %>% 
  ggvis(~travel.pattern, ~aadt) %>%
  layer_smooths() %>%
  layer_model_predictions(model = "lm", stroke := "red", se = TRUE)

king %>% 
  ggvis(~year, ~aadt) %>%
  layer_smooths() %>%
  layer_model_predictions(model = "lm", stroke := "red", se = TRUE)

secon %>% 
  ggvis(~year, ~aadt) %>%
  layer_smooths() %>%
  layer_model_predictions(model = "lm", stroke := "red", se = TRUE)

tert %>% 
  ggvis(~year, ~aadt) %>%
  layer_smooths() %>%
  layer_model_predictions(model = "lm", stroke := "red", se = TRUE)



#Distribution of Travel by Pattern
fwy %>% 
  ggvis(~travel.pattern, ~aadt) %>%
  layer_histograms()

fwyhist <- select(fwy, travel.pattern, aadt)
hist(fwyhist$aadt)




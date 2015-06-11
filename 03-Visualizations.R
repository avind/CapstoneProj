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

#Mapping

library(ggmap)
library(mapproj)
map <- get_map(location = 'Ontario Canada', zoom = 5)
ggmap(map)

---

#Interactive Map and Choropleth  
  
library(ggvis)
library(rgdal)
library(rgeos)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(data.table)
library(maptools)  
  
regions <- readOGR("MTO_Regions", 
                   layer="MTO_Regions")

map <- ggplot2::fortify(regions, region="NAME")

map %>%
  group_by(group, id) %>%
  ggvis(~long, ~lat) %>%
  layer_paths(strokeOpacity := .30) %>%
  hide_legend("fill") %>%
  hide_axis("x") %>% hide_axis("y") %>%
  set_options(width=700, height=600, keep_aspect=TRUE)





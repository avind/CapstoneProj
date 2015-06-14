# Import ####
library(dplyr)

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

#3: Basic Visualizations ####

library(ggvis)
library(ggplot2)

qplot(year, aadt, data=fwy, position="jitter") + 
  geom_smooth()

ggplot(fwy, aes(x=year, y=aadt)) + geom_smooth() + geom_abline(data=fwy, aes(intercept=a, slope=b))

add_title <- function(vis, ..., x_lab = "X units", title = "Plot Title") 
{
  add_axis(vis, "x", title = x_lab) %>% 
    add_axis("x", orient = "top", ticks = 0, title = title,
             properties = axis_props(
               axis = list(stroke = "white"),
               title = list(fontSize = 26),
               labels = list(fontSize = 0)
             ), ...)
}

fwy %>% 
  ggvis(~year, ~aadt) %>%
  layer_smooths() %>%
  layer_model_predictions(model = "lm", stroke := "red", se = TRUE) %>%
  add_title(title = "Freeway", x_lab="Year")

king %>% 
  ggvis(~year, ~aadt) %>%
  layer_smooths() %>%
  layer_model_predictions(model = "lm", stroke := "red", se = TRUE) %>%
  add_title(title = "King's Highway", x_lab="Year")

secon %>% 
  ggvis(~year, ~aadt) %>%
  layer_smooths() %>%
  layer_model_predictions(model = "lm", stroke := "red", se = TRUE) %>%
  add_title(title = "Secondary Highway", x_lab="Year")

tert %>% 
  ggvis(~year, ~aadt) %>%
  layer_smooths() %>%
  layer_model_predictions(model = "lm", stroke := "red", se = TRUE) %>%
  add_title(title = "Tertiary Highway", x_lab="Year")



##Distribution of Travel by Pattern ####
c1 <- ggplot(fwy,aes(x=aadt))+
  geom_histogram(aes(y= ..density..)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_histogram(colour="black", fill="grey") + ggtitle("Freeways") + xlab("AADT") +
  ylab("Frequency") + 
  geom_vline(aes(xintercept=mean(aadt, na.rm=T)),  
             color="red", linetype="dashed", size=1)

c2 <- ggplot(king,aes(x=aadt))+
  geom_histogram(aes(y= ..density..)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_histogram(colour="black", fill="grey") + ggtitle("King's") + xlab("AADT") +
  ylab("Frequency") + 
  geom_vline(aes(xintercept=mean(aadt, na.rm=T)),  
             color="red", linetype="dashed", size=1)

c3 <- ggplot(secon,aes(x=aadt))+
  geom_histogram(aes(y= ..density..)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_histogram(colour="black", fill="grey") + ggtitle("Secondary") + xlab("AADT") +
  ylab("Frequency") + 
  geom_vline(aes(xintercept=mean(aadt, na.rm=T)),  
             color="red", linetype="dashed", size=1)

c4 <- ggplot(tert,aes(x=aadt))+
  geom_histogram(aes(y= ..density..)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_histogram(colour="black", fill="grey") + ggtitle("Tertiary") + xlab("AADT") +
  ylab("Frequency") + 
  geom_vline(aes(xintercept=mean(aadt, na.rm=T)),  
             color="red", linetype="dashed", size=1)

multiplot(c1, c2, c3, c4, cols=2)

##ggmap Mapping ####

library(ggmap)
library(mapproj)
map <- get_map(location = 'Ontario Canada', zoom = 5)
ggmap(map)

---

##Mosaic Plots ####

library(dplyr)  
library(vcd)
mytable <- table(fil.data$reg,fil.data$travel.pattern) 


mosaic(mytable,  shade=TRUE,legend=TRUE, split_vertical=NULL)  

mosaicplot(mytable, main="Region vs. Travel Pattern",
           xlab="Travel Pattern",ylab="Region",
           col=c(3:16), legend=TRUE)

#Talk about differences between sqkm landmass and population of north and south ontario
---

##Correspondence Analysis for All Regions ####
  
library(ca)
mytable <- table(fil.data$reg,fil.data$travel.pattern) # A will be rows, B will be columns 
mytable <- with(mydata, table(A,B)) # create a 2 way table
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages
fit <- ca(mytable)
print(fit) # basic results 
summary(fit) # extended results 
plot(fit) # symmetric map
plot(fit, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map  
  

##Interactive Map and Choropleth ####
  
library(ggvis)
library(rgdal)
library(rgeos)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(data.table)
library(maptools)  
library(reshape)
  
#import shapefile
regions <- readOGR("MTO_Regions", 
                   layer="MTO_Regions")

#fortify shapefile
map <- ggplot2::fortify(regions, region="NAME")

#plot basic polygons using ggvis
map %>%
  group_by(group, id) %>%
  ggvis(~long, ~lat) %>%
  layer_paths(strokeOpacity := .30) %>%
  hide_legend("fill") %>%
  hide_axis("x") %>% hide_axis("y") %>%
  set_options(width=700, height=600, keep_aspect=TRUE)

#extract labels
region_centers <- regions %>%
  gCentroid(byid=TRUE) %>%
  data.frame %>%
  cbind(name=regions$NAME)

#plot labels on the map
map %>%
  group_by(group, id) %>%
  ggvis(~long, ~lat) %>%
  layer_paths(strokeWidth:=0.5, stroke:="#7f7f7f") %>%
  layer_points(data=region_centers, x=~x, y=~y, size:=8) %>%
  layer_text(data=region_centers,
             x=~x+0.05, y=~y, text:=~name,
             baseline:="middle", fontSize:=16) %>%
  hide_legend("fill") %>%
  hide_axis("x") %>% hide_axis("y") %>%
  set_options(width=700, height=600, keep_aspect=TRUE)

#basic choropleth

#read in data and convert to appropriate form for mapping ####

library(dplyr)
map.data <- select(fil.data, year, reg, aadt, annual.aadt.change, decade.change)
map.data$reg <- as.character(map.data$reg)

map.data$reg[map.data$reg=="CR"] <- "Central"
map.data$reg[map.data$reg=="ER"] <- "East"
map.data$reg[map.data$reg=="NE"] <- "North"
map.data$reg[map.data$reg=="NW"] <- "North"
map.data$reg[map.data$reg=="SW"] <- "West"

map.data$reg <- factor(map.data$reg)

## Aggregate data by week into data by Year

yeartest <- map.data %>%
  group_by(reg, year) %>%
  summarise(avg = mean(aadt))


## Transpose dataframe to make columns by Year
yeartestt <- t(yeartest)
colnames(yeartestt) <- yeartestt[2,]
yeartestt <- yeartestt[-c(2),]

## ggvis explorations
regionIDs <- as.vector(unique(yeartest$reg))
Years <- as.vector(unique(yeartest$year))
yeartest$year <- as.character(yeartest$year)

## bar chart w/selectable Years
RegionBars <- yeartest %>% 
  ggvis(x = ~reg, y = ~avg) %>% 
  filter(year == eval(input_radiobuttons(choices = Years, label = "Years"))) %>% 
  layer_points()

RegionBars

#### not sure if need####
## merge state names with Region definitions
MumpsbyYearTRegion <- merge(MumpsbyYearT, Regions, by.x="row.names", by.y="State")
MumpsbyYearTRegion$Region = as.character(MumpsbyYearTRegion$Region)

## Aggregate state data into Regional data
MumpsbyRegion <- aggregate(MumpsbyYearTRegion[2:37],by = list(MumpsbyYearTRegion$Region), sum)
colnames(MumpsbyRegion)[1] <- "Region"

## Use "reshape" package to melt data into long form (With Year as variable rather than a column header)
melt_MumpsbyRegion <- melt(MumpsbyRegion, id="Region")
colnames(melt_MumpsbyRegion)[2] <- "Year"
colnames(melt_MumpsbyRegion)[3] <- "Cases"

map.regions <- map.data %>%

#crime stat reshaping ####  
  
#get it into a form we can use (and only use 2013 data)

crime_1k <- me_crime %>%
  filter(year==2013) %>%
  select(1,5:12) %>%
  left_join(me_pop) %>%
  mutate(murder_1k=1000*(murder/population_2010),
         rape_1k=1000*(rape/population_2010),
         robbery_1k=1000*(robbery/population_2010),
         aggravated_assault_1k=1000*(aggravated_assault/population_2010),
         burglary_1k=1000*(burglary/population_2010),
         larceny_1k=1000*(larceny/population_2010),
         motor_vehicle_theft_1k=1000*(motor_vehicle_theft/population_2010),
         arson_1k=1000*(arson/population_2010))

# normalize the county names

map %<>% mutate(id=gsub(" County, ME", "", id)) %>%
  left_join(crime_1k, by=c("id"="county"))

#### crime stat choropleth ####

# this is for the tooltip. it does a lookup into the crime data frame and
# then uses those values for the popup

aadt_values <- function(x) {
  if(is.null(x)) return(NULL)
  y <- yeartest %>% filter(year==x$year, reg==x$group) 
  sprintf("<table width='100%%'>%s</table>",
          paste0("<tr><td style='text-align:left'>", names(y),
                 ":</td><td style='text-align:right'>", format(y), collapse="</td></tr>"))
}map %>%
  group_by(group, id) %>%
  ggvis(~long, ~lat) %>%
  layer_paths(fill=input_select(label="AADT:",
                                choices= yeartest %>%
                                  select(avg) %>%
                                  colnames %>% sort,
                                id="AADT",
                                map=id),
              strokeWidth:=0.5, stroke:="white") %>%
  scale_numeric("fill", range=c("#bfd3e6", "#8c6bb1" ,"#4d004b")) %>%
  add_tooltip(aadt_values, "hover") %>%
  add_legend("fill", title="Average AADT") %>%
  hide_axis("x") %>% hide_axis("y") %>%
  set_options(width=700, height=600, keep_aspect=TRUE)


map %>%
  group_by(group, id) %>%
  ggvis(~long, ~lat) %>%
  layer_paths(fill=input_select(label="Crime:",
                                choices=crime_1k %>%
                                  select(ends_with("1k")) %>%
                                  colnames %>% sort,
                                id="Crime",
                                map=as.name),
              strokeWidth:=0.5, stroke:="white") %>%
  scale_numeric("fill", range=c("#bfd3e6", "#8c6bb1" ,"#4d004b")) %>%
  add_tooltip(crime_values, "hover") %>%
  add_legend("fill", title="Crime Rate/1K Pop") %>%
  hide_axis("x") %>% hide_axis("y") %>%
  set_options(width=400, height=600, keep_aspect=TRUE)

map
regions
yeartest

yeartestgrp <- rename(yeartest, "group"=reg)




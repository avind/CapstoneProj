## Import ####

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

#2: Exploratory ####

library(pastecs)
library(dplyr)
explor.stat <- select (fil.data, aadt:wadt)
stat.desc(explor.stat)


library(psych)
describe(explor.stat)
#cor(renamed.raw)

###Multiplot function ####
#From R Cookbook: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot1)/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Histograms ####

#describe distribution of patterns by region
#central

library(ggvis)
par(mfrow=c(3,2))
ggvis(central, ~aadt, fill:= "#fff8dc") %>%
  layer_histograms() %>%
  add_axis("x", title = "AADT") %>%
  add_axis("y", title = "Count") 

library(ggplot2)

p1 <- ggplot(central$aadt, aes(x=aadt) + geom_histogram(fill="#FF9999", colour="black"))

ggplot(central$aadt, aes(x=aadt) + geom_bar())

d1 <- ggplot(fil.data,aes(x=aadt))+
  geom_histogram(aes(y= ..density..)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_histogram(colour="black", fill="grey") + ggtitle("All Regions") + xlab("AADT") +
  ylab("Frequency") + 
  geom_vline(aes(xintercept=mean(aadt, na.rm=T)),  
             color="red", linetype="dashed", size=1)

d2 <- ggplot(central,aes(x=aadt))+
  geom_histogram(aes(y= ..density..)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_histogram(colour="black", fill="grey") + ggtitle("Central") + xlab("AADT") +
  ylab("Frequency") + 
  geom_vline(aes(xintercept=mean(aadt, na.rm=T)),  
             color="red", linetype="dashed", size=1)

d3 <- ggplot(eastern,aes(x=aadt))+
  geom_histogram(aes(y= ..density..)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_histogram(colour="black", fill="grey") + ggtitle("Eastern") + xlab("AADT") +
  ylab("Frequency") + 
  geom_vline(aes(xintercept=mean(aadt, na.rm=T)),  
             color="red", linetype="dashed", size=1)

d4 <- ggplot(noreast,aes(x=aadt))+
  geom_histogram(aes(y= ..density..)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_histogram(colour="black", fill="grey") + ggtitle("Northeast") + xlab("AADT") +
  ylab("Frequency") + 
  geom_vline(aes(xintercept=mean(aadt, na.rm=T)),  
             color="red", linetype="dashed", size=1)

d5 <- ggplot(norwest,aes(x=aadt))+
  geom_histogram(aes(y= ..density..)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_histogram(colour="black", fill="grey") + ggtitle("Northwest") + xlab("AADT") +
  ylab("Frequency") + 
  geom_vline(aes(xintercept=mean(aadt, na.rm=T)),  
             color="red", linetype="dashed", size=1)
 
d6 <- ggplot(west,aes(x=aadt))+
  geom_histogram(aes(y= ..density..)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_histogram(colour="black", fill="grey") + ggtitle("West") + xlab("AADT") +
  ylab("Frequency") + 
  geom_vline(aes(xintercept=mean(aadt, na.rm=T)),  
             color="red", linetype="dashed", size=1)

multiplot(d1, d2, d3, d4, d5, d6, cols=3)


p1 <- qplot(central$aadt,
            geom="histogram",
            main = "Histogram for Central",
            xlab = "AADT") + theme(panel.background = element_rect(fill = 'white', colour = 'black')) + geom_histogram(colour="black", fill="grey")

p2 <- qplot(eastern$aadt,
            geom="histogram",
            main = "Histogram for Eastern",
            xlab = "AADT") + theme(panel.background = element_rect(fill = 'white', colour = 'black')) + geom_histogram(colour="black", fill="grey") 

p3 <- qplot(noreast$aadt,
            geom="histogram",
            main = "Histogram for Northeastern",
            xlab = "AADT") + theme(panel.background = element_rect(fill = 'white', colour = 'black')) + geom_histogram(colour="black", fill="grey")

p4 <- qplot(norwest$aadt,
            geom="histogram",
            main = "Histogram for Northwestern",
            xlab = "AADT") + theme(panel.background = element_rect(fill = 'white', colour = 'black')) + geom_histogram(colour="black", fill="grey") 

p5 <- qplot(west$aadt,
            geom="histogram",
            main = "Histogram for West",
            xlab = "AADT") + theme(panel.background = element_rect(fill = 'white', colour = 'black')) + geom_histogram(colour="black", fill="grey") 

p6 <- qplot(fil.data$aadt,
            geom="histogram",
            main = "Histogram for All Regions",
            xlab = "AADT") + theme(panel.background = element_rect(fill = 'white', colour = 'black')) + geom_histogram(colour="black", fill="grey")

multiplot(p1, p2, p3, p4, p5, p6, cols=3)


##look for areas that have large seasonal variations 


#MTO divides the highways into four categories: the king's highways, the secondary highways, the tertiary roads, and selected 7000 series highways. the king's highways will be further grouped into the 400 series highways +  the QEW.

library(dplyr)

#select for the four highway types in the dataset
king <- filter(fil.data, hwy.type == "King")
secon <- filter(fil.data, hwy.type == "Sec")
fwy <- filter(fil.data, hwy.type == "Fwy")
tert <- filter(fil.data, hwy.type == "Tert")

#select for the 400, 401, 402, 403, 404, 405, 406, 407, 409, 410, 416, 417, 420, 427
target <- c("400", "401", "402", "403", "404", "405", "406", "407", "409", "410", "416", "417", "420", "427")
four <- filter(fil.data, hwy.num %in% target)

#select for the remaining non-400 series freeways (the QEW) 
qew <- filter(fwy, !(hwy.num %in% target))

summary(lm(sadt~aadt, data=fil.data))
plot(fil.data$sadt~fil.data$aadt, xlab="SADT", ylab="AADT", main="Plot of SADT and AADT")

fil.data %>%
  group_by(hwy.type) %>%
  ggvis (~year, ~aadt, stroke = ~factor(hwy.type)) %>%
  layer_smooths() %>%
  title= "Year ~ AADT"

##Distribution of AADT, SADT, SAWDT, and WADT ####

datadis <- select(fil.data, aadt, sadt, sawdt, wadt)
boxplot(datadis,
        main="Distribution of Data",
        ylab="Value",
        xlab="Data Type")

##Histograms of Highway Type ####

fil.data %>%
  group_by(hwy.type) %>%
  ggvis (~year, ~aadt, fill=~hwy.type) %>%
  layer_histograms() 

##Boxplots for Regions ####

par(mfrow=c(3,3))

boxplot(aadt ~ travel.pattern, data = fil.data, 
        main = "AADT for All Regions", ylab = "AADT", xlab = "Travel Pattern")  

boxplot(aadt ~ travel.pattern, data = central, 
        main = "AADT for All Central", ylab = "AADT", xlab = "Travel Pattern")   

boxplot(aadt ~ travel.pattern, data = eastern, 
        main = "AADT for All Eastern", ylab = "AADT", xlab = "Travel Pattern")   

boxplot(aadt ~ travel.pattern, data = noreast, 
        main = "AADT for All NorthEast", ylab = "AADT", xlab = "Travel Pattern")  

boxplot(aadt ~ travel.pattern, data = norwest, 
        main = "AADT for All NorthWest", ylab = "AADT", xlab = "Travel Pattern")  

boxplot(aadt ~ travel.pattern, data = west, 
        main = "AADT for All West", ylab = "AADT", xlab = "Travel Pattern")   

library(ggplot2)

p1 <- ggplot(central$aadt, aes(x=aadt) + geom_histogram(fill="#FF9999", colour="black"))

ggplot(central$aadt, aes(x=aadt) + geom_bar())

d1 <- ggplot(fil.data,aes(x=travel.pattern, y = aadt))+
  geom_boxplot(aes()) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_boxplot(colour="black", fill="grey") + ggtitle("All Regions") + xlab("AADT") +
  ylab("Frequency") 

d2 <- ggplot(central,aes(x=travel.pattern, y = aadt))+
  geom_boxplot(aes()) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_boxplot(colour="black", fill="grey") + ggtitle("Central") + xlab("AADT") +
  ylab("Frequency") 

d3 <- ggplot(eastern,aes(x=travel.pattern, y = aadt))+
  geom_boxplot(aes()) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_boxplot(colour="black", fill="grey") + ggtitle("Eastern") + xlab("AADT") +
  ylab("Frequency") 

d4 <- ggplot(noreast,aes(x=travel.pattern, y = aadt))+
  geom_boxplot(aes()) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_boxplot(colour="black", fill="grey") + ggtitle("Northeast") + xlab("AADT") +
  ylab("Frequency") 

d5 <- ggplot(norwest,aes(x=travel.pattern, y = aadt))+
  geom_boxplot(aes()) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_boxplot(colour="black", fill="grey") + ggtitle("Northwest") + xlab("AADT") +
  ylab("Frequency") 

d6 <- ggplot(west,aes(x=travel.pattern, y = aadt))+
  geom_boxplot(aes()) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) + 
  geom_boxplot(colour="black", fill="grey") + ggtitle("Western") + xlab("AADT") +
  ylab("Frequency") 

multiplot(d1, d2, d3, d4, d5, d6, cols=3)

##Frequency Tables for Region and Travel Pattern ####

# 2-Way Frequency Table 
mytable <- table(fil.data$reg,fil.data$travel.pattern) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages

##Crosstable ####
library(gmodels)
CrossTable(fil.data$reg,fil.data$travel.pattern)

chisq.test(mytable)

##Measure of Association between Region and Travel Pattern ####
library(vcd)
assocstats(mytable)

##One-Way Permutation Test based on 9999 Monte-Carlo resamplings. ####
library(coin)
oneway_test(aadt~travel.pattern, data=noreast,
            distribution=approximate(B=9999))


##Import 
library(readr)
raw <- read_csv("1371_traffic_volumes.csv")
raw2 <- raw[-1:-2,] #remove extraneous rows (French and blank row) to allow "readr" to reparse variables types correctly
raw3 <- type_convert(raw2)
str(raw3) #check datatypes

#1: Initial Cleaning
library(magrittr)
library(dplyr)

#rename columns
library(dplyr)
renamed.raw <- rename(raw3, "lhrs" = LHRS,"OS" =`O/S`, "year" = Year, "hwy.num" = `Hwy #`, "hwy.let" = `Hwy Let`, "hwy.type" = `Hwy Type (Text)`, "location" = `Location Description`, "reg" = Reg, "section.len" = `Section Length`, "conn.link.len" = `Connecting Link Length`, "sec.description" = `Secondary Description (for Connecting Links, Regional Boundarys,etc)`, "travel.pattern" = `Travel Pattern`, "dhv.percent" = `DHV %`, "directional.split" = `Directional Split`, "aadt" = AADT, "annual.aadt.change" = `Yearly % Change in AADT`, "decade.change" = `10 Year % Change in AADT`, "sadt" = SADT, "sawdt" = SAWDT, "wadt" = WADT)

#checking for NAs
length(renamed.raw$lhrs)
length(renamed.raw$year)
length(renamed.raw$aadt)
sum(is.na(renamed.raw$aadt))
sum(is.na(renamed.raw$sadt))
sum(is.na(renamed.raw$sawdt))
sum(is.na(renamed.raw$wadt))
sum(renamed.raw$year=="9999")
#there are 553 NA entries for aadt. year is also set to 9999 for these entries

#removing NAs and duplicate entries
fil.data <- renamed.raw %>%
  distinct() %>%
  filter(year!="9999")

length(fil.data$year)
length(fil.data$aadt)

glimpse(fil.data)

#convert travel.pattern and region to factor
fil.data$travel.pattern <-  factor(fil.data$travel.pattern)
fil.data$reg <- factor(fil.data$reg)
is.factor(fil.data$travel.pattern)

#grouping data by regions: Central, Eastern, Northeastern, Northwestern, West
central <- filter(fil.data, reg == "CR")
eastern <- filter(fil.data, reg == "ER")
noreast <- filter(fil.data, reg == "NE")
norwest <- filter(fil.data, reg == "NW")
west <- filter(fil.data, reg == "SW")

dim(central)
dim(eastern)
dim(noreast)
dim(norwest)
dim(west)
 
##There are `r sum(renamed.raw$year=="9999")` incomplete entries. These entries are set to Year "9999" and have no values for AADT and the related variables.

##There are `r length(fil.data$aadt)` remaining observations

saveRDS(fil.data, "fil.data.rds")
saveRDS(central, "central.rds")
saveRDS(eastern, "eastern.rds")
saveRDS(noreast, "noreast.rds")
saveRDS(norwest, "norwest.rds")
saveRDS(west, "west.rds")
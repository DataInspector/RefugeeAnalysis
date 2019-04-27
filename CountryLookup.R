#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

rm(list=ls())

library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(animate)
library(stringr)
library(leaflet)
library(ggmap)
library(OpenStreetMap)

Data <- read.csv("CleanRefugeeTable.csv", na.strings=c("NA","NaN", " ",""," ","  "))
Data <- Data[2:ncol(Data)]
head(Data)

print(nrow(Data))
Data <- na.omit(Data)
print(nrow(Data))

ColourLookup <- setNames(as.data.frame(unique(c(as.character(Data$CountryTo),as.character(Data$CountryFrom)))),"Country")
ColourLookup <- setNames(as.data.frame(ColourLookup[order(ColourLookup$Country,decreasing = TRUE),]),"Country")
coul = brewer.pal(4, "Spectral")
ColourLookup$Country <- ColourLookup$Country
ColourLookup$Colour <- colorRampPalette(coul)(nlevels(ColourLookup$Country))

Data$ColourFrom <- ColourLookup$Colour[match(Data$CountryFrom,ColourLookup$Country)]
Data$ColourTo <- ColourLookup$Colour[match(Data$CountryTo,ColourLookup$Country)]

ggmap::register_google(key = "")
Locations <- geocode(as.character(ColourLookup$Country))
ColourLookup$Lat <- Locations$lat
ColourLookup$Lon <- Locations$lon

write.csv(x = ColourLookup,"ColourLookup.csv")
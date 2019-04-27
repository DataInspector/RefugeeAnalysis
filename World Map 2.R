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

options(digits=9)

Data <- read.csv("CleanRefugeeTable.csv", na.strings=c("NA","NaN", " ",""," ","  "))
Data <- Data[2:ncol(Data)]
head(Data)

print(nrow(Data))
Data <- na.omit(Data)
print(nrow(Data))
sum(Data$Value)
Data <- Data[as.character(Data$CountryFrom) != as.character(Data$CountryTo),]
sum(Data$Value)

Data <- Data[order(Data$CountryFrom,Data$CountryTo,Data$Year),]
Data$ValueMid <- NA
for (i in 2:nrow(Data)){
  if(Data$CountryFrom[i] == Data$CountryFrom[i-1] & Data$CountryTo[i] == Data$CountryTo[i-1]){
    Data$ValueMid[i] <- mean(c(Data$Value[i],Data$Value[i-1]))
  }
}

temp <- Data[,c(1,7,3,4,5,6)]
names(temp)[2] <- "Value"
temp$Year <- temp$Year-0.5
Data <- Data[,1:6]
Data <- rbind.data.frame(Data,temp)
rm(temp)
Data <- Data[order(Data$CountryFrom,Data$CountryTo,Data$Year),]
Data <- Data[is.na(Data$Value)==FALSE,]

ColourLookup <- read.csv("ColourLookup.csv")

map <- ggmap(get_googlemap(center = c(lon = 12.567380, lat = 41.8719410),
                           zoom = 1,maptype ='roadmap',
                           color = 'color',
                           scale = 2,
                           size = c(600,600)
)) +
  scale_x_continuous(limits = c(-165, 200), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-55, 80), expand = c(0, 0))

Data <- Data[order(Data$Year),]

for (i in 39:length(unique(Data$Year))){
  print(i)
  
  temp <- Data[Data$Year==unique(Data$Year)[i],]
  
  CountryFromAgg <- setNames(as.data.frame(aggregate(temp$Value,by=list(temp$CountryFrom),FUN = sum)),c("CountryFrom","Value"))
  CountryToAgg <- setNames(as.data.frame(aggregate(temp$Value,by=list(temp$CountryTo),FUN = sum)),c("CountryTo","Value"))
  
  CountryFromAgg$Lat <- ColourLookup$Lat[match(CountryFromAgg$CountryFrom,ColourLookup$Country)]
  CountryFromAgg$Lon <- ColourLookup$Lon[match(CountryFromAgg$CountryFrom,ColourLookup$Country)]
  
  CountryToAgg$Lat <- ColourLookup$Lat[match(CountryToAgg$CountryTo,ColourLookup$Country)]
  CountryToAgg$Lon <- ColourLookup$Lon[match(CountryToAgg$CountryTo,ColourLookup$Country)]
  
  CountryFromAgg <- CountryFromAgg[is.na(CountryFromAgg$Lat)==FALSE,]
  CountryFromAgg$radius <- CountryFromAgg$Value/100000
  CountryFromAgg <- CountryFromAgg[is.na(CountryFromAgg$radius)==FALSE,]
  
  CountryToAgg <- CountryToAgg[is.na(CountryToAgg$Lat)==FALSE,]
  CountryToAgg$radius <- CountryToAgg$Value/100000
  CountryToAgg <- CountryToAgg[is.na(CountryToAgg$radius)==FALSE,]
  
  map2 <- map
  map2 + 
    geom_point(data=CountryFromAgg, aes(x=Lon, y=Lat, colour = "Origins"),size=((CountryFromAgg$Value/1000000)*8), alpha = 0.5) + 
    geom_point(data=CountryToAgg, aes(x=Lon, y=Lat, colour ="Destinations"),size=((CountryToAgg$Value/1000000)*8),  alpha = 0.5) + 
    annotate("text", x = 160, y = -45, size = 20, fontface=2, label = trunc(unique(Data$Year)[i])) +
    theme(plot.title = element_text(lineheight=.8, face="bold")) +
    scale_colour_manual(values = c("blue", "red")) +
    labs(colour = "Legend") +
    theme(legend.position = "bottom",
          legend.title = element_text(color = "Grey50", size = 14),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.y = element_blank())
  MapString <- paste(i,"map.png")
  ggsave(MapString, width=20, height=15)
}

zip(zipfile = "Images",files = list.files(pattern=".png"))
file.remove(list.files(pattern=".png"))

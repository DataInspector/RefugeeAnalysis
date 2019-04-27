#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(animate)
library(stringr)
library(leaflet)
library(ggmap)
library(OpenStreetMap)

rm(list=ls())

Data <- read.csv("CleanRefugeeTable.csv", na.strings=c("NA","NaN", " ",""," ","  "))
Data <- Data[2:ncol(Data)]
head(Data)

print(nrow(Data))
Data <- na.omit(Data)
print(nrow(Data))

DataNoConToCon <- Data[as.character(Data$ContinentFrom) != as.character(Data$ContinentTo),]
print(nrow(DataNoConToCon))

ContinentFromAgg <- setNames(as.data.frame(sapply(as.character(unique(DataNoConToCon$ContinentFrom)), FUN = function(x){sum(DataNoConToCon$Value[DataNoConToCon$ContinentFrom ==x])})),"Refugees")
ContinentFromAgg$ContinentFrom <- row.names(ContinentFromAgg)
row.names(ContinentFromAgg) <- NULL
ContinentFromAgg <- ContinentFromAgg[order(ContinentFromAgg$Refugees, decreasing = TRUE),]
print(ContinentFromAgg)
ggplot(data=ContinentFromAgg, aes(x=ContinentFrom, y=Refugees)) +
  geom_bar(stat="identity")

ContinentToAgg <- setNames(as.data.frame(sapply(as.character(unique(DataNoConToCon$ContinentTo)), FUN = function(x){sum(DataNoConToCon$Value[DataNoConToCon$ContinentTo ==x])})),"Refugees")
ContinentToAgg$ContinentTo <- row.names(ContinentToAgg)
row.names(ContinentToAgg) <- NULL
ContinentToAgg <- ContinentToAgg[order(ContinentToAgg$Refugees, decreasing = TRUE),]
print(ContinentToAgg)
ggplot(data=ContinentToAgg, aes(x=ContinentTo, y=Refugees)) +
  geom_bar(stat="identity")

YearAgg <- setNames(as.data.frame(sapply(as.character(unique(Data$Year)), FUN = function(x){sum(Data$Value[Data$Year ==x])})),"Refugees")
YearAgg$Year <- row.names(YearAgg)
row.names(YearAgg) <- NULL
YearAgg <- YearAgg[order(YearAgg$Year, decreasing = FALSE),]
#print(YearAgg)
ggplot(data=YearAgg, aes(x=Year, y=Refugees, group = 1)) +
  geom_line()

YearAgg <- setNames(as.data.frame(sapply(as.character(unique(Data$Year)), FUN = function(x){sum(Data$Value[Data$Year ==x])})),"Refugees")
YearAgg$Year <- row.names(YearAgg)
row.names(YearAgg) <- NULL
YearAgg <- YearAgg[order(YearAgg$Year, decreasing = FALSE),]
#print(YearAgg)
ggplot(data=YearAgg, aes(x=Year, y=Refugees, group = 1)) +
  geom_line()

Data$Decade <- as.numeric(substr(Data$Year,1,3))*10
DecadeConFromAgg <- setNames(as.data.frame(aggregate(Data$Value, by=list(Data$Decade,Data$CountryFrom), FUN = sum)),c("Decade","CountryFrom","Refugees"))
DecadeConFromAgg <- DecadeConFromAgg[order(DecadeConFromAgg$Refugees, decreasing = TRUE),]
DecadeConFromAgg <- DecadeConFromAgg[order(DecadeConFromAgg$CountryFrom,DecadeConFromAgg$Decade, decreasing = FALSE),]

ColourLookup <- setNames(as.data.frame(unique(Data$CountryFrom)),"CountryFrom")
ColourLookup <- setNames(as.data.frame(ColourLookup[order(ColourLookup$CountryFrom,decreasing = TRUE),]),"CountryFrom")
coul = brewer.pal(4, "Spectral")
ColourLookup$Colour <- colorRampPalette(coul)(nlevels(ColourLookup$CountryFrom))
Data$Colour <- ColourLookup$Colour[match(Data$CountryFrom,ColourLookup$CountryFrom)]

PlotList = list()
for (i in 1:length(unique(Data$Year))){
  #for (i in 30:33){
  DataYear <- Data[Data$Year==unique(Data$Year)[i],]  
  DataYear <- setNames(as.data.frame(aggregate(DataYear$Value,by=list(DataYear$CountryFrom),FUN=sum)),c("CountryFrom","Value"))
  
  DataYear$CountryFromPad <-str_pad(DataYear$CountryFrom, 40, side = c("left"), pad = " ")
  
  DataYear$CountryFromPad <- factor(DataYear$CountryFromPad, levels = DataYear$CountryFromPad[order(DataYear$Value)])
  DataYear$Colour <- ColourLookup$Colour[match(DataYear$CountryFrom,ColourLookup$CountryFrom)]
  DataYear <- DataYear[order(DataYear$Value,decreasing = TRUE),]
  DataYear <- DataYear[1:10,]
  plot <- ggplot(data=DataYear,aes(x=CountryFromPad,y=Value, fill =Colour, Colour=Colour))+ 
    geom_bar(stat="identity") + 
    geom_text(label = DataYear$Value, hjust = -0.2) + 
    coord_flip() + 
    scale_fill_manual(values=DataYear$Colour) + 
    ggtitle("Refugee Movement") +
    xlab("Number Of Refugees") +
    ylab("Country Of Origin") +
    #theme_minimal() +
    theme(legend.position="none", 
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(10, 10, 10, 50),
          panel.grid.major.y = element_blank() ,
          # explicitly set the horizontal lines (or they will disappear too)
          panel.grid.major.x = element_line( size=.05, color="black" ) ) +
    expand_limits(y = max(DataYear$Value)*1.2) +
    annotate("text", x = 2, y = max(DataYear$Value)*0.8, size = 20, fontface=2, label = unique(Data$Year)[i])
  #print(plot)
  PlotList[[i]] <- plot
  
}

# Save plots to tiff. Makes a separate file for each plot.
for (i in 1:length(unique(Data$Year))){
  #for (i in 30:33) {
  file_name = paste("plot", i, ".png", sep="")
  png(file_name, width = 1200,height = 800)
  print(PlotList[i])
  dev.off()
}
zip(zipfile = "Images",files = list.files(pattern=".png"))
file.remove(list.files(pattern=".png"))
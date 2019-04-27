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
CountryFromAgg <- setNames(aggregate(Data$Value, by = list(Data$CountryFrom, Data$Year), FUN = sum),c("Country","Year","Refugees"))

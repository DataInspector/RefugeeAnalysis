#----Set Up Environment----#

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(gganimate)
library(stringr)
library(leaflet)
library(ggmap)
library(gifski)

#----Read Data---#

Data <-
  read.csv("CleanRefugeeTable.csv",
           na.strings = c("NA", "NaN", " ", "", " ", "  "))
Data <- Data[2:ncol(Data)]
head(Data)

print(nrow(Data))
Data <- na.omit(Data)
print(nrow(Data))

#----Create Hex Code Colours For Each Country----#

ColourLookup <-
  setNames(as.data.frame(unique(c(
    as.character(Data$CountryTo),
    as.character(Data$CountryFrom)
  ))), "Country")
ColourLookup <-
  setNames(as.data.frame(ColourLookup[order(ColourLookup$Country, decreasing = TRUE), ]), "Country")
coul = brewer.pal(4, "Spectral")
ColourLookup$Country <- ColourLookup$Country
ColourLookup$Colour <-
  colorRampPalette(coul)(nlevels(ColourLookup$Country))
Data$ColourFrom <-
  ColourLookup$Colour[match(Data$CountryFrom, ColourLookup$Country)]
Data$ColourTo <-
  ColourLookup$Colour[match(Data$CountryTo, ColourLookup$Country)]

#----Pull Lat & Long For Each Country Using Google Maps API----#

ggmap::register_google(key = "")
Locations <- geocode(as.character(ColourLookup$Country))
ColourLookup$Lat <- Locations$lat
ColourLookup$Lon <- Locations$lon

#----Fix Broken LAT LON----#

ColourLookup$textAddress <-
  mapply(
    FUN = function(Lon, Lat)
      revgeocode(c(Lon, Lat)),
    ColourLookup$Lon,
    ColourLookup$Lat
  )
ColourLookup[ColourLookup$Country == "Syrian Arab Rep.", c("Lat", "Lon")] <-
  c(34.8021, 38.9968)
ColourLookup[ColourLookup$Country == "Western Sahara", c("Lat", "Lon")] <-
  c(24.2155, 12.8858)
ColourLookup[ColourLookup$Country == "Jordan", c("Lat", "Lon")] <-
  c(30.5852, 36.2384)
ColourLookup[ColourLookup$Country == "Rep. of Korea", c("Lat", "Lon")] <-
  c(35.9078, 127.7669)
ColourLookup[ColourLookup$Country == "Georgia", c("Lat", "Lon")] <-
  c(32.1656, 82.9001)
ColourLookup[ColourLookup$Country == "Saint Vincent and the Grenadines", c("Lat", "Lon")] <-
  c(12.9843, 61.2872)
ColourLookup[ColourLookup$Country == "Samoa", c("Lat", "Lon")] <-
  c(13.7590, 172.1046)
ColourLookup[ColourLookup$Country == "Tuvalu", c("Lat", "Lon")] <-
  c(7.1095, 177.6493)
ColourLookup[ColourLookup$Country == "Tonga", c("Lat", "Lon")] <-
  c(21.1790, 175.1982)
ColourLookup[ColourLookup$Country == "Kiribati", c("Lat", "Lon")] <-
  c(3.3704, 168.7340)
ColourLookup[ColourLookup$Country == "Kiribati", c("Lat", "Lon")] <-
  c(3.3704, 168.7340)
ColourLookup <- ColourLookup[1:4]

#----Write Lookup Table----#

write.csv(x = ColourLookup, "ColourLookup.csv")

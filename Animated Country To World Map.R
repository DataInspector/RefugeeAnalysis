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

options(digits = 9)

#----Read Data---#

Data <-
  read.csv("CleanRefugeeTable.csv",
           na.strings = c("NA", "NaN", " ", "", " ", "  "))
Data <- Data[2:ncol(Data)]
head(Data)

ColourLookup <- read.csv("ColourLookup.csv")

#----Remove Data Which Consists Of Inter Country Refugees---#

print(nrow(Data))
Data <- na.omit(Data)
print(nrow(Data))
sum(Data$Value)
Data <-
  Data[as.character(Data$CountryTo) != as.character(Data$CountryFrom), ]
sum(Data$Value)
Data <- Data[order(Data$CountryTo, Data$CountryTo, Data$Year), ]
Data <- Data[is.na(Data$Value) == FALSE, ]

#----Aggregate By Country From & Year----#

CountryToAgg <-
  setNames(aggregate(
    Data$Value,
    by = list(Data$CountryTo, Data$Year),
    FUN = sum
  ),
  c("Country", "Year", "Refugees"))

#----Pull In Coordinates----#

CountryToAgg$Lat <-
  ColourLookup$Lat[match(CountryToAgg$Country, ColourLookup$Country)]
CountryToAgg$Lon <-
  ColourLookup$Lon[match(CountryToAgg$Country, ColourLookup$Country)]
CountryToAgg <- CountryToAgg[is.na(CountryToAgg$Lat) == FALSE, ]

#----Add Label For Map Title----#

CountryToAgg$TitleLabel = paste("Refugee Inflow", CountryToAgg$Year)

#----Filter Data To Remove Sparse Years----#

CountryToAgg <- CountryToAgg[CountryToAgg$Year >= 1965,]

#----Repeat The Last 3 Years To Add An End Frame Pause Before GIF Replays----#

temp <- CountryToAgg[CountryToAgg$Year == max(CountryToAgg$Year),]
temp$Year <- temp$Year + 0.1
CountryToAgg <- rbind(CountryToAgg, temp)
temp <- CountryToAgg[CountryToAgg$Year == max(CountryToAgg$Year),]
temp$Year <- temp$Year + 0.1
CountryToAgg <- rbind(CountryToAgg, temp)
CountryToAgg$Year2 <- round(CountryToAgg$Year)

#----Pull The ggmap In & Format----#

ggmap::register_google(key = "AIzaSyDYQKGvFscHLSBHWm8VSJ6O5LOne08gbCs")

map <-
  ggmap(
    get_googlemap(
      center = c(lon = 12.567380, lat = 41.8719410),
      zoom = 1,
      maptype = 'roadmap',
      color = 'color',
      scale = 2,
      size = c(600, 600)
    )
  ) +
  scale_x_continuous(limits = c(-165, 200), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-55, 80), expand = c(0, 0))

#----Build Animated Map----#

map2 <- map + #Add Colour Circles
  annotate(
    "text",
    x = 15,
    y = 0,
    label = "www.TheDataInspector.com",
    vjust = -30,
    hjust = -1.5,
    colour = "black",
    size = 6
  ) + 
  geom_point(
    data = CountryToAgg,
    aes(x = Lon, y = Lat, size = Refugees),
    alpha = 0.5,
    stat = "identity",
    show.legend = TRUE,
    colour = "Blue"
  ) +
  scale_size_continuous(
    range = c(0, 30),
    name = "Refugees",
    #Scale Circle Sizes & Format Legend
    breaks = c(250000, 500000, 1000000, 2000000, 4000000),
    labels = format(
      c(250000, 500000, 1000000, 2000000, 4000000),
      big.mark = ",",
      scientific = FALSE
    )
  ) +
  geom_text(
    data = CountryToAgg,
    #Add A Label For The Title
    aes(
      x = 10,
      y = 0,
      label = as.character(TitleLabel),
      fontface = 2
    ),
    size = 14,
    vjust = -13,
    colour = "black",
    check_overlap = TRUE
  ) +
  labs(colour = "Legend") +
  scale_fill_discrete(name = "Refugees", labels = c("A", "B", "C")) + #Format Legend Labels
  theme(
    legend.position = "bottom",
    #Change Plot Themes
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 25),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(1, 1, 2, 1, "cm"),
    plot.background = element_blank()
  ) + #Add A Long Transition & Small State To Give Effect Of Constantly Changing Volumes
  transition_states(Year)

#----Render Animation----#

animate(
  map2,
  height = 1000,
  width = 1200,
  nframes = 120,
  fps = 5,
  renderer = gifski_renderer("RefugeesToMap.gif")
)

#----Set Up Environment----#

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(gganimate)
library(stringr)
library(leaflet)
library(ggmap)
library(gifski)

rm(list = ls())

options(digits = 9)

#----Read Data---#

Data <-
  read.csv("CleanRefugeeTable.csv",
           na.strings = c("NA", "NaN", " ", "", " ", "  "))

Data <- Data[2:ncol(Data)]
head(Data)

print(nrow(Data))
Data <- na.omit(Data)
print(nrow(Data))
sum(Data$Value)
Data <-
  Data[as.character(Data$CountryFrom) != as.character(Data$CountryTo),]
sum(Data$Value)

#----Create Aggregated View Of Coutnry From----#

Data <- Data[order(Data$CountryFrom, Data$CountryTo, Data$Year),]
CountryFromAgg <-
  setNames(aggregate(
    Data$Value,
    by = list(Data$CountryFrom, Data$Year),
    FUN = sum
  ),
  c("Country", "Year", "Refugees"))

#----Add 3 Year Rolling Yearly Average Column----#

CountryFromAgg$Rolling <- NA
CountryFromAgg <-
  CountryFromAgg[order(CountryFromAgg$Country, CountryFromAgg$Year),]

for (i in 3:nrow(CountryFromAgg)) {
  temp <- CountryFromAgg[(i - 2):i,]
  temp <- temp[temp$Country == CountryFromAgg$Country[i],]
  CountryFromAgg$Rolling[i] <- as.integer(mean(temp$Refugees))
}

#----Create Data Frame With Top 10 Each Year----#

Data2 <- CountryFromAgg %>%
  group_by(Year) %>%
  mutate(rank = rank(-Refugees)) %>%
  group_by(Country) %>%
  filter(rank <= 10) %>%
  ungroup()

Data2$Country <- as.factor(as.character(Data2$Country))

#----Create Column With Labels Explaining Current Rolling Average Period---#

Data2$YearLabels <- NA

for (i in 3:nrow(Data2)) {
  if (Data2$Country[i - 2] == Data2$Country[i]) {
    Data2$YearLabels[i] <-
      paste("Average", Data2$Year[i - 2], "-", Data2$Year[i])
    
  } else if (Data2$Country[i - 1] == Data2$Country[i]) {
    Data2$YearLabels[i] <-
      paste("Average", Data2$Year[i - 1], "-", Data2$Year[i])
  } else {
    Data2$YearLabels[i] <- paste("Average", Data2$Year[i])
  }
}

Data2$YearLabels[1] <- paste("Average", Data2$Year[1])
Data2$YearLabels[2] <-
  paste("Average", Data2$Year[1], "-", Data2$Year[2])

#----Add Column Showing Refugees With Formatting (,)---#

Data2$RefugeesFormatted <-
  format(Data2$Refugees, big.mark = ",", scientific = FALSE)

#----Added Padding Spaces To Prevent Overlapping In Bar Chart----#
Data2$RefugeesFormatted <-
  paste("  ", Data2$RefugeesFormatted, sep = "")

#----Filter Data To Remove Sparse Years----#

Data3 <- Data2[Data2$Year >= 1965,]
#Data3 <- Data2[Data2$Year > 1990 & Data2$Year < 2000,]

#----Repeat The Last 3 Years To Add An End Frame Pause Before GIF Replays----#

temp <- Data3[Data3$Year == max(Data3$Year),]
temp$Year <- temp$Year + 0.1
Data3 <- rbind(Data3, temp)
temp <- Data3[Data3$Year == max(Data3$Year),]
temp$Year <- temp$Year + 0.1
Data3 <- rbind(Data3, temp)
Data3$Year2 <- round(Data3$Year)

#----Create Title Variable With Years For Moving Animation----#

Data3$RefugeesSum1 <- as.character(paste("Refugees", Data3$Year2))

#----Create Total Refugees Column With Total For Moving Animation----#

Data3$RefugeesSum2 <-
  as.character(paste(
    "Total:",
    str_pad(
      string = as.character(trimws(
        format(
          sapply(
            Data3$Year,
            FUN = function(x) {
              sum(Data3$Refugees[Data3$Year == x])
            }
          )
          ,
          big.mark = ",",
          scientific = FALSE
        )
      )),
      width = 10,
      side = c("right"),
      pad = " "
    ),
    sep = " "
  ))
Data3$Country <- as.factor(as.character(Data3$Country))

#----Create HEX Code Colour Lookup For Countries---#

ColourLookup <-
  setNames(as.data.frame(unique(c(
    as.character(Data3$Country)
  ))), "Country")
coul = brewer.pal(4, "Spectral")
ColourLookup$Colour <-
  colorRampPalette(coul)(nlevels(ColourLookup$Country))
Data3$Colour <-
  ColourLookup$Colour[match(Data3$Country, ColourLookup$Country)]

#----Create Graph For Animation----#

Graphs <- ggplot(Data3, aes(
  rank,
  group = Country,
  fill = Colour,
  colour = Colour
)) + #Add Column Bar Chart With The Refugees
  geom_col(aes(y = Refugees), alpha = 1) +
  geom_text(
    aes(y = 0, label = paste(Country, " ")),
    vjust = 0.2,
    hjust = 1,
    size = 8
  ) + #Add Label Showing The Number Of Refugees For Each Country
  geom_text(aes(y = Refugees, label = as.character(RefugeesFormatted)),
            size = 8,
            nudge_y = 400000) + #Add Label For Title Showing Current Year
  geom_text(
    aes(
      y = max(Data3$Refugees) * 0.475,
      x = 1,
      label = as.character(RefugeesSum1),
      fontface = 2
    ),
    size = 12,
    vjust = -2.5,
    colour = "#8D8D8D",
    check_overlap = TRUE
  ) + #Add Label For Total Refugees In That Year
  geom_text(
    aes(
      y = max(Data3$Refugees) * 0.13,
      x = 1,
      label = RefugeesSum2,
      fontface = 2
    ),
    size = 10,
    vjust = -3,
    colour = "#8D8D8D",
    check_overlap = TRUE
  ) + #Format Graph
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) + #Add Watermark With Website Name
  annotate(
    "text",
    x = 1,
    y = max(Data3$Refugees) * 0.82,
    label = "www.TheDataInspector.com",
    vjust = -5.5,
    colour = "#8D8D8D",
    size = 6
  ) +
  #labs(title = "Refugees")+
  theme(
    #Set Theme Options
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(size = .05, colour = "#DBDBDB"),
    panel.grid.minor.x = element_line(size = .05, colour = "#DBDBDB"),
    plot.title = element_text(
      size = 30,
      hjust = 0.5,
      face = "bold",
      colour = "#8D8D8D",
      vjust = 5
    ),
    plot.background = element_blank(),
    plot.margin = margin(3, 2, 2, 10, "cm")
  ) + #Add Transition State Controlling Time Spent Moving And Time Spent At Data Point
  transition_states(Year, transition_length = 30, state_length = 20) + #Control If X & Y Axis Should Scale As Data Changes
  view_follow(fixed_x = TRUE, fixed_y = TRUE)

#----Render Animation----#

animate(
  Graphs,
  height = 1000,
  width = 1200,
  nframes = 1200,
  fps = 25,
  renderer = gifski_renderer("RefugeesFromBar.gif")
)
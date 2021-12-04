# Load needed packages
library(tidyr)
library(ggplot2)
library(ggtext)
library(dplyr)
library(readxl)
library(here)

# Load some custom functions

# returns med, min, and max of vector x
median_min_max <- function(x) { 
  m <- median(x, na.rm = TRUE)
  ymin <- min(x, na.rm = TRUE)
  ymax <- max(x, na.rm = TRUE)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# modified stat_summary function for use in ggplot construction
stat_sum_df <- function(geom="crossbar", ...) { 
  stat_summary(fun.y="min",
               fun.ymin = "min",
               fun.ymax = "max",
               colour="black",
               geom=geom,
               width=0.2, ...)
}

# function to build and save analysis plots
GabePlot_IAQ <- function(AirData, ClientData, ClientName = "Client Name", SaveImage = FALSE, SaveDirectory = getwd()) {
  
  # browser()
  # ClientName <- as.character(Client_Sample[,1])
  # ClientData <- Client_Sample[,-1]
  # AirData <- Air_Sample2[,-1]
  
  # pm2.5.min <- min(c(AirData$Kitchen_PM2.5, AirData$Living_Room_PM2.5), na.rm = TRUE)
  # pm2.5.max <- max(c(AirData$Kitchen_PM2.5, AirData$Living_Room_PM2.5), na.rm = TRUE)
  # pm10.min <- min(c(AirData$Kitchen_PM10, AirData$Living_Room_PM10), na.rm = TRUE)
  # pm10.max <- max(c(AirData$Kitchen_PM10, AirData$Living_Room_PM10), na.rm = TRUE)
  # 
  # AirData.pm2.5 <- data.frame(apply(AirData[,c(1,3)], 2, function(x){(x-pm2.5.min)/(pm2.5.max-pm2.5.min)}))
  # AirData.pm10 <- data.frame(apply(AirData[,c(2,4)], 2, function(x){(x-pm10.min)/(pm10.max-pm10.min)}))
  # AirData2 <- data.frame("Kitchen_PM2.5" = AirData.pm2.5$Kitchen_PM2.5,
  #                        "Kitchen_PM10" = AirData.pm10$Kitchen_PM10,
  #                        "Living_Room_PM2.5" = AirData.pm2.5$Living_Room_PM2.5,
  #                        "Living_Room_PM10" = AirData.pm10$Living_Room_PM10)
  
  AirData.L <- data.frame("PM2.5" = c(AirData$Kitchen_PM2.5, AirData$Living_Room_PM2.5),
                          "PM10" = c(AirData$Kitchen_PM10, AirData$Living_Room_PM10))
  
  AirData2 <- data.frame(apply(AirData.L, 2, function(x){(x-min(x))/(max(x)-min(x))}))
  
  ClientData2 <- data.frame(key = colnames(AirData2),
                            room = c("Kitchen", "Kitchen", "Living Room", "Living Room"),
                            value = as.vector(t(ClientData)))
  ClientData2$value <- (ClientData2$value-apply(AirData.L,2,min))/(apply(AirData.L,2,max)-apply(AirData.L,2,min))
  
  AirData3 <- gather(AirData2)
  AirData3$key <- factor(AirData3$key, levels = c("PM10", "PM2.5"))
  
  p <- ggplot(AirData3, aes(x = key, y = value)) + 
    geom_segment(data = ClientData2, aes(y = value, yend = value),
                 x = c(2.23,1.23,2.23,1.23), xend = c(2.3,1.3,2.5,1.5)) +
    stat_sum_df(fill="blue") +
    coord_flip(ylim = c(-.15, 1.15)) +
    geom_text(data = data.frame(key = colnames(AirData2), value = apply(AirData2, 2, min)),
              label = round(apply(AirData.L, 2, min),1), 
              position = position_nudge(x = -.21, y = 0), hjust = .5) +
    geom_text(data = data.frame(key = colnames(AirData2), value = -0.04),
              label = "Lowest:", 
              position = position_nudge(x = -.21, y = 0), hjust = 1) +
    geom_text(data = data.frame(key = colnames(AirData2), value = apply(AirData2, 2, max)),
              label = round(apply(AirData.L, 2, max),1), 
              position = position_nudge(x = -.21, y = 0), hjust = 0.5) +
    geom_text(data = data.frame(key = colnames(AirData2), value = 1.04),
              label = ":Highest", 
              position = position_nudge(x = -.21, y = 0), hjust = 0) +
    geom_point(data = ClientData2, aes(fill = room), shape = 25, size = 4,
               position = position_nudge(x = .23, y = 0)) +
    #Change the hexidecimal values in line 82 to change the arrow colors.
    scale_fill_manual(values = c("#ffcc00","#8a9dff"), guide = FALSE) +
    #
    #Kitchen
    #Change the hexidecimal value in line 89 to change the kitchen box color.
    geom_textbox(data = ClientData2[1:2,],
              label = as.character(paste0(ClientData2$room[1:2], ": ", round(ClientData[1:2],1), " (ug/m3)")), 
              position = position_nudge(x = .35, y = 0), 
              col = 'black', fill = "#ffcc00", width = unit(4.2, "cm")) +
    #Living Room
    #Change the hexidecimal value in line 95 to change the kitchen box color.
    geom_textbox(data = ClientData2[3:4,],
              label = as.character(paste0(ClientData2$room[3:4], ": ", round(ClientData[3:4],1), " (ug/m3)")), 
              position = position_nudge(x = .50, y = 0),
              col = 'black', fill = "#8a9dff", width = unit(5.2, "cm")) +
    #
    labs(title = paste0("Indoor air monitoring results for: ", as.character(ClientName)),
         caption = paste0("Report Date: ", Sys.Date())
         ) +
    theme_classic() +
    theme(title = element_text(face = 'bold', size = 14),
          axis.text.y = element_text(face = 'bold', size = 16),
          axis.title = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()
          )
  
  if(SaveImage == TRUE) {
    ggsave(filename = paste0(SaveDirectory, "/", ClientName, "_", Sys.Date(), ".pdf"), plot = p, device = "pdf",
           units = "in", width = 8, height = 5.5)
    ggsave(filename = paste0(SaveDirectory, "/", ClientName, "_", Sys.Date(), ".png"), plot = p, device = "png",
           units = "in", width = 8, height = 5.5)
  } else {
    return(p)
  }

}

# Load data, select specific columns
Air_Sample <- read_excel(here("Data/IAQSampleComposite.xlsx")) ### Cleaned master data file prior to upload
Air_Sample2 <- Air_Sample %>%
  select(Name = "Names",
         Kitchen_PM2.5 = "KPM2.5",
         Kitchen_PM10 = "KPM10",
         Living_Room_PM2.5 = "LRPM2.5",
         Living_Room_PM10 = "LRPM10") # Fixed issue with selection with 2 LRPM2.5 names

# If needed the loop below will Replace 0, ND, and NA for each element with min(element)*.25 
pm2.5.min <- min(c(Air_Sample2$Kitchen_PM2.5, Air_Sample2$Living_Room_PM2.5), na.rm = TRUE)
pm2.5.max <- max(c(Air_Sample2$Kitchen_PM2.5, Air_Sample2$Living_Room_PM2.5), na.rm = TRUE)
pm10.min <- min(c(Air_Sample2$Kitchen_PM10, Air_Sample2$Living_Room_PM10), na.rm = TRUE)
pm10.max <- max(c(Air_Sample2$Kitchen_PM10, Air_Sample2$Living_Room_PM10), na.rm = TRUE)

for (i in c(2,4)) {
  Air_Sample2[,i] <- Air_Sample2[,i] %>% na_if(0) %>% na_if("ND")
  Air_Sample2[,i] <- mutate_all(Air_Sample2[,i], .funs = funs(ifelse(is.na(.), (pm2.5.min/4) , .)))
}
for (i in c(3,5)) {
  Air_Sample2[,i] <- Air_Sample2[,i] %>% na_if(0) %>% na_if("ND")
  Air_Sample2[,i] <- mutate_all(Air_Sample2[,i], .funs = funs(ifelse(is.na(.), (pm10.min/4) , .)))
}

# Check dimensions and view summary stats to make sure data was read in correctly and looks accurate
dim(Air_Sample2)
summary(Air_Sample2)

# i = 1

# Build analysis plots by running the below loop.
# !!!! Be sure the Results folder exists in the R project directory before proceeding !!!!
# When the code successfully runs, you can find the png and pdf files of the figures in the Results folder.
for (i in 1:nrow(Air_Sample2)) {

  Client_Sample<- Air_Sample2[i,]
  
  GabePlot_IAQ(AirData = Air_Sample2[,-1], ClientData = Client_Sample[-1], ClientName = Client_Sample[1],
           SaveImage = TRUE, SaveDirectory = here("Results/May_23_2021"))
}

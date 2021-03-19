library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(lubridate)
library(wordcloud)
library(reshape)
library(tm)
library(wordcloud2)

my.dir <- "C:\\Users\\Kyle\\Documents\\IST719 Poster\\"
df <- read.csv(paste0(my.dir, "BHCData.csv")
                   , header = TRUE
                   , stringsAsFactors = FALSE)

df <- na.omit(df)
df$Date <- strptime(df$Date, '%m/%d/%Y')
df <- df[order(df$Date),]
df$Year <- format(as.Date(df$Date, format="%m/%d/%Y"), "%Y")
df$Year <- as.Date(df$Year)
df <- df %>% mutate(Count=row_number())
colnames(df)[16] <- 'AttemptNumber'
df <- df %>% group_by(Result) %>% mutate(Count=row_number())
df$Count[df$Result=='Miss'] <- 0
colnames(df)[17] <- 'MakeNumber'
df$Qtr[df$Qtr=='2OT'] <- 'OT2'
df$Qtr[df$Qtr=='3OT'] <- 'OT3'

Players <- df %>% count(Player, Result)
Players <- spread(Players, Result, n)
Players[is.na(Players)] <- 0
Players$Attempt <- Players$Make + Players$Miss
Players$Percent <- Players$Make/Players$Attempt
Players$MPercent <- Players$Miss/Players$Attempt

Makes <- df[df$Result=='Make',]

Final <- Makes %>% count(Final)
colnames(Final)[2] <- 'Result'

myPalette <- brewer.pal(5, "Set2") 
# pie chart win/loss
pie(Final$Result, labels = c("Loss", "Win"), border = "white", col=myPalette)


FourQ <- Makes[Makes$Qtr=='4th',]

col.vec <- rep(rgb(64,64,255, maxColorValue = 255), nrow(FourQ))
col.vec[FourQ$Final=='W'] <- (rgb(255,64,64, maxColorValue = 255))
#scatterplot buzzer beater distance
plot(FourQ$Date, FourQ$FT, pch=16, cex=1.8, xlab = "Date of Made Beyond Half Court Shot"
     , ylab="Distance from Basket (in feet)", main="Fourth Quarter Buzzer Beaters"
     , cex.main=1.22, cex.lab=1, col=col.vec)

p <- ggplot(Players, aes(x=x))+
        geom_density( aes(x = Miss, y = ..density..), fill= "#404080") +
        geom_label(aes(x=65, y = 0.075, label="Misses"), color="#404080") +
        theme_ipsum() +
        xlab("Density of Misses by Players")
# Density plot
p        

TopPlayers <- Players[Players$Make >1,]

data <- TopPlayers
data$Make <- as.factor(data$Make)


empty_bar <- 2
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Make), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$Make <- rep(levels(data$Make), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(Make)
data$id <- seq(1, nrow(data))

label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

base_data <- data %>% 
        group_by(Make) %>% 
        summarize(start=min(id), end=max(id) - empty_bar) %>% 
        rowwise() %>% 
        mutate(title=mean(c(start, end)))

grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


tp <- ggplot(data, aes(x=as.factor(id), y=Attempt, fill=Make)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
        geom_bar(stat="identity", alpha=0.5) +
        ylim(-100,120) +
        theme_minimal() +
        theme(
                legend.position = "none",
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank(),
                plot.margin = unit(rep(-1,4), "cm") 
        ) +
        coord_polar() + 
        geom_text(data=label_data, aes(x=id, y=Attempt +5, label=Player, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

par(mar=c(0,0,0,0))
tp






#clear working space
rm(list = ls())
#clear console
cat("\014")
graphics.off()
#set working directory
setwd("/Users/Manu/Desktop/Master USA/NYU/Jobs/CAN Lab/Tao project/Data Analysis/Circular Tracking")

#load library
library(ggplot2)


createCircle <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

remove_outliers <- function(data_in, na.rm = TRUE) {
  qnt <- quantile(data_in[,1], probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(data_in[,1], na.rm = na.rm)
  y <- data_in[,1]
  y[data_in[,1] < (qnt[1] - H)] <- NA
  y[data_in[,1] > (qnt[2] + H)] <- NA
  y
}


#read data
data.700 <- read.csv("coordinates1.csv")

colnames(data.700) <- c("Pos_x", "Pos_y",
                        "Destination_x", "Destination_y",
                        "Starting_x", "Starting_y",
                        "Ind_trial")

data.700.valid <- data.700[data.700$Ind_trial != 0,] #Remove trial 0
data.700.valid <- data.700.valid[data.700.valid$Ind_trial %% 2 != 0,] #Remove even trials (returns)

circleOut <- createCircle(c(150,-100),130,npoints = 100)
circleIn <- createCircle(c(150,-100),190,npoints = 100)
circleIdeal <- createCircle(c(150,-100),160,npoints = 100)

path_graph_valid <- ggplot() + geom_path(data = data.700.valid, aes(x = Pos_x, y = -Pos_y, group = Ind_trial),colour = "black") + 
                              geom_point(data=NULL, aes(x = 150, y = -100)) + 
                              geom_point(data=NULL, aes(x = 283, y = -40), colour ="red") + 
                              geom_path(data=circleIn, aes(x,y),colour ="blue") + 
                              geom_path(data=circleOut, aes(x,y),colour ="blue") + 
                              geom_path(data=circleIdeal, aes(x,y),colour ="green")

path_graph_valid


trials <- unique(data.700.valid$Ind_trial)
Measure <- vector(mode = 'numeric', length = length(trials)) #Empty vector where we will store the costs

Target_Pos <- data.700.valid[,c(3,4,7)]

aux = 1
for (i in trials[1:(length(trials))]){
  trial <- data.700.valid[data.700.valid$Ind_trial==i,] #Piece of the data of current trial
  distance <- trial[,c(1,2)] - trial[,c(3,4)]
  M <- ((distance[,1])^2 + (distance[,2])^2)^0.5 #Cost function
  Measure[aux] <- sum(M)
  aux = aux + 1
}

Measure <- as.data.frame(Measure) #Set measure as table
Measure$Trial <- seq(1,max(data.700.valid$Ind_trial),by=2) #Set absolute number of trials

scatter_graph <- ggplot(data = Measure, aes(x = Trial, y = Measure)) +
  geom_point(colour = "blue")

scatter_graph #Displays the graph

linear_regression <- ggplot() + geom_smooth(mapping = aes(x = Trial, y = Measure), method="loess", data = Measure)

linear_regression

combined_graphs <- scatter_graph + geom_smooth(mapping = aes(x = Trial, y = Measure), method="loess", data = Measure)

combined_graphs
# 
# boxplot_graph <- ggplot() + geom_boxplot(mapping = aes(x = Trial, y = Measure), data = Measure, fill = "green")
# 
# boxplot_graph

y <- remove_outliers(Measure)
Measure$Measure <- y

scatter_graph_no_outliers <- ggplot(data = Measure, aes(x = Trial, y = Measure)) +
  geom_point(colour = "blue")

scatter_graph_no_outliers #Displays the graph

linear_regression_no_outliers <- ggplot() + geom_smooth(mapping = aes(x = Trial, y = Measure), method="loess", data = Measure)

linear_regression_no_outliers

combined_graphs_no_outliers <- scatter_graph_no_outliers + geom_smooth(mapping = aes(x = Trial, y = Measure), method="loess", data = Measure)

combined_graphs_no_outliers
# 
# boxplot_graph <- ggplot() + geom_boxplot(mapping = aes(x = Trial, y = Measure), data = Measure, fill = "green")

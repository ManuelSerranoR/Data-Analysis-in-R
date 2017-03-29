#clear working space
rm(list = ls())
#clear console
cat("\014")
graphics.off()
#set working directory
setwd("Directory_Path")

#load library
library(ggplot2)

#read data
data.700 <- read.csv("coordinatesEric0200pm.csv")

colnames(data.700) <- c("f_Pos_Scr_x", "f_Pos_Scr_y",
                        "f_Pos_World_x", "f_Pos_World_y",
                        "Start_Pos_World_x", "Start_Pos_World_y",
                        "Ind_trial", "Frame_trial", "Type_of_trayectory", "bflag", "sflag", "dflag")

data.bad <- data.700[data.700$bflag == 1,] #Trials that do not count (after breaks)
bad.trial <- unique(data.bad$Ind_trial) #Trials where we have data that does not count

data.700.tail <- data.700[data.700$Ind_trial != 0,] #We remove the trial 0, so we give time to the person to get used to the experiment

data.700.valid <- data.700.tail[data.700.tail$Ind_trial %% 2 != 0,] #Select only odd trials, which correspond to the forward trials

data.700.invalid <- data.700.tail[data.700.tail$Ind_trial %% 2 == 0,] #Select only even trials, which correspond to the backwards trials

length <- 80
theta <- pi/4

#Coordinates of the ideal segments/paths
x1 <- c(5,5,5,5)
y1 = c(5,5,5,5)

x2 <- c(5+length*cos(theta*1),
        5+length*cos(theta*3),
        5+length*cos(theta*5),
        5+length*cos(theta*7))
y2 = c(5+length*sin(theta*1),
       5+length*sin(theta*3),
       5+length*sin(theta*5),
       5+length*sin(theta*7))

#Graph of the 
path_graph_valid <- ggplot(data = data.700.valid,
            aes(x = f_Pos_World_x-Start_Pos_World_x, y = f_Pos_World_y-Start_Pos_World_y),
            group = Ind_trial) + geom_path(colour = "black") + xlim(-100,100) + ylim(-100,100)

data.ideal.path <- data.frame(x1, x2, y1, y2)

#Display the graph and ideal path
path_graph_valid + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"),
                                data = data.ideal.path)


#Create a new csv with the relative distance run every trial
t           <- NULL
t$X         <- data.700.valid$f_Pos_World_x - data.700.valid$Start_Pos_World_x
t$Y         <- data.700.valid$f_Pos_World_y - data.700.valid$Start_Pos_World_y
t$Trial     <- data.700.valid$Ind_trial
t$Type      <- data.700.valid$Type_of_trayectory
t           <- as.data.frame(t)
write.csv(t, 'trial_scr.csv')

data.clean <- read.csv('trial_scr.csv') #Clean data that we will analyze
trials <- unique(data.clean$Trial)
num.trial <- length(trials) #Number of trials to be analyzed
target.pos <- as.matrix(data.ideal.path[,c(2,4)]) #Set a matrix with columns x2,y2

Measure <- vector(mode = 'numeric', length = num.trial+1)
Type.trial <- vector(mode = 'numeric', length = num.trial+1)
aux = 1
for (i in trials[1:(length(trials))]){
  trial <- data.clean[data.clean$Trial==i,] #Piece of the data of current trial
  P <- as.matrix(trial[,c(2,3)]) #Columns X and Y (relative run distances)
  Type <- as.numeric(trial$Type[1]) #Gets the type of that trial
  Type.trial[aux] <- Type
  rn <- nrow(trial)
  target.Matrix <- t(matrix(as.vector(rep(target.pos[Type+1,],rn)),
                            nrow = 2,
                            ncol = rn)) #That t is transpose
  dist2 <- P-target.Matrix
  M <- ((dist2[,1])^2 + (dist2[,2])^2)^0.5 #Cost function
  Measure[aux] <- sum(M)
  aux = aux + 1
}

Measure <- as.data.frame(Measure) #Set measure as table
Measure$Trial <- seq(1,length(unique(data.700$Ind_trial)),by=2) #Set absolute number of trials
Measure$Type <- as.factor(Type.trial)

useful.trial <- setdiff(1:num.trial, bad.trial)
useful.Measure <- Measure[useful.trial,]
l <- dim(useful.Measure)[1]
useful.Measure <- useful.Measure[2:l,]

dim(useful.Measure)

useful.Measure.100 <- useful.Measure[1:441,]


scatter_graph <- ggplot(data = useful.Measure.100, aes(x = Trial, y = Measure, color = Type)) +
  geom_point()

scatter_graph #Displays the graph

linear_regression <- ggplot() + geom_smooth(mapping = aes(x = Trial, y = Measure, color = Type), method="loess", data = useful.Measure.100) +
  geom_vline(xintercept = c(150,700))

linear_regression

combined_graphs <- scatter_graph + geom_smooth(mapping = aes(x = Trial, y = Measure, color = Type), method="loess", data = useful.Measure.100) +
  geom_vline(xintercept = c(150,700))

combined_graphs

boxplot_graph <- ggplot() + geom_boxplot(mapping = aes(x = Type, y = Measure, fill = Type), data = useful.Measure.100) +
  geom_vline(xintercept = c(150,700))

boxplot_graph

p<-ggplot(useful.Measure.100, aes(x=Measure, fill = Type)) + 
  geom_histogram(color="black") + geom_density()
p

ggplot(useful.Measure.100, aes(x=Measure, fill = "white")) + 
  geom_histogram(aes(y=..density..), colour="black")+
  geom_density(alpha=.2, fill="#FF6666") 

remove_outliers <- function(data_in, na.rm = TRUE) {
  qnt <- quantile(data_in[,1], probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(data_in[,1], na.rm = na.rm)
  y <- data_in[,1]
  y[data_in[,1] < (qnt[1] - H)] <- NA
  y[data_in[,1] > (qnt[2] + H)] <- NA
  y
}

y <- remove_outliers(useful.Measure.100)

useful.Measure.100$Measure <- y

scatter_graph_no_outliers <- ggplot(data = useful.Measure.100, aes(x = Trial, y = Measure, color = Type)) +
  geom_point()

scatter_graph_no_outliers #Displays the graph

linear_regression_no_outliers <- ggplot() + geom_smooth(mapping = aes(x = Trial, y = Measure, color = Type), method="loess", data = useful.Measure.100) +
  geom_vline(xintercept = c(150,700))

linear_regression_no_outliers

combined_graphs_no_outliers <- scatter_graph_no_outliers + geom_smooth(mapping = aes(x = Trial, y = Measure, color = Type), method="loess", data = useful.Measure.100) +
  geom_vline(xintercept = c(150,700))

combined_graphs_no_outliers

boxplot_graph_no_outliers <- ggplot() + geom_boxplot(mapping = aes(x = Type, y = Measure, fill = Type), data = useful.Measure.100) +
  geom_vline(xintercept = c(150,700))

boxplot_graph_no_outliers

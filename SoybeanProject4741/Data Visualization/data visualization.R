setwd("C:/Users/User/Dropbox/SoybeanProject4741/David_Exp")

dataset <- read.csv("../Student_Team_Competition_Syngenta_Data/EXPERIMENT DATA CSV (Cleaned).csv")
dim(dataset)
head(dataset)
#install.packages("dplyr")
#install.packages("gridExtra")
library(ggplot2)
library(dplyr)
library(gridExtra)

set.seed(1)
dataset %>% 
    group_by(FAMILY) %>%
    summarise(no_rows = length(FAMILY))

# Plots
ggplot(dataset, aes(x = dataset$RM, y = dataset$YIELD)) +
    geom_point(alpha = 0.3) + xlab("RM") + ylab("Yield")
ggplot(dataset, aes(dataset$RM, dataset$YIELD)) +
    stat_binhex() +
    xlab("RM") + ylab("Yield")

df <- subset(dataset, dataset$YEAR == "2009")
ggplot(df, aes(x = df$RM, y = df$YIELD)) +
    geom_point(alpha = 0.3)

ggplot(dataset, aes(x = dataset$YEAR, y = dataset$YIELD)) +
    geom_point(color = dataset$CLASS_OF)

# Overlay
dat <- dataset[which(dataset$YEAR %in% c("2009", "2014") & dataset$CLASS_OF == "2011"), ]
ggplot(dat, aes(x = dat$YIELD, fill = factor(dat$YEAR))) + 
    geom_histogram(binwidth = 1.0, alpha = 0.4) + 
    xlab("Yield") + ylab("Count") + labs(title = "Class of 2011") +
    guides(fill=guide_legend(title = "Year"))
    
dat2 <- dataset[which(dataset$YEAR %in% c("2009", "2014") & dataset$CLASS_OF == "2012"), ]
ggplot(dat2, aes(x = dat$YIELD, fill = factor(dat$YEAR))) + 
    geom_histogram(binwidth = 1.0, alpha = 0.4) + 
    xlab("Yield") + ylab("Count") + labs(title = "Class of 2012") +
    guides(fill = guide_legend(title = "Year")) 

dat3 <- dataset[which(dataset$YEAR %in% c("2009", "2014") & dataset$CLASS_OF == "2013"), ]
ggplot(dat3, aes(x = dat$YIELD, fill = factor(dat$YEAR))) + 
    geom_histogram(binwidth = 1.0, alpha = 0.4) + 
    xlab("Yield") + ylab("Count") + labs(title = "Class of 2012") +
    guides(fill = guide_legend(title = "Year"))


# Histograms of YIELD
df1 <- dataset[which(dataset$YEAR == "2009" & dataset$CLASS_OF == "2011"), ]
ggplot(df1, aes(df1$YIELD)) +
    geom_histogram(breaks = seq(36, 90, by = 2), col = "white", fill = "blue", alpha = 0.2) +
    xlab("Yield") + ylab("Count") + 
    scale_fill_gradient("Count")

df2 <- dataset[which(dataset$YEAR == "2014" & dataset$CLASS_OF == "2011"), ]
ggplot(df2, aes(df2$YIELD)) +
    geom_histogram(breaks = seq(36, 90, by = 2), col = "white", fill = "blue", alpha = 0.2) +
    xlab("Yield") + ylab("Count") +
    scale_fill_gradient("Count")

df3 <- dataset[which(dataset$YEAR == "2009" & dataset$CLASS_OF == "2012"), ]
ggplot(df3, aes(df3$YIELD)) +
    geom_histogram(breaks = seq(40, 80, by = 1), col = "white", aes(fill = ..count..)) +
    xlab("Yield") + ylab("Count") +
    scale_fill_gradient("Count")

df4 <- dataset[which(dataset$YEAR == "2014" & dataset$CLASS_OF == "2012"), ]
ggplot(df4, aes(df4$YIELD)) +
    geom_histogram(breaks = seq(26, 92, by = 2), col = "white", aes(fill = ..count..)) +
    xlab("Yield") + ylab("Count") +
    scale_fill_gradient("Count")

df5 <- dataset[which(dataset$YEAR == "2009" & dataset$CLASS_OF == "2013"), ]
ggplot(df5, aes(df5$YIELD)) +
    geom_histogram(breaks = seq(48, 84, by = 1), col = "white", aes(fill = ..count..)) +
    xlab("Yield") + ylab("Count") +
    scale_fill_gradient("Count")

df6 <- dataset[which(dataset$YEAR == "2014" & dataset$CLASS_OF == "2013"), ]
ggplot(df6, aes(df6$YIELD)) +
    geom_histogram(breaks = seq(22, 92, by = 2), col = "white", aes(fill = ..count..)) +
    xlab("Yield") + ylab("Count") +
    scale_fill_gradient("Count")


df7 <- dataset[which(dataset$BAGSOLD > 0), ]
ggplot(df7, aes(x = df7$BAGSOLD, y = df7$YIELD)) +
    geom_point(aes(color = df7$RM))

# Boxplot
ggplot(dataset, aes(x = factor(dataset$YEAR), y = dataset$YIELD)) +
    geom_boxplot(alpha = 0.2) +
    xlab("Year") + ylab("Yield")

# Other
df8 <- dataset[which(dataset$GRAD == "1"), ]
p1 <- ggplot(df8, aes(df8$YIELD)) +
        geom_histogram(breaks = seq(10, 104, by = 2), col = "white", fill = "blue", alpha = 0.2) +
        xlab("Yield") + ylab("Count") + labs(title = "Graduated Class") + 
        scale_fill_gradient("Count") 
p1
df9 <- dataset[which(dataset$GRAD == "0"), ]
p2 <- ggplot(df9, aes(df9$YIELD)) +
        geom_histogram(breaks = seq(12, 106, by = 2), col = "white", fill = "red", alpha = 0.2) +
        xlab("Yield") + ylab("Count") + labs(title = "Class Not Graduated") + 
        scale_fill_gradient("Count") 

grid.arrange(p1, p2, ncol = 2)

newdf <- dataset[which(dataset$CLASS_OF < "2014"), ]
newdf1 <- dataset[which(dataset$BAGSOLD > 0), ] 
ggplot(newdf1, aes(x = factor(newdf1$CLASS_OF), y = newdf1$BAGSOLD)) + 
    geom_boxplot() + xlab("Class") + ylab("Bags Sold") + labs(title = "Boxplot") 


# Used for Mid Term Report and/or Final Report
# Training Set: 2009
#traindata <- read.csv("../Split_Data/training[1].csv")

# Training Set: training[5]
traindata<- read.csv("../Split_Data2/training[5].csv")
ggplot(traindata, aes(x = traindata$RM, y = traindata$YIELD, color = factor(traindata$GRAD))) +
    geom_point()

df1 <- traindata[which(traindata$GRAD > "-1"), ]
df1$GRAD <- factor(df1$GRAD)
attach(df1)
p1 <- ggplot(df1, aes(x = RM, y = YIELD, color = GRAD)) +
        geom_point() + xlab("RM") + ylab("Yield") +
        scale_fill_gradient("GRAD") + labs(title = "Yield vs. RM")
p1
p2 <- ggplot(traindata, aes(traindata$YIELD)) +
        geom_histogram(breaks = seq(8, 104, by = 2), col = "white", fill = "blue", alpha = 0.2) +
        xlab("Yield") + ylab("Count") + labs(title = "Histogram of Yield")
        scale_fill_gradient("Count") 
p2
grid.arrange(p1, p2, ncol = 2)
        
p3 <- ggplot(traindata, aes(x = traindata$CHECK, y = traindata$YIELD)) + 
        geom_boxplot() + xlab("Check") + ylab("Yield") + labs(title = "Boxplot")
p3
p4 <- ggplot(traindata, aes(x = factor(traindata$GRAD), y = traindata$YIELD)) + 
        geom_boxplot() + xlab("Grad") + ylab("Yield") + labs(title = "Boxplot")
p4
grid.arrange(p3, p4, ncol = 2)

table <- xtabs(~ traindata$CHECK+ traindata$GRAD, traindata) # Frequency table
table

p5 <- ggplot(traindata, aes(traindata$GRAD, fill=traindata$CHECK)) + 
geom_bar(position="dodge") + xlab("Grad") + ylab("Count")+ scale_fill_discrete(name="Check", labels = c("False", "True"))
p5

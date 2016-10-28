#install.packages("ggplot2")
#install.packages("rtf")

library(ggplot2)
library(lattice)
library(rtf)

data <- read.csv("../Student_Team_Competition_Syngenta_Data/EXPERIMENT DATA CSV (Cleaned).csv")
str(data)
Freq_vs_Yield = hist(data$RM, breaks=12, col="blue")
hist(data$YIELD, breaks=12, col="blue")
hist(data$BAGSOLD, breaks=12, col="blue")



ggplot(data, aes(YEAR, YIELD)) + geom_point()

ggplot(data, aes(YEAR, YIELD)) + geom_boxplot(aes(group=YEAR))
ggplot(data, aes(RM, YIELD)) + geom_boxplot(aes(group=YEAR))
ggplot(data, aes(BAGSOLD, YIELD)) + geom_boxplot(aes(group=YEAR))


data$CLASS_FACTOR <- factor(data$CLASS_OF)
str(data)
#ggplot(data, aes(YEAR, YIELD)) + geom_point(colour = data$FAMILY)

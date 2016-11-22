library(mlbench)
library(e1071)
library(rpart)
library(caret)

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(ggplot2)


install.packages('ggplot2', dep = TRUE)

setwd("C:/Users/User/Dropbox/SoybeanProject4741/Clara_Exp")
myvars <- c("YEAR", "CHECK", "RM", "YIELD", "FAM_SEGMENT1", "FAM_SEGMENT2", "FAM_SEGMENT3", "FAM_SEGMENT4", "FAM_SEGMENT5", "CLASS_OF", "GRAD")

train1 <- read.csv("../Split_Data/training[1].csv")
train1 <- train1[myvars]
train1$YEAR <- as.factor(train1$YEAR)
train1$GRAD <- as.factor(train1$GRAD)
train1$CLASS_OF <- as.factor(train1$CLASS_OF)

train2 <- read.csv("../Split_Data/training[2].csv")
train2 <- train2[myvars]
train2$YEAR <- as.factor(train2$YEAR)
train2$GRAD <- as.factor(train2$GRAD)
train2$CLASS_OF <- as.factor(train2$CLASS_OF)

train3 <- read.csv("../Split_Data/training[3].csv")
train3 <- train3[myvars]
train3$YEAR <- as.factor(train3$YEAR)
train3$GRAD <- as.factor(train3$GRAD)
train3$CLASS_OF <- as.factor(train3$CLASS_OF)

train4 <- read.csv("../Split_Data/training[4].csv")
train4 <- train4[myvars]
train4$YEAR <- as.factor(train4$YEAR)
train4$GRAD <- as.factor(train4$GRAD)
train4$CLASS_OF <- as.factor(train4$CLASS_OF)

train5 <- read.csv("../Split_Data/training[5].csv")
train5 <- train5[myvars]
train5$YEAR <- as.factor(train5$YEAR)
train5$GRAD <- as.factor(train5$GRAD)
train5$CLASS_OF <- as.factor(train5$CLASS_OF)




# test sets # 
test1 <-read.csv("../Split_Data/test_2010.csv")
test1 <- test1[myvars]
test1$YEAR <- as.factor(test1$YEAR)
test1$CLASS_OF <- as.factor(test1$CLASS_OF)
test1$GRAD <- as.factor(test1$GRAD)


test2 <-read.csv("../Split_Data/test_2011.csv")
test2 <- test2[myvars]
test2$YEAR <- as.factor(test2$YEAR)
test2$CLASS_OF <- as.factor(test2$CLASS_OF)
test2$GRAD <- as.factor(test2$GRAD)



test3 <-read.csv("../Split_Data/test_2012.csv")
test3 <- test3[myvars]
test3$YEAR <- as.factor(test3$YEAR)
test3$CLASS_OF <- as.factor(test3$CLASS_OF)
test3$GRAD <- as.factor(test3$GRAD)



test4 <-read.csv("../Split_Data/test_2013.csv")
test4 <- test4[myvars]
test4$YEAR <- as.factor(test4$YEAR)
test4$CLASS_OF <- as.factor(test4$CLASS_OF)
test4$GRAD <- as.factor(test4$GRAD)



test5 <-read.csv("../Split_Data/test_2014.csv")
test5 <- test5[myvars]
test5$YEAR <- as.factor(test5$YEAR)
test5$CLASS_OF <- as.factor(test5$CLASS_OF)
test5$GRAD <- as.factor(test5$GRAD)


library(lattice)
barchart(test5$~Reason,data=Reasonstats,groups=Catergory, 
         scales=list(x=list(rot=90,cex=0.8)))


ggplot(test5, aes(test5$GRAD, Species, fill = Category)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")



counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)



######


nb_model <- naiveBayes(train1$GRAD~., data = train1)
pred <- predict(nb_model, test1)
new <- cbind (test1, pred)
write.csv(new, file = "naivebayes1.csv")
confusion1 <- confusionMatrix(pred, reference = test1$GRAD, positive='yes')

nb_model <- naiveBayes(train2$GRAD~., data = train2)
pred <- predict(nb_model, test2)
new <- cbind (test2, pred)
write.csv(new, file = "naivebayes2.csv")
confusion2 <- confusionMatrix(pred, reference = test2$GRAD, positive='yes')

nb_model <- naiveBayes(train3$GRAD~., data = train3)
pred <- predict(nb_model, test3)
new <- cbind (test3, pred)
write.csv(new, file = "naivebayes3.csv")
confusion3 <- confusionMatrix(pred, reference = test3$GRAD, positive='yes')


nb_model <- naiveBayes(train4$GRAD~., data = train4)
pred <- predict(nb_model, test4)
new <- cbind (test4, pred)
write.csv(new, file = "naivebayes4.csv")
confusion4 <- confusionMatrix(pred, reference = test4$GRAD, positive='yes')


nb_model <- naiveBayes(train5$GRAD~., data = train5)
pred <- predict(nb_model, test5)
new <- cbind (test5, pred)
write.csv(new, file = "naivebayes5.csv")
confusion5 <-confusionMatrix(pred, reference = test5$GRAD, positive='yes')


confusion1
confusion2
confusion3
confusion4
confusion5


precision <- confusion1$byClass['Pos Pred Value']    
recall <- confusion1$byClass['Sensitivity']
f_measure1 <- 2 * ((precision * recall) / (precision + recall))

f_measure1



#K MEANS# 


set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster
table(irisCluster$cluster, iris$Species)


train1 <- read.csv("../Split_Data/training[1].csv")

# select variables v1, v2, v3

myvars <- c("FAMILY", "CHECK", "RM_GRP", "YIELD", "CLASS_OF", "GRAD")
train1 <- train1[myvars]


train1$CHECK <- as.numeric(train1$CHECK)
train1$RM_GRP <- as.numeric(train1$RM_GRP)
train1$YIELD <- as.numeric(train1$YIELD)
set.seed(12)
soybeanCluster <- kmeans(train1[,c("CHECK", "YIELD", "CLASS_OF")], 4, nstart = 20)
soybeanCluster
table(soybeanCluster$cluster, train1$RM_GRP)

soybeanCluster$cluster <- as.factor(soybeanCluster$cluster)
ggplot(iris, aes(train1$RM_GRP, train1$YIELD, color = train1$GRAD)) + geom_point()

nrow(train1)
nrow(soybeanCluster)
soybeanCluster$cluster

#########################################
ml <- read.dta("http://www.ats.ucla.edu/stat/data/hsbdemo.dta")
with(ml, table(ses, prog))

with(ml, do.call(rbind, tapply(write, prog,
                               function(x) c(M = mean(x), SD = sd(x)))))
ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ ses + write, data = ml)

summary(test)
unique(train1$LOCATION)

#####################################################

data(HouseVotes84)
model <- naiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,-1])
predict(model, HouseVotes84[1:10,-1], type = "raw")

pred <- predict(model, HouseVotes84[,-1])
table(pred, HouseVotes84$Class)
pred

data(Titanic)
m <- naiveBayes(Survived ~ ., data = Titanic)
m
predict(m, as.data.frame(Titanic)[,1:3]) # Leave out survived and frequency



data(iris)
m <- naiveBayes(Species ~ ., data = iris)
## alternatively:
m
table(predict(m, iris[,-5]), iris[,5])

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()



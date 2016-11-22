library(mlbench)
library(e1071)
library(rpart)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(ggplot2)
install.packages('ggplot2', dep = TRUE)

setwd("C:/Users/User/Dropbox/SoybeanProject4741/Clara_Exp")
train1 <- read.csv("../Split_Data/training[1].csv")

# select variables v1, v2, v3

myvars <- c("CHECK", "RM_GRP", "YIELD", "CLASS_OF", "GRAD")
train1 <- train1[myvars]

#train1$YEAR <- as.factor(train1$YEAR)
#train1$EXPERIMENT <- as.factor(train1$EXPERIMENT)
#train1$RM <- as.factor(train1$RM)

#train1$LOCATION <- as.factor(train1$LOCATION)
train1$RM_GRP <- as.factor(train1$RM_GRP)
train1$YIELD_GRP <- as.factor(train1$YIELD_GRP)

train1$GRAD <- as.factor(train1$GRAD)
train1$CLASS_OF <- as.factor(train1$CLASS_OF)


test1 <-read.csv("../Split_Data/test_2010.csv")
test1 <- test1[myvars]
test1 <- test1[,-1]

#test1$LOCATION <- as.factor(test1$LOCATION)
train1$RM_GRP <- as.factor(train1$RM_GRP)
test1$CLASS_OF <- as.factor(test1$CLASS_OF)
test1$GRAD <- as.factor(test1$GRAD)




train1$GRAD <- as.numeric(train1$GRAD)
train1$GRAD <- as.factor(train1$GRAD)
ggplot(train1, aes(train1$RM_GRP, train1$YIELD, color = train1$FAMILY))+geom_point()


str(train1)
ggplot(train1, aes(train1$RM_GRP, train1$YIELD, color = train1$EXPRIMENT))+geom_point()




######


nb_model <- naiveBayes(train1$GRAD~., data = train1)



pred <- predict(nb_model, test1)

new <- cbind (test1, pred)
write.csv(new, file = "naivebayes.csv")


confusion <- table (pred, test1$GRAD)

########################################



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



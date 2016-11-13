library(mlbench)
library(e1071)
library(rpart)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(ggplot2)
install.packages('ggplot2', dep = TRUE)

train1 <- read.csv("../Split_Data/training[1].csv")

# select variables v1, v2, v3

myvars <- c("FAMILY", "CHECK", "RM_GRP", "YIELD", "CLASS_OF", "GRAD")
train1 <- train1[myvars]

#train1$YEAR <- as.factor(train1$YEAR)
#train1$EXPERIMENT <- as.factor(train1$EXPERIMENT)
#train1$RM <- as.factor(train1$RM)

#train1$LOCATION <- as.factor(train1$LOCATION)
train1$RM_GRP <- as.factor(train1$RM_GRP)
train1$GRAD <- as.factor(train1$GRAD)
train1$CLASS_OF <- as.factor(train1$CLASS_OF)


test1 <-read.csv("../Split_Data/test_2010.csv")
test1 <- test1[myvars]
test1 <- test1[,-1]

#test1$LOCATION <- as.factor(test1$LOCATION)
train1$RM_GRP <- as.factor(train1$RM_GRP)
test1$CLASS_OF <- as.factor(test1$CLASS_OF)
test1$GRAD <- as.factor(test1$GRAD)

######

prop.table(table(train1$GRAD))
prop.table(table(test1$GRAD))

nb_model <- naiveBayes(train1$GRAD~., data = train1)



pred <- predict(nb_model, test1)


nrow(pred)
new <- rbind (test1, pred)
write.csv(pred, file = "naivebayes.csv")
str(new)
str(df)
str(pred)



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

train1$GRAD <- as.numeric(train1$GRAD)
train1$GRAD <- as.factor(train1$GRAD)
ggplot(train1, aes(train1$GRAD, train1$YIELD, color = train1$RM_GRP))+geom_point()


str(train1)
ggplot(train1, aes(train1$RM_GRP, train1$YIELD, color = train1$EXPRIMENT))+geom_point()


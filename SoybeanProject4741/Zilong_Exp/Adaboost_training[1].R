#install.packages("mlr");
#library(mlr);
#library(mlbench);
#library(e1071);
#library(rpart);
#require(foreign);
#require(nnet);
#require(ggplot2);
#require(reshape2);

library(rpart);
library(adabag);
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)


training = read.table("../Split_Data/training[1].csv",sep=",",header=TRUE);
testing = read.table("../Split_Data/test_2010.csv",sep=",",header=TRUE);

training$GRAD <- as.factor(training$GRAD);
testing$GRAD <- as.factor(testing$GRAD);

training.adaboost <- boosting( GRAD ~.,data=training,boos=TRUE,mfinal=2);
importanceplot(training.adaboost);
save.image(file = "training_done.RData");
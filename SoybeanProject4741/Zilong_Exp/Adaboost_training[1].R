#install.packages("mlr");
#library(mlr);
#library(mlbench);
#library(e1071);
#library(rpart);
#require(foreign);
#require(nnet);
#require(ggplot2);
#require(reshape2);
#install.packages("fastAdaboost");
library(nnet);
library(e1071);
library(caret);
library(fastAdaboost);
library(rpart);
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)


training_ = read.table("../Split_Data/training[1].csv",sep=",",header=TRUE);
testing_ = read.table("../Split_Data/test_2010.csv",sep=",",header=TRUE);


#Cut out YEAR, EXPERIMENT, LOCATION, VARIETY, FAMILY, RM, YIELD

#Use 
#X: CHECK, RM, YIELD, Family segments, CLASS_OF
#y: GRAD as a factor

training <- subset( training_,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ) );
testing <- subset( testing_,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ));

training = training[training$GRAD != -1,];
training$GRAD <- factor(training$GRAD); 

testing = testing[testing$GRAD != -1,];
testing$GRAD <- factor(testing$GRAD); 

test_adaboost <- adaboost(GRAD~.,data=training,10);
pred <- predict(test_adaboost,newdata = testing);

#Test error
#print( paste("error is", toString(pred$error) )  );

# Confusion matrix
#print( table(pred$class,testing$GRAD) );

confusion1 <- confusionMatrix(pred$class,reference = testing$GRAD);

# Length of outcomes
l = length(pred$class);

# Number correct
correctNum = sum(pred$class == testing$GRAD);

# Percentage correct
pCorrect = correctNum/l;

#print( paste("Proportion correct is",toString(pCorrect))    );

print(confusion1)
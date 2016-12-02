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


training_1 = read.table("../Split_Data2/training[1].csv",sep=",",header=TRUE);
testing_1 = read.table("../Split_Data2/test_2010.csv",sep=",",header=TRUE);


#Cut out YEAR, EXPERIMENT, LOCATION, VARIETY, FAMILY, RM, YIELD

#Use 
#X: CHECK, RM, YIELD, Family segments, CLASS_OF
#y: GRAD as a factor

training1 <- subset( training_1,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ) );
testing1 <- subset( testing_1,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ));

training1 = training1[training1$GRAD != -1,];
training1$GRAD <- factor(training1$GRAD); 

testing1 = testing1[testing1$GRAD != -1,];
testing1$GRAD <- factor(testing1$GRAD); 

test_adaboost1 <- adaboost(GRAD~.,data=training1,10);
pred1 <- predict(test_adaboost1,newdata = testing1);

# Confusion Matrix
confusion1 <- confusionMatrix(pred1$class,reference = testing1$GRAD);

# print(confusion1)

#####################################2#########################################

training_2 = read.table("../Split_Data2/training[2].csv",sep=",",header=TRUE);
testing_2 = read.table("../Split_Data2/test_2011.csv",sep=",",header=TRUE);

training2 <- subset( training_2,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ) );
testing2 <- subset( testing_2,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ));

training2 = training2[training2$GRAD != -1,];
training2$GRAD <- factor(training2$GRAD); 

testing2 = testing1[testing2$GRAD != -1,];
testing2$GRAD <- factor(testing2$GRAD); 

test_adaboost2 <- adaboost(GRAD~.,data=training2,10);
pred2 <- predict(test_adaboost2,newdata = testing2);

# Confusion Matrix
confusion2 <- confusionMatrix(pred2$class,reference = testing2$GRAD);

#####################################3###########################################
# Starts becoming bad
training_3 = read.table("../Split_Data2/training[3].csv",sep=",",header=TRUE);
testing_3 = read.table("../Split_Data2/test_2012.csv",sep=",",header=TRUE);

training3 <- subset( training_3,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ) );
testing3 <- subset( testing_3,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ));

training3 = training3[training3$GRAD != -1,];
training3$GRAD <- factor(training3$GRAD); 

testing3 = testing3[testing3$GRAD != -1,];
testing3$GRAD <- factor(testing3$GRAD); 

test_adaboost3 <- adaboost(GRAD~.,data=training3,10);
pred3 <- predict(test_adaboost3,newdata = testing3);

# Confusion Matrix
confusion3 <- confusionMatrix(pred3$class,reference = testing3$GRAD);

#####################################4###########################################
# Starts becoming bad
training_4 = read.table("../Split_Data2/training[4].csv",sep=",",header=TRUE);
testing_4 = read.table("../Split_Data2/test_2013.csv",sep=",",header=TRUE);

training4 <- subset( training_4,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ) );
testing4 <- subset( testing_4,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ));

training4 = training4[training4$GRAD != -1,];
training4$GRAD <- factor(training4$GRAD); 

testing4 = testing4[testing4$GRAD != -1,];
testing4$GRAD <- factor(testing4$GRAD); 

test_adaboost4 <- adaboost(GRAD~.,data=training4,10);
pred4 <- predict(test_adaboost4,newdata = testing4);

# Confusion Matrix
confusion4 <- confusionMatrix(pred4$class,reference = testing4$GRAD);

#####################################5###########################################
# Starts becoming bad
training_5 = read.table("../Split_Data2/training[5].csv",sep=",",header=TRUE);
testing_5 = read.table("../Split_Data2/test_2014.csv",sep=",",header=TRUE);

training5 <- subset( training_5,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ) );
testing5 <- subset( testing_5,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ));

training5 = training5[training5$GRAD != -1,];
training5$GRAD <- factor(training5$GRAD); 

testing5 = testing5[testing5$GRAD != -1,];
testing5$GRAD <- factor(testing5$GRAD); 

test_adaboost5 <- adaboost(GRAD~.,data=training5,10);
pred5 <- predict(test_adaboost5,newdata = testing5);

# Confusion Matrix
confusion5 <- confusionMatrix(pred5$class,reference = testing5$GRAD);


############### PRECISION AND RECALL ##############################################

PRmat <- function(cmatrix,n){
  arr = array(cmatrix$table);
  precision0 = arr[1]/(arr[1]+arr[3]);
  precision1 = arr[4]/(arr[2]+arr[4]);
  recall1 = arr[1]/(arr[1]+arr[2]);
  recall2 = arr[4]/(arr[3]+arr[4]); 
  
  k=paste("for",toString(n));
  a=paste("precision0",toString(precision0));
  b=paste("precision1",toString(precision1));
  c=paste("recall1",toString(recall1));
  d=paste("recall2",toString(recall2));
  
  print(k);
  print(a);print(b);print(c);print(d);print("");
  
  return( c(precision0,precision1,recall1,recall2) )
}

arr1 = array(confusion1$table);
arr2 = array(confusion2$table);
arr3 = array(confusion3$table);
arr4 = array(confusion4$table);
arr5 = array(confusion5$table);

results1 = PRmat(confusion1,1);
results2 = PRmat(confusion2,2);
results3 = PRmat(confusion3,3);
results4 = PRmat(confusion4,4);
results5 = PRmat(confusion5,5);



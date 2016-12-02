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


PRmat <- function(cmatrix,n){
  arr = array(cmatrix$table);
  precision0 = arr[1]/(arr[1]+arr[3]);
  precision1 = arr[4]/(arr[2]+arr[4]);
  recall1 = arr[1]/(arr[1]+arr[2]);
  recall2 = arr[4]/(arr[3]+arr[4]); 
  
  avgPrec = 0.5*(precision0 + precision1);
  avgRec = 0.5*(recall1 + recall2);
  IndividualFScore = 2/(1/avgPrec + 1/avgRec);
  
  k=paste("for",toString(n));
  a=paste("precision0",toString(precision0));
  b=paste("precision1",toString(precision1));
  c=paste("recall1",toString(recall1));
  d=paste("recall2",toString(recall2));
  e=paste("F1 Score",toString(IndividualFScore));
  
  print(k);
  print(a);print(b);print(c);print(d);print(e);print("");
  
  return( c(precision0,precision1,recall1,recall2,IndividualFScore) );
}


############### PRECISION AND RECALL TRAINING ##########################################
# CHECK P,R, F for training set
check1 = predict(test_adaboost1,newdata=training1);
check2 = predict(test_adaboost2,newdata=training2);
check3 = predict(test_adaboost3,newdata=training3);
check4 = predict(test_adaboost4,newdata=training4);
check5 = predict(test_adaboost5,newdata=training5);

trainConf1 <- confusionMatrix(check1$class,reference = training1$GRAD);
trainConf2 <- confusionMatrix(check2$class,reference = training2$GRAD);
trainConf3 <- confusionMatrix(check3$class,reference = training3$GRAD);
trainConf4 <- confusionMatrix(check4$class,reference = training4$GRAD);
trainConf5 <- confusionMatrix(check5$class,reference = training5$GRAD);

trainresults1 = PRmat(trainConf1,1);
trainresults2 = PRmat(trainConf2,2);
trainresults3 = PRmat(trainConf3,3);
trainresults4 = PRmat(trainConf4,4);
trainresults5 = PRmat(trainConf5,5);

globalTrainPrecision = trainresults1[1]+trainresults1[2]+trainresults2[1]+trainresults2[2]+trainresults3[1]+trainresults3[2]+trainresults4[1]+trainresults4[2]+trainresults5[1]+trainresults5[2];
globalTrainRecall = trainresults1[3]+trainresults1[4]+trainresults2[3]+trainresults2[4]+trainresults3[3]+trainresults3[4]+trainresults4[3]+trainresults4[4]+trainresults5[3]+trainresults5[4];

avgGlobalTrainPrecision = globalTrainPrecision/10;
avgGlobalTrainRecall = globalTrainRecall/10;
trainGlobalF = 2/(1/avgGlobalTrainRecall+1/avgGlobalTrainPrecision);
print(paste("Training Global F1 Score is",toString(trainGlobalF)));


############### PRECISION AND RECALL TEST ##############################################



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

globalPrecision = results1[1]+results1[2]+results2[1]+results2[2]+results3[1]+results3[2]+results4[1]+results4[2]+results5[1]+results5[2];
globalRecall = results1[3]+results1[4]+results2[3]+results2[4]+results3[3]+results3[4]+results4[3]+results4[4]+results5[3]+results5[4];

avgPrecision = globalPrecision/10;
avgRecall = globalRecall/10;

globalF = 2/(1/avgPrecision+1/avgRecall);
print(paste("Global F1 Score is",toString(globalF)));

############## CHOOSE THE BEST ###########################################################

modelchoices = array(c(test_adaboost1,test_adaboost2,test_adaboost3,test_adaboost4,test_adaboost5));
resultsarr = array(c(results1,results2,results3,results4,results5));
best = 1;
maxF = resultsarr[5] 
for (i in 2:5){
  if (maxF<resultsarr[i*5]){
    maxF = resultsarr[i*5];
    best = i;
  }
  else{
    ;
  }
}
print(paste("Best model is",toString(best)));
bestmodel = modelchoices[best];
############## OBTAIN THE PREDICTION SET ###############################################

predictionData = read.table("../Split_Data2/prediction_set.csv",sep=",",header=TRUE);
predictionBackup = predictionData;
predictionData <- subset( predictionData,select=c("YEAR","CHECK","RM", "YIELD","FAM_SEGMENT1","FAM_SEGMENT2","FAM_SEGMENT3","FAM_SEGMENT4","FAM_SEGMENT5","CLASS_OF", "GRAD" ) );

predictionData$GRAD <- factor(predictionData$GRAD); 

predPRED <- predict(bestmodel,newdata = predictionData);

new <- cbind(predictionBackup,predPRED);
write.csv(new, file = "adaboost.csv");


#install.packages("mlr");
library(mlr);

training = read.table("../Split_Data/training[1].csv",sep=",",header=TRUE);
testing = read.table("../Split_Data/test_2010.csv",sep=",",header=TRUE);

train_sub = subset(training,select = -c(EXPERIMENT,LOCATION,YEAR,FAMILY,VARIETY,RM));
test_sub = subset(testing, select = -c(EXPERIMENT,LOCATION,YEAR,FAMILY,VARIETY,RM));

classif.task = makeClassifTask(id = "training[1]",data = train_sub, target = "GRAD");
#classif.lrn = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE);

#mod = train(classif.lrn,classif.task);
#getLearnerModel(mod);

#newdata.predict = predict(mod, newdata=test_sub);
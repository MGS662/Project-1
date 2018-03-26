
#import the data
train<-read.csv('/Users/ranaa/Box Sync/spring 2018 (arpitran@buffalo.edu)/Machine Learning for IT managers/BlogFeedback/blogData_train.csv',header=FALSE,sep=",",)
testfeb1<-read.csv('/Users/ranaa/Box Sync/spring 2018 (arpitran@buffalo.edu)/Machine Learning for IT managers/BlogFeedback/blogData_test-2012.02.01.00_00.csv',header=FALSE,sep=",",)
testfeb2<-read.csv('/Users/ranaa/Box Sync/spring 2018 (arpitran@buffalo.edu)/Machine Learning for IT managers/BlogFeedback/blogData_test-2012.02.02.00_00.csv',header=FALSE,sep=",",)
testmar1<-read.csv('/Users/ranaa/Box Sync/spring 2018 (arpitran@buffalo.edu)/Machine Learning for IT managers/BlogFeedback/blogData_test-2012.03.01.00_00.csv',header=FALSE,sep=",",)
testmar2<-read.csv('/Users/ranaa/Box Sync/spring 2018 (arpitran@buffalo.edu)/Machine Learning for IT managers/BlogFeedback/blogData_test-2012.03.02.00_00.csv',header=FALSE,sep=",",)
#view the data
summary(train)
summary(testfeb1)
summary(testfeb2)
sumamry(testmar1)
summary(testmar2)
basic_feature<-train[,51:60]
basic_feature
target_feature<-train["V281"]
target_feature
textutual_feature<-train[,63:262]
train1<-data.frame(basic_feature,target_feature)
train2<-data.frame(textutual_feature,target_feature)

#preprocessing the dataset for linear regression
exp1.testf1<-testfeb1[,51:60]
exp1.testf2<-testfeb2[,51:60]
exp1.testm1<-testmar1[,51:60]
exp1.testm2<-testmar2[,51:60]
exp2.testf1<-testfeb1[,63:262]
exp2.testf2<-testfeb2[,63:262]
exp2.testm1<-testmar1[,63:262]
exp2.testm2<-testmar2[,63:262]
#preprocessing the dataset for Logistic regression
log.train<-ifelse(train$V281>0,1,0)
train.log1<-train1
train.log1$V281<-log.train
train.log2<-train2
train.log2$V281<-log.train

#preprocessing the dataset for test sets for logistic regression

log.testfeb1<-ifelse(testfeb1$V281>0,1,0)
test.log.feb1<-testfeb1
test.log.feb1$V281<-log.testfeb1

log.testfeb2<-ifelse(testfeb2$V281>0,1,0)
test.log.feb2<-testfeb2
test.log.feb2$V281<-log.testfeb2

log.testmar1<-ifelse(testmar1$V281>0,1,0)
test.log.mar1<-testmar1
test.log.mar1$V281<-log.testmar1

log.testmar2<-ifelse(testmar2$V281>0,1,0)
test.log.mar2<-testmar2
test.log.mar2$V281<-log.testmar2


#fitting the linear regression model for exp1 and exp 2 with categorical values
exp1.lmfitcat.train1<-lm(formula=V281~.,data=train.log1)
exp2.lmfitcat.train2<-lm(formula=V281~.,data=train.log2)
summary(exp1.lmfitcat.train1)
summary(exp2.lmfitcat.train2)

#fitting the Linear Regression model for exp1 and exp2 without categorical values
exp1.lmfit.train1<-lm(formula=V281~.,data=train1)
exp2.lmfit.train2<-lm(formula=V281~.,data=train2)
summary(exp1.lmfit.train1)
summary(exp2.lmfit.train2)

#fitting the logistic regression model for exp1 and exp2
exp1.glmfit.train1<-glm(formula=V281~.,data=train.log1,family=binomial(link=logit))
exp2.glmfit.train2<-glm(formula=V281~.,data=train.log2,family=binomial(link=logit))
summary(exp1.glmfit.train1)
summary(exp2.glmfit.train2)

#prediction of linear regression for exp1 and exp2 with categorical values
exp1.lmcat.predfeb1<-predict(exp1.lmfitcat.train1,exp1.testf1,se.fit=TRUE)
exp1.lmcat.predfeb2<-predict(exp1.lmfitcat.train1,exp1.testf2,se.fit=TRUE)
exp1.lmcat.predmar1<-predict(exp1.lmfitcat.train1,exp1.testm1,se.fit=TRUE)
exp1.lmcat.predmar2<-predict(exp1.lmfitcat.train1,exp1.testm2,se.fit=TRUE)
exp2.lmcat.predfeb1<-predict(exp2.lmfitcat.train2,exp2.testf1,se.fit=TRUE)
exp2.lmcat.predfeb2<-predict(exp2.lmfitcat.train2,exp2.testf2,se.fit=TRUE)
exp2.lmcat.predmar1<-predict(exp2.lmfitcat.train2,exp2.testm1,se.fit=TRUE)
exp2.lmcat.predmar2<-predict(exp2.lmfitcat.train2,exp2.testm2,se.fit=TRUE)

#prediction of linear regression for exp1 and exp2 without categorical values

exp1.lm.predfeb1<-predict(exp1.lmfit.train1,exp1.testf1,se.fit=TRUE)
exp1.lm.predfeb2<-predict(exp1.lmfit.train1,exp1.testf2,se.fit=TRUE)
exp1.lm.predmar1<-predict(exp1.lmfit.train1,exp1.testm1,se.fit=TRUE)
exp1.lm.predmar2<-predict(exp1.lmfit.train1,exp1.testm2,se.fit=TRUE)
exp2.lm.predfeb1<-predict(exp2.lmfit.train2,exp2.testf1,se.fit=TRUE)
exp2.lm.predfeb2<-predict(exp2.lmfit.train2,exp2.testf2,se.fit=TRUE)
exp2.lm.predmar1<-predict(exp2.lmfit.train2,exp2.testm1,se.fit=TRUE)
exp2.lm.predmar2<-predict(exp2.lmfit.train2,exp2.testm2,se.fit=TRUE)

#prediciton for logistic regression
exp1.glm.predfeb1<-predict(exp1.glmfit.train1,exp1.testf1,se.fit=TRUE)
exp1.glm.predfeb2<-predict(exp1.glmfit.train1,exp1.testf2,se.fit=TRUE)
exp1.glm.predmar1<-predict(exp1.glmfit.train1,exp1.testm1,se.fit=TRUE)
exp1.glm.predmar2<-predict(exp1.glmfit.train1,exp1.testm2,se.fit=TRUE)
exp2.glm.predfeb1<-predict(exp2.glmfit.train2,exp2.testf1,se.fit=TRUE)
exp2.glm.predfeb2<-predict(exp2.glmfit.train2,exp2.testf2,se.fit=TRUE)
exp2.glm.predmar1<-predict(exp2.glmfit.train2,exp2.testm1,se.fit=TRUE)
exp2.glm.predmar2<-predict(exp2.glmfit.train2,exp2.testm2,se.fit=TRUE)

#rss values calculated for linear regression model with categorical values
rss.exp1.lm_feb1cat=sum((test.log.feb1$V281-exp1.lmcat.predfeb1$fit)^2)
rss.exp1.lm_feb2cat=sum((test.log.feb2$V281-exp1.lmcat.predfeb2$fit)^2)
rss.exp1.lm_mar1cat=sum((test.log.mar1$V281-exp1.lmcat.predmar1$fit)^2)
rss.exp1.lm_mar2cat=sum((test.log.mar2$V281-exp1.lmcat.predmar2$fit)^2)
rss.exp2.lm_feb1cat=sum((test.log.feb1$V281-exp2.lmcat.predfeb1$fit)^2)
rss.exp2.lm_feb2cat=sum((test.log.feb2$V281-exp2.lmcat.predfeb2$fit)^2)
rss.exp2.lm_mar1cat=sum((test.log.mar1$V281-exp2.lmcat.predmar1$fit)^2)
rss.exp2.lm_mar2cat=sum((test.log.mar2$V281-exp2.lmcat.predmar2$fit)^2)

#rss values calculated for linear regression model
rss.exp1.lm_feb1=sum((testfeb1$V281-exp1.lm.predfeb1$fit)^2)
rss.exp1.lm_feb2=sum((testfeb2$V281-exp1.lm.predfeb2$fit)^2)
rss.exp1.lm_mar1=sum((testmar1$V281-exp1.lm.predmar1$fit)^2)
rss.exp1.lm_mar2=sum((testmar2$V281-exp1.lm.predmar2$fit)^2)
rss.exp2.lm_feb1=sum((testfeb1$V281-exp2.lm.predfeb1$fit)^2)
rss.exp2.lm_feb2=sum((testfeb2$V281-exp2.lm.predfeb2$fit)^2)
rss.exp2.lm_mar1=sum((testmar1$V281-exp2.lm.predmar1$fit)^2)
rss.exp2.lm_mar2=sum((testmar2$V281-exp2.lm.predmar2$fit)^2)


#Rss value for linear regression model with categorical values
rss.exp1.lm_feb1cat
rss.exp1.lm_feb2cat
rss.exp1.lm_mar1cat
rss.exp1.lm_mar2cat
rss.exp2.lm_feb1cat
rss.exp2.lm_feb2cat
rss.exp2.lm_mar1cat
rss.exp2.lm_mar2cat

#RSS value for linear regression model
rss.exp1.lm_feb1
rss.exp1.lm_feb2
rss.exp1.lm_mar1
rss.exp1.lm_mar2
rss.exp2.lm_feb1
rss.exp2.lm_feb2
rss.exp2.lm_mar1
rss.exp2.lm_mar2

#rss values calculated for Logistic regression model
rss.exp1.glm_feb1=sum((test.log.feb1$V281-exp1.glm.predfeb1$fit)^2)
rss.exp1.glm_feb2=sum((test.log.feb2$V281-exp1.glm.predfeb2$fit)^2)
rss.exp1.glm_mar1=sum((test.log.mar1$V281-exp1.glm.predmar1$fit)^2)
rss.exp1.glm_mar2=sum((test.log.mar2$V281-exp1.glm.predmar2$fit)^2)
rss.exp2.glm_feb1=sum((test.log.feb1$V281-exp2.glm.predfeb1$fit)^2)
rss.exp2.glm_feb2=sum((test.log.feb2$V281-exp2.glm.predfeb2$fit)^2)
rss.exp2.glm_mar1=sum((test.log.mar1$V281-exp2.glm.predmar1$fit)^2)
rss.exp2.glm_mar2=sum((test.log.mar2$V281-exp2.glm.predmar2$fit)^2)

#RSS value for logistic regression model
rss.exp1.glm_feb1
rss.exp1.glm_feb2
rss.exp1.glm_mar1
rss.exp1.glm_mar2
rss.exp2.glm_feb1
rss.exp2.glm_feb2
rss.exp2.glm_mar1
rss.exp2.glm_mar2

#mean squared error
MSE.exp1.lm_feb1<-rss.exp1.lm_feb1/nrow(testfeb1)
MSE.exp1.lm_feb2<-rss.exp1.lm_feb2/nrow(testfeb2)
MSE.exp1.lm_mar1<-rss.exp1.lm_mar1/nrow(testmar1)
MSE.exp1.lm_mar2<-rss.exp1.lm_mar2/nrow(testmar2)
MSE.exp2.lm_feb1<-rss.exp2.lm_feb1/nrow(testfeb1)
MSE.exp2.lm_feb2<-rss.exp2.lm_feb2/nrow(testfeb2)
MSE.exp2.lm_mar1<-rss.exp2.lm_mar1/nrow(testmar1)
MSE.exp2.lm_mar2<-rss.exp2.lm_mar2/nrow(testmar2)

#mean squared error for Logistic regression
MSE.exp1.glm_feb1<-rss.exp1.glm_feb1/nrow(testfeb1)
MSE.exp1.glm_feb2<-rss.exp1.glm_feb2/nrow(testfeb2)
MSE.exp1.glm_mar1<-rss.exp1.glm_mar1/nrow(testmar1)
MSE.exp1.glm_mar2<-rss.exp1.glm_mar2/nrow(testmar2)
MSE.exp2.glm_feb1<-rss.exp2.glm_feb1/nrow(testfeb1)
MSE.exp2.glm_feb2<-rss.exp2.glm_feb2/nrow(testfeb2)
MSE.exp2.glm_mar1<-rss.exp2.glm_mar1/nrow(testmar1)
MSE.exp2.glm_mar2<-rss.exp2.glm_mar2/nrow(testmar2)


#view the MSE values for linear regression
MSE.exp1.lm_feb1
MSE.exp1.lm_feb2
MSE.exp1.lm_mar1
MSE.exp1.lm_mar2
MSE.exp2.lm_feb1
MSE.exp2.lm_feb2
MSE.exp2.lm_mar1
MSE.exp2.lm_mar2

#view the MSE values for logistic regression
MSE.exp1.glm_feb1
MSE.exp1.glm_feb2
MSE.exp1.glm_mar1
MSE.exp1.glm_mar2
MSE.exp2.glm_feb1
MSE.exp2.glm_feb2
MSE.exp2.glm_mar1
MSE.exp2.glm_mar2



install.packages("glmnet")
install.packages("mlogit")
install.packages("caret")
install.packages("gam")
install.packages("leaps")
#D2 code
#To display all the rows max upto 99999 if using Rstudio
options(max.print = 99999)

#To read dataset csv file
read.csv("C:/vertebral_column.csv", header=TRUE)

#Load data into the variable mydata
mydata <- read.csv("C:/vertebral_column.csv", header=TRUE)

#To check all missing or null values 
sum(is.na(mydata))

#To show summary of data set such as min & max value, mean, median,quartile #of each variables.
summary(mydata )

#To plot histogram for predictor variable pelvic_incidence
xname="pelvic Incidence"
hist(mydata$pelvic_incidence, labels=TRUE, col="blue",xlab = xname, ylab= "count", main=paste("Histogram of", xname))

#To plot histogram for predictor variable pelvic_tilt
xname1="Pelvic Tilt"
hist(mydata$pelvic_tilt, labels=TRUE, col="yellow",xlab = xname1, ylab= "count", main=paste("Histogram of", xname1))

#To plot histogram for predictor variable lumbar_lordosis_angle
xname2="Lumbar Lordosis Angle"
hist(mydata$lumbar_lordosis_angle, labels=TRUE, col="green",xlab = xname2, ylab= "count", main=paste("Histogram of", xname2))

#To plot histogram for predictor variable sacral_slope
xname3="Sacral Slope"
hist(mydata$sacral_slope, labels=TRUE, col="purple",xlab = xname3, ylab= "count", main=paste("Histogram of", xname3))

#To plot histogram for predictor variable pelvic_radius
xname4="Pelvic Radius"
hist(mydata$pelvic_radius, labels=TRUE, col="red",xlab = xname4, ylab= "count", main=paste("Histogram of", xname4))

#To plot histogram for predictor variable degree_spondylolithesis
xname5="Degree Spondylolithesis"
hist(mydata$degree_spondylolisthesis , labels=TRUE, col="grey",xlab = xname5, ylab= "count", main=paste("Histogram of", xname5))

#To plot bar graph for multi class response variable "class"
library(MASS)
class = (mydata$class)
class.freq = table(class)
barplot(class.freq, main ="Bar Graph for multi class response variable CLASS", col ="black")

#D3 code
#correlation matrix
round(cor(mydata[,-(7:10)]),2)

#pair plots 
pairs(mydata)

##Splitting dataset into train and test data
## 75% of the sample size
smp_size <- floor(0.75 * nrow(mydata))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)

train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]

train
test


#multinominal logistic regression
#case 1: With pelvic_incidence and class_output as a outcome variable
#multinominal logistic regression
library("mlogit")
data_ml <- mlogit.data(train, shape = "wide", choice = "class_output")
mlogit.fit = mlogit(formula = class_output ~ 1 | pelvic_incidence + lumbar_lordosis_angle + pelvic_radius + degree_spondylolisthesis, data_ml )
summary(mlogit.fit)

#case 2: Without pelvic_incidence and class_output as a outcome variable
#multinominal logistic regression
library("mlogit")
data_ml = mlogit.data(train, shape = "wide", choice = "class_output")
mlogit.fit = mlogit(formula = class_output ~ 1 | pelvic_tilt + lumbar_lordosis_angle + sacral_slope + pelvic_radius + degree_spondylolisthesis, data_ml )
summary(mlogit.fit)


#LDA
#case 1: With pelvic_incidence and class_output as a outcome variable
library(MASS)
lda.fit=lda(class_output~pelvic_incidence+lumbar_lordosis_angle+pelvic_radius+degree_spondylolisthesis , data= train)
lda.fit
lda.pred=predict(lda.fit,test)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class, test$class_output)
mean(lda.class==test$class_output)

#case 2: Without pelvic_incidence and class_output as a outcome variable
library(MASS)
lda.fit=lda(class_output~pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+degree_spondylolisthesis , data= train)
lda.fit
lda.pred=predict(lda.fit,test)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class, test$class_output)
mean(lda.class==test$class_output)

#QDA
#case 1: With pelvic_incidence and class_output as a outcome variable
library(MASS)
qda.fit=qda(class_output~pelvic_incidence+lumbar_lordosis_angle+pelvic_radius+degree_spondylolisthesis , data= train)
qda.fit
qda.pred=predict(qda.fit,test)
names(qda.pred)
qda.class=qda.pred$class
table(qda.class, test$class_output)
mean(qda.class==test$class_output)

#case 2: Without pelvic_incidence and class_output as a outcome variable
library(MASS)
qda.fit=qda(class_output~pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+degree_spondylolisthesis , data= train)
qda.fit
qda.pred=predict(qda.fit,test)
names(qda.pred)
qda.class=qda.pred$class
table(qda.class, test$class_output)
mean(qda.class==test$class_output)

#KNN
#case 1: With pelvic_incidence and class_output as a outcome variable
library(class)
attach(mydata)
tr=1:232
train.X=cbind(pelvic_incidence,lumbar_lordosis_angle,pelvic_radius,degree_spondylolisthesis)[tr,]
test.X=cbind(pelvic_incidence,lumbar_lordosis_angle,pelvic_radius,degree_spondylolisthesis)[-tr,]
train.class_output=class_output[tr]
set.seed(1)
knn.pred=knn(train.X,test.X,train.class_output, k=1)
table(knn.pred, test$class_output)
mean(knn.pred == test$class_output)

knn.pred=knn(train.X,test.X,train.class_output, k=3)
table(knn.pred, test$class_output)
mean(knn.pred == test$class_output)

knn.pred=knn(train.X,test.X,train.class_output, k=5)
table(knn.pred, test$class_output)
mean(knn.pred == test$class_output)

knn.pred=knn(train.X,test.X,train.class_output, k=7)
table(knn.pred, test$class_output)
mean(knn.pred == test$class_output)

knn.pred=knn(train.X,test.X,train.class_output, k=9)
table(knn.pred, test$class_output)
mean(knn.pred == test$class_output)

#case 2: Without pelvic_incidence and class_output as a outcome variable
library(class)
tr=1:232
train.X=cbind(pelvic_tilt,lumbar_lordosis_angle,sacral_slope,pelvic_radius,degree_spondylolisthesis)[tr,]
test.X=cbind(pelvic_tilt,lumbar_lordosis_angle,sacral_slope,pelvic_radius,degree_spondylolisthesis)[-tr,]
train.class_output=class_output[tr]
set.seed(1)
knn.pred=knn(train.X,test.X,train.class_output, k=1)
table(knn.pred, test$class_output)
mean(knn.pred == test$class_output)

knn.pred=knn(train.X,test.X,train.class_output, k=3)
table(knn.pred, test$class_output)
mean(knn.pred == test$class_output)

knn.pred=knn(train.X,test.X,train.class_output, k=5)
table(knn.pred, test$class_output)
mean(knn.pred == test$class_output)

knn.pred=knn(train.X,test.X,train.class_output, k=7)
table(knn.pred, test$class_output)
mean(knn.pred == test$class_output)

knn.pred=knn(train.X,test.X,train.class_output, k=9)
table(knn.pred, test$class_output)
mean(knn.pred == test$class_output)

#D4 code
# Model assessment using the entire dataset as training data
library(MASS)
attach(mydata)
set.seed(1)
miscal = rep(0,10)
deg = c(1:10)
for (i in 1:10){
  lda.fit=lda(class_output~poly(pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+degree_spondylolisthesis,i) ,data= mydata)
  lda.pred=predict(lda.fit,mydata$class_output)
  lda.class=(lda.pred$class)
  #table(lda.class, mydata$class_output)
  miscal[i] = mean(mydata$class_output != lda.class)
}
miscal
plot(deg,miscal,type ="b",col = "red", xlab = "Degree of Polynomial", ylab = "Misclassification rate", main = "Predictive Performance by using The entire dataset as training data" )

# Model assessment using Validation Set Approach
library(MASS)
set.seed(1)
miscal = rep(0,10)
deg = c(1:10)
train = sample(310,155)
for (i in 1:10){
  lda.fit=lda(class_output~poly(pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+degree_spondylolisthesis,i) ,data= mydata, subset= train)
  lda.pred=predict(lda.fit,mydata$class_output[-train])
  lda.class=(lda.pred$class)
  #table(lda.class, mydata$class_output)
  miscal[i] = mean(mydata$class_output != lda.class)
}
miscal
plot(deg,miscal,type ="b",col = "blue", xlab = "Degree of Polynomial", ylab = "Misclassification rate", main = "Predictive Performance by using Validation Set Approach" )


# Model assessment with LOOCV
library(caret)
set.seed(1)
model = list()
miscal = rep(0,10)
train_control <- trainControl(method="LOOCV")
deg=c(1:10)
for (i in 1:10){
  f <- bquote(class_output~poly(pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+degree_spondylolisthesis, .(i)))
  model[[i]] <- train(as.formula(f), data=mydata, trControl=train_control, method="lda")
  miscal[i]= (1-model[[i]]$results$Accuracy)
}
miscal
plot(deg,miscal,type ="b",col = "green", xlab = "Degree of Polynomial", ylab = "Misclassification rate", main = "Predictive Performance by using LOOCV" )



# Model assessment with 5 fold cross - validation
library(caret)
set.seed(1)
model = list()
miscal = rep(0,10)
train_control <- trainControl(method="cv", number=5)
deg=c(1:10)
for (i in 1:10){
  f <- bquote(class_output~poly(pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+degree_spondylolisthesis, .(i)))
  model[[i]] <- train(as.formula(f), data=mydata, trControl=train_control, method="lda")
  miscal[i]= (1-model[[i]]$results$Accuracy)
}
miscal
plot(deg,miscal,type ="b",col = "purple", xlab = "Degree of Polynomial", ylab = "Misclassification rate", main = "Predictive Performance by using 5-Fold Cross Validation" )



# Model assessment with 10 fold cross - validation
library(caret)
set.seed(1)
model = list()
miscal = rep(0,10)
train_control <- trainControl(method="cv", number=10)
deg=c(1:10)
for (i in 1:10){
  f <- bquote(class_output~poly(pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+degree_spondylolisthesis, .(i)))
  model[[i]] <- train(as.formula(f), data=mydata, trControl=train_control, method="lda")
  miscal[i]= (1-model[[i]]$results$Accuracy)
}
miscal
plot(deg,miscal,type ="b",col = "orange", xlab = "Degree of Polynomial", ylab = "Misclassification rate", main = "Predictive Performance by using 10-Fold Cross Validation" )


#Bootstrap Approach
library(boot)
boot.fn=function(data, index)
  return(coef(lda(class_output~pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+degree_spondylolisthesis, data=data, subset= index)))
boot.fn(mydata,1:310)
set.seed(1)
boot.fn(mydata,sample(310,310, replace=T))
boot.fn(mydata,sample(310,310, replace=T))
boot(mydata,boot.fn,1000)

lda.fit=lda(class_output~pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+degree_spondylolisthesis , data= mydata)
lda.fit

#D5
#K-fold Forward Selection Method
library(leaps)
predict.regsubsets =function (object ,newdata ,id){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

#Creating folds matrix 
set.seed(1)
k=10
folds=sample(1:k,nrow(mydata),replace=TRUE)
table(folds)
cv.errors=matrix(NA,k,5, dimnames=list(NULL, paste(1:5)))
cv.errors
#K=10 Cross Validation - forward
for(j in 1:k){
  regfit.fwd=regsubsets(class_output~., data=mydata[folds!=j,], nvmax=5, method = "forward")
  for(i in 1:5){								
    pred=predict.regsubsets(regfit.fwd,mydata[folds==j,],id=i)
    cv.errors[j,i]=mean((mydata$class_output[folds==j]-pred)^2)
  }
}

summary(regfit.fwd)
fwd.summary=summary(regfit.fwd)
names(fwd.summary)
fwd.summary$rsq
fwd.summary$rss

plot(reg.summary$rsq ,xlab=" Number of Variables ",ylab=" RSq",type="l")
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSs",type="l")
plot(regfit.fwd ,scale ="r2")
coef(regfit.fwd,5)

#K-fold Backward Selection Method

#Predict Function
library(leaps)
predict.regsubsets =function (object ,newdata ,id){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

#Creating folds matrix 
set.seed(1)
k=10
folds=sample(rep(1:k,nrow(mydata)),replace=TRUE)
table(folds)
cv.errors=matrix(NA,k,5, dimnames=list(NULL, paste(1:5)))
cv.errors
#K=10 Cross Validation - forward
for(j in 1:k){
  regfit.bwd=regsubsets(class_output~., data=mydata[folds!=j,], nvmax=5, method = "backward")
  for(i in 5:1){								
    pred=predict.regsubsets(regfit.bwd,mydata[folds==j,],id=i)
    cv.errors[j,i]=mean((mydata$class_output[folds==j]-pred)^2)
  }
}
summary(regfit.bwd)
bwd.summary=summary(regfit.bwd)
names(bwd.summary)
bwd.summary$rsq
bwd.summary$rss
plot(reg.summary$rsq ,xlab=" Number of Variables ",ylab=" RSq",
     type="l")
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSs",
     type="l")
plot(regfit.bwd ,scale ="r2")
coef(regfit.bwd,5)

#Choosing Among Models Using K-folds Cross-Validation Method
library(leaps)
predict.regsubsets =function (object ,newdata ,id){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

set.seed(1)
k=10
folds=sample(1:k,nrow(mydata),replace=TRUE)
cv.errors=matrix(NA,k,5, dimnames=list(NULL, paste(1:5)))
for(j in 1:k){
  best.fit=regsubsets(class_output~.,data=mydata[folds!=j,],nvmax=5)
  for(i in 1:5){
    pred=predict.regsubsets(best.fit,mydata[folds==j,],id=i)
    cv.errors[j,i]=mean((mydata$class_output[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(class_output~.,data=mydata, nvmax=5)
coef(reg.best,11)



#Ridge Regression

library(glmnet)
grid=10^seq(10,-2,length=100)
x=model.matrix(class_output ~ pelvic_incidence + pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+degree_spondylolisthesis,data= mydata)[,-1]
y=mydata$class_output
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12, family="multinomial")
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
ridge.pred=predict(ridge.mod,s=0,newx=x[test,])
predict(ridge.mod,s=0,type="coefficients")
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0, family="multinomial")
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
out=glmnet(x,y,alpha=0, family="multinomial")
predict(out,type="coefficients",s=bestlam)

#LASSO

library(glmnet)
x=model.matrix(class_output ~ pelvic_incidence +pelvic_tilt+lumbar_lordosis_angle+sacral_slope+pelvic_radius+degree_spondylolisthesis,mydata)[,-1]
y=mydata$class_output



train1=sample(1:nrow(x),nrow(x)/2)
test1=(-train1)

y.test <- y[test1]

summary(train1)
summary(test1)

lasso.mod=glmnet(x[train1,],y[train1],alpha=1,family="multinomial")
plot(lasso.mod,xvar="lambda", label=TRUE)

cv.out=cv.glmnet(x[train1,],y[train1],alpha=1,family="multinomial")
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test1,])
lasso.pred
out=glmnet(x,y,alpha=1,family="multinomial")
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef

#GAM

##Splitting dataset into train and test data
## 75% of the sample size
smp_size <- floor(0.75 * nrow(mydata))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)

train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]

#GAM

library(gam)

gam.m1=gam(output_s~s(pelvic_tilt,2)+s(lumbar_lordosis_angle,2)+ns(sacral_slope)+s(pelvic_radius,4)+s(degree_spondylolisthesis,5),family = binomial, data=train)
gam.m4=gam(output_s~s(pelvic_tilt,4)+s(lumbar_lordosis_angle,4)+s(sacral_slope,4)+(pelvic_radius)+(degree_spondylolisthesis),family = binomial, data=train)
gam.m5=gam(output_s~s(pelvic_tilt,5)+s(lumbar_lordosis_angle,4)+s(sacral_slope,4)+s(pelvic_radius,2)+s(degree_spondylolisthesis,2),family = binomial, data=train)
gam.m6=gam(output_s~s(pelvic_tilt,4)+s(lumbar_lordosis_angle,3)+s(sacral_slope,4)+s(pelvic_radius,5)+s(degree_spondylolisthesis,4),family = binomial, data=train)
gam.m2=gam(output_s~s(pelvic_tilt,6)+s(lumbar_lordosis_angle,2)+(sacral_slope)+s(pelvic_radius,4)+s(degree_spondylolisthesis,5),family = binomial, data=train)
gam.m3=gam(output_s~s(pelvic_tilt,3)+s(lumbar_lordosis_angle,2)+(sacral_slope)+s(pelvic_radius,4)+s(degree_spondylolisthesis,5),family = binomial, data=train)

anova(gam.m1,gam.m4,gam.m5,gam.m6, test = "Chisq")
summary(gam.m4)

par(mfrow=c(1,5))
plot(gam.m4, se=TRUE,col="blue")

preds1 <- predict(gam.m4, newdata = test)




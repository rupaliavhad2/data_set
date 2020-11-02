#IMPORTING DATA SET
advertising <- read.csv("C:/Users/DELL/Desktop/RUPALI/R FILES/advertising.csv")
ad<-advertising
names(ad)
str(ad)

#DELETING COLUMNS
ad$Ad.Topic.Line<-NULL
ad$City<-NULL
ad$Country<-NULL
ad$Timestamp<-NULL

#CHECKING THE DATA TYPE
names(ad)
str(ad)

#DATA CONVERSION
ad$Age<- as.numeric(ad$Age)
ad$Gender<-as.factor(ad$Gender)
ad$Clicked.on.Ad<- as.factor(ad$Clicked.on.Ad)
str(ad)

#MISSING VALUE 
summary(ad)

#IDENTIFYING OUTLIERS
boxplot(ad$Daily.Time.Spent.on.Site)
boxplot(ad$Age)
boxplot(ad$Area.Income)
boxplot(ad$Daily.Internet.Usage)

#TREATMENT OF OUTLIER FOR AREA.INCOME
boxplot(ad$Area.Income)
summary(ad$Area.Income)
lower<-47032-1.5*IQR(ad$Area.Income);lower
ad$Area.Income[ad$Area.Income<lower]<-lower
boxplot(ad$Area.Income)
summary(ad$Area.Income)

#MULTIPLE LOGISTIC REGRESSION
library(caret)
data<-createDataPartition(ad$Clicked.on.Ad,p=0.70,list = FALSE)
training<-ad[data,]
testing<-ad[-data,]

#MODEL BUILDING
model=glm(Clicked.on.Ad~.,family = 'binomial' , data=training)
summary(model)

#VARIABLE SIGNIFICANCE SELECTION
reg.model= step(glm(Clicked.on.Ad~.,family = 'binomial',data = training)
                ,direction = "both")
summary(reg.model)

#BUSINESS MODEL
table(training$Gender)

#MODEL BUILDING
reg.model1=step(glm(Clicked.on.Ad~relevel(Gender,ref=1)+Daily.Time.Spent.on.Site 
                      +Age + Area.Income + 
                      Daily.Internet.Usage,
                    family='binomial',data=training),direction = "both")
summary(reg.model1)

Acc(reg.model1)

#TO CHECK MULTICOLLINEARITY
library(car)
vif(reg.model1)

#TO GET ODDS RATIO
exp(coef(reg.model1))

#PREDICTION
testing$probs<-predict(reg.model1,testing,type='response')
testing$Predict<-as.factor(ifelse(testing$probs>0.70,1,0))
table(testing$Clicked.on.Ad,testing$Predict)
library(caret)
confusionMatrix(testing$Clicked.on.Ad,testing$Predict)

library(ROCR)
library(ggplot2)
predictTrain=predict(reg.model,testing,type="response")
ROCRpred=prediction(predictTrain,testing$Clicked.on.Ad)
ROCRperf=performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)
pred=prediction(testing$probs,testing$Clicked.on.Ad)
as.numeric(performance(pred,"auc")@y.values)

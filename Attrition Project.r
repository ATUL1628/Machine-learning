
'C:/Users/AC00466058/Desktop/My R Project/Attrition Case Study.csv'
inputData<-read.csv('C:/Users/AC00466058/Desktop/My R Project/Attrition Case Study.csv',sep=",")
inputData<-read.csv(file.choose(),sep=",")
str(inputData)

library("dplyr")
num_var<-select_if(inputData, is.numeric)
colnames(num_var)
library(dplyr)
library(purrr)
library(tidyr)#need to install the library
library(reshape2)
library(ggplot2)
library(ggpubr)
cormattrix<-round(cor(num_var),2)
head(cormattrix)
cormat_plot <- melt(cormattrix)
cormat_plot
ggplot(data =cormat_plot,aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                               size = 12, hjust = 1))
fact_var<-select_if(inputData, is.factor)
colnames(fact_var)
head(inputData)

a<-ggplot(inputData,aes(x=Age,y=MonthlyIncome))+geom_histogram(stat='identity',aes(fill=as.factor(Attrition)))
a
b<-ggplot(inputData,aes(x=Department,y=PercentSalaryHike))+geom_bar(stat='identity',aes(fill=as.factor(inputData$Attrition)))+
  theme(legend.title = element_text(colour="blue", size=10, 
                                    face="bold"))
b###need to convert scale into percentage

c <- ggplot(inputData, aes(x=Gender))+
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.4) + 
  ylab("Percentage") + coord_flip() + theme_minimal()+ggtitle("Male and Female Ratio")
c
d<-ggplot(inputData,aes(y=inputData$DistanceFromHome,x=WorkLifeBalance))+geom_boxplot(aes(fill=Gender))+
  facet_grid(~Attrition)+ggtitle("Worklife balance and Distance from home")
d#legend & Axis
e<-c <- ggplot(inputData, aes(x=inputData$JobInvolvement,fill=Gender))+
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + coord_flip() + theme_minimal()+facet_grid(~inputData$Attrition)+ggtitle("Job involvement and Attrition")
e
f<-ggplot(inputData,aes(inputData$YearsAtCompany,fill=Gender))+
  geom_histogram(bins=20)+ coord_flip() + theme_minimal()+facet_grid(~inputData$Attrition)+
  ggtitle("Attrition Vs Years at company")
f
g<-ggplot(inputData,aes(inputData$Age,fill=inputData$Department))+
  geom_histogram(bins=5)+facet_grid(~Attrition)+ggtitle('Attrition vs age vs departments')
g
h<-ggplot(inputData,aes(inputData$OverTime,fill=Gender))+geom_bar()+facet_grid(~Attrition)+
  ggtitle("Attrition vs Overtime")
h
#############################
i<-ggplot() + geom_bar(aes(y = ..count..,x =as.factor(Age),fill = as.factor(inputData$Attrition)),data=inputData,position = position_stack())+
  ggtitle("Attrition Vs Age")
i
############################
ar1<-ggarrange(a,b,c,d,e,f)
ar2<-ggarrange(g,h,i)
ar2
dim(inputData)
table(inputData$Attrition)
input_ones <- inputData[which(inputData$Attrition == 1), ]  # all 1's
input_zeros <- inputData[which(inputData$Attrition == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 
library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
model <- glm(Attrition~Age+BusinessTravel        +Department +DistanceFromHome          +Education+EducationField
             +EmployeeNumber         +EnvironmentSatisfaction    +Gender       +HourlyRate   
             +JobInvolvement  +JobLevel      +JobRole       +JobSatisfaction       
             +MaritalStatus      +MonthlyIncome       +MonthlyRate 
             +NumCompaniesWorked+OverTime      
             +PercentSalaryHike         +PerformanceRating 
             +RelationshipSatisfaction+StockOptionLevel      +TotalWorkingYears  
             +TrainingTimesLastYear   +WorkLifeBalance     +YearsAtCompany    +YearsInCurrentRole 
             +YearsSinceLastPromotion         +YearsWithCurrManager,data=trainingData, family=binomial())
summary(model)
vif(model)
anova(model, test="Chisq")
testData$prediction <- predict(model,newdata =testData,type="response")
library(pROC)

testData$pred2 <- ifelse(prediction>=0.51,1,0)

library(gmodels)
table(testData$Attrition,testData$pred2)
View(testData)
table(testData$Attrition)
table(testData$pred2)
Con<-table(testData$Attrition, prediction > 0.5)
AccuracyRate <- sum(diag(Con))/sum(Con)
AccuracyRate

library(ROCR)
ROCRpred <- prediction(testData$prediction, testData$Attrition)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

auc_ROCR <- performance(ROCRpred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR #.77 IS the AREA under the curve
inputData$Attrition
library(mlogit)

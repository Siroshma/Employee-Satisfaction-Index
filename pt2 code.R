library(naivebayes)
library(FSelector)
library(Metrics)
library(dplyr)
library(ggplot2)
library(caret)
emp<- read.csv("D:\\Users\\SIROSHMA\\Downloads\\pt2\\empsatind.csv")
print(emp)

dim(emp)

dim(emp)
length(emp)
head(emp)
head(emp, n=10)
tail(emp)
tail(emp,n=10)
emp[6,]
emp[,c(10,11,12,13)]
emp[1:10,]
emp[,9]
str(emp)

#Shuffling or Re-ordering Rows
shuffle_index <- sample(1:nrow(emp))
print(head(shuffle_index))
emp <- emp[shuffle_index,]
print(head(emp))
print(tail(emp))
print(dim(emp))

# DATA PREPROCESSING
summary(emp)
print(anyNA(emp))
emp <- select(emp,-emp_id)
emp
emp$Dept=as.factor(emp$Dept)
emp$location =as.factor(emp$location )
emp$education=as.factor(emp$education)
emp$recruitment_type=as.factor(emp$recruitment_type)
emp$satisfied =as.factor(emp$satisfied)
summary(emp)

y<-emp$satisfied
print(y)

barplot(table(emp$satisfied),
        main="Employee Satisfaction", col="Green")
barplot(table(y), main = "Satisfaction Index",xlab = "Class",col = c("red","green"))
legend("bottom",c("Not Satisfied","Satisfied"),fill = c("red","green"))

x2<- emp$age
hist(x2, main = "Age of Employees",xlab="Age", ylab="No.of Employees",col = "Brown")

x3<- emp$Dept
barplot(table(x3),
        main="Department", col="Yellow")
ggplot(emp,aes(x=Dept,fill=satisfied)
)+geom_bar(position = position_dodge())
x3

x4<- emp$location 
barplot(table(x4))
ggplot(emp,aes(x=location,fill=satisfied)
)+geom_bar(position = position_dodge())
x4

x5<- emp$education
barplot(table(x5))
ggplot(emp,aes(x=education,fill=satisfied)
)+geom_bar(position = position_dodge())

x6<- emp$recruitment_type
barplot(table(x6))
ggplot(emp,aes(x=recruitment_type,fill=satisfied)
)+geom_bar(position = position_dodge())

x7<- emp$job_level
barplot(table(emp$job_level))
ggplot(emp,aes(x=job_level,fill=satisfied)
)+geom_bar(position = position_dodge())

x8<- emp$rating
ggplot(emp,aes(x=rating,fill=satisfied)
)+geom_bar(position = position_dodge())

x9<- emp$onsite
ggplot(emp,aes(x=onsite,fill=satisfied)
)+geom_bar(position = position_dodge())

x10<- emp$awards
barplot(table(emp$awards))

x12<- emp$certifications
x12
ggplot(emp,aes(x=certifications,fill=satisfied)
)+geom_bar(position = position_dodge())

x13<- emp$salary
x13

res <- gain.ratio(satisfied~.,emp)
print(res)

print(emp)
dim(emp)

emp<-select(emp,-c(age,Dept,education,onsite,certifications))
# Sampling of Datasets
set.seed(1)

ind <- sample(2, nrow(emp), replace=TRUE, prob=c(0.80, 0.20))
trainData <- emp[ind==1,]
testData <- emp[ind==2,]

print(trainData)
print(testData)
naive_model <- naive_bayes(satisfied ~ location+recruitment_type+job_level+
                             rating+awards+ salary, 
                           data=trainData, type="C-classification")

print(naive_model)
plot(naive_model)
y_test=testData$satisfied
testData
#predict on test data
testPred <- predict(naive_model, newdata = testData)
print(testPred)
print(testData$satisfied)
str(testPred)
confusionMatrix(y_test,testPred)

y_Test=as.numeric(y_test)
y_Pred=as.numeric(testPred)
str(y_Test)
str(y_Pred)

res_rmse <- rmse(y_Test,y_Pred)
res_rmse

a<-data.frame(age=38,Dept="Marketing",location= "Suburb", education="UG",
              recruitment_type ="Referral", job_level= 2, rating= 5, onsite= 1,
              awards=2, certifications= 0, salary = 29805)
result <- predict(naive_model,a)
print(result)
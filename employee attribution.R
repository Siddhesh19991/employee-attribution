employee<- read.csv("~/Downloads/datasets_1067_1925_WA_Fn-UseC_-HR-Employee-Attrition.csv")

library(dplyr)
names(employee)[1]<-"Age"


str(employee)

table(employee$Attrition)#imbalance dataset

employee[sapply(employee, is.character)] <- lapply(employee[sapply(employee, is.character)], 
                                       as.factor)


library(ggplot2)

#gender

plot(table(employee$Gender))#more males employed
table(employee$Gender,employee$JobRole)
table(employee$Gender,employee$Department)
#same proportion in al departments for male and female 
#more in R&D
prop.table(table(employee$Gender,employee$Attrition),1)#No gender issue

ggplot(employee,aes(Attrition,JobSatisfaction))+geom_boxplot()+facet_grid(~Gender)

employee%>%select(Gender,Age)%>%group_by(Gender)%>%summarise(avg=mean(Age))
#almost the same age for both 


employee%>%select(Gender,Age)%>%group_by(Gender)%>%summarise(avg=mean(employee$MonthlyIncome))


male<-subset(employee,Gender=="Male")
female<-subset(employee,Gender=="Female")

#average salary for female is more


employee%>%select(Gender,MonthlyIncome,Department)%>%group_by(Gender,Department)%>%summarise(mean(MonthlyIncome))
#males are paid more in sales ,while women are paid more in Human resources.

prop.table(table(employee$Attrition,employee$Department),1)
#most of the employees that attribute come from R&D

#generation

employee$Generation <- ifelse(employee$Age<37,"Millenials",
                        ifelse(employee$Age>=38 & employee$Age<54,"Generation X",
                               ifelse(employee$Age>=54 & employee$Age<73,"Boomers","Silent"
                               )))

prop.table(table(employee$NumCompaniesWorked,employee$Generation),1)
plot(table(employee$Attrition,employee$Generation))

#we see that millenails and generation X tend to shift companies more.

prop.table(table(employee$Attrition,employee$Education),1)
prop.table(table(employee$Generation,employee$Education),1)

#many hold a level 3 education and millenials and generation X at this stage ofent leave the company.
#And people at level 1 tend to leave more relative the proportion.
employee%>%select(Attrition,Education)%>%group_by(Education,Attrition)%>%summarise(n())


#income levels

ggplot(employee,aes(Department,MonthlyIncome))+geom_bar(stat="identity")+facet_grid(~Attrition)
ggplot(employee,aes(PercentSalaryHike,MonthlyIncome))+geom_bar(stat="identity")+facet_grid(~Attrition)
ggplot(employee,aes(YearsSinceLastPromotion,MonthlyIncome))+geom_bar(stat="identity")+facet_grid(~Attrition)

#we see employeesn with big difference in income between those who attribute and those who dont.

ggplot(employee,aes(JobSatisfaction,MonthlyIncome))+geom_bar(stat="identity")+facet_grid(~Attrition)

#low job satisfaction

ggplot(employee,aes(EnvironmentSatisfaction,MonthlyIncome))+geom_bar(stat="identity")+facet_grid(~Attrition)

ggplot(employee,aes(Department))+geom_bar()+facet_grid(~Attrition)

#many ppl from R&D leave ,they have a lower overall satisfaction level

prop.table(table(employee$WorkLifeBalance,employee$Attrition),1)

#ppl withb less worklife balance tend to quit



#correlation

num<-select_if(employee,is.numeric)

corr <- round(cor(num), 1)

library(ggcorrplot)
ggcorrplot(corr, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "#01A9DB"), 
           title="Correlogram Employee Attritions", 
           ggtheme=theme_minimal())
p<-corr>0.6

#The higher the total working years the higher the monthly income of an employee.
#The higher the percent salary hike the higher the performance rating.
#The higher the years with current manager the higher the years since last promotion.
#The higher the age the higher the monthly income.
#monthly income is more if the jobsatisfaction is more 


#splitting the data
library(caret)
intrain<-createDataPartition(y=employee$Attrition,p=0.7,list = FALSE)
training<-employee[intrain,]
test<-employee[-intrain,]


prop.table(table(training$Attrition))
prop.table(table(test$Attrition))
#equal distribution in both(attrition)


#model
library(rpart)
model1<-rpart(Attrition~.,data=training)


plot(model1, uniform=TRUE, branch=0.6, margin=0.05)
text(model1, all=TRUE, use.n=TRUE)dwllmdl

#features importance
sort(model1$variable.importance,n=10)

pred1<-predict(model1,newdata=training,type = "class")
pred1<-ifelse(pred1>0.)

confusionMatrix(pred1,training$Attrition)

##############################################################
#H2o package method
##############################################################
library(h2o)
h2o.init()

employee<-as.h2o(employee)

splits <- h2o.splitFrame(data = employee, 
                         ratios = c(0.7, 0.15),  
                         seed = 1)  
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]



y<-"Attrition"
x<-setdiff(names(employee),"Attrition")

aml<-h2o.automl(y=y,x=x,training_frame = train,validation_frame=valid,max_models = 10)

lb<-aml@leaderboard



model_id<-as.data.frame(lb$model_id)[,1]
best_family <- h2o.getModel(grep("StackedEnsemble_BestOfFamily", model_id, value=TRUE)[1])
obtain_model <- h2o.getModel(best_family@model$metalearner$name)


h2o.varimp(obtain_model)

options(repr.plot.width=8, repr.plot.height=4) 
h2o.varimp_plot(obtain_model)


glm <- h2o.getModel(grep("GLM", model_id, value = TRUE)[1])

h2o.varimp(glm)
h2o.varimp_plot(glm)


glm@parameters


best_dl_perf <- h2o.performance(model = glm, 
                                newdata = test)
#accuracy=86.5%


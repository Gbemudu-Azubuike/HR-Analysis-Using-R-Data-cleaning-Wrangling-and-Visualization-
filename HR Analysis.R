#Load Libraries
library(tidyverse)
library(reshape2)

#Import data frame
HR_Analysis <- read.csv ('/Downloads/Datasets/HR-Employee-Attrition.csv')

#Data Cleaning

#Structure of data frame
str(HR_Analysis)

#Summary of data frame
names(HR_Analysis)
glimpse(HR_Analysis)
dim(HR_Analysis)

#Check for missing values
HR_Analysis %>% select (everything(HR_Analysis)) %>% 
  filter(!complete.cases(.)) %>% 
  view()

#Check for duplicates
duplicated(HR_Analysis)
# Or better still
HR_Analysis[duplicated(HR_Analysis),]

HR_Analysis %>% distinct() %>% view()

summary(HR_Analysis)
# Data Cleaning Complete


#View First 6 rows of data
head(HR_Analysis)

#View last 6 rows of data
tail(HR_Analysis)

#Counting the Frequency of values in the Attrition column
table(HR_Analysis$Attrition) %>% view()

#Counting the total number of values in the Attrition column
length(HR_Analysis$Attrition)

#Finding the Percentage of each frequency in the attrition column
Attrition_Percentage <- (table(HR_Analysis$Attrition)/length(HR_Analysis$Attrition)) * 100
view(Attrition_Percentage)

#Count of Male and Female in the Organization
table(HR_Analysis$Gender) %>% print()

#Total count of gender
length(HR_Analysis$Gender)

#Percentage of Male to Female in the organization
Gender_Percentage <- (table(HR_Analysis$Gender)/length(HR_Analysis$gender)) * 100
view(Gender_Percentage)

#Count of Single, Married and Divorced
table(HR_Analysis$MaritalStatus)

length(HR_Analysis$MaritalStatus)

#Marital Status by Percentage
MS_Percentage <- ((table(HR_Analysis$MaritalStatus)/length(HR_Analysis$MaritalStatus)) * 100)

view(MS_Percentage)

#Count of Staff who did Overtime
table(HR_Analysis$OverTime)

#Percentage of Staff who did Overtime
Overtime_Percentage <- (table(HR_Analysis$OverTime)/length(HR_Analysis$OverTime)) * 100
view(Overtime_Percentage)

#Find the average Job Satisfaction
Average_Employee_Job_Satisfaction <- mean(HR_Analysis$JobSatisfaction)


#Generate a Pearson Correlation Matrix Value for relevant columns of Data
Correlation_Variable <- HR_Analysis[ ,c("Age", "DailyRate", "DistanceFromHome", "EnvironmentSatisfaction", "HourlyRate", "JobInvolvement", "JobLevel", "JobSatisfaction",
                   "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked", "PercentSalaryHike", "PerformanceRating",
                   "RelationshipSatisfaction", "StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear", "WorkLifeBalance", "YearsAtCompany",
                   "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager")]

cor(Correlation_Variable) %>% view()
Cor_Matrix <- cor(Correlation_Variable)
Cor_HeatMap <- melt(Cor_Matrix)

#Plot Correlation Heat Map
ggplot(data = Cor_HeatMap, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +theme(axis.text.x = element_text(angle = 90, hjust = 1))




#Charts

#Chart of Employee Attrition
ggplot(data = HR_Analysis,aes(x=Attrition, color = "Red")) + geom_bar()

#Chart of Employee Overtime
ggplot(data = HR_Analysis, aes(x=OverTime, fill = "Red")) + geom_bar()

#Chart of Marital Status vs Attrition
ggplot(data = HR_Analysis, aes(x=MaritalStatus, fill = Attrition)) + geom_bar()

#Chart of Job Role by Attrition
ggplot(data = HR_Analysis, aes(x= JobRole , fill = Attrition)) +geom_bar() + coord_flip()


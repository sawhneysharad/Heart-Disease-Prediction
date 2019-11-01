# Analysing the heart-disease-uci from kaggle datasets.
# In particular, the Cleveland database is the only one that has been used by ML researchers to this 
# date. The "target" field refers to the presence of heart disease in the patient. It is integer valued 
# from 0 (no presence) and 1 (presence) and it will get stored in the target column. Target is the 
# dependent variable and rests all the variable are the independent variable. We are analysing the 
# data to see the gender bifurcation for the patient having the heart diseases. As we are analysing 
# the clinical data set so, we are dealing with some of the clinical abbreviations.

# 1) age: The person's age in years
# 2) sex: The person's sex (1 = male, 0 = female)
# 3) cp: The chest pain experienced (Value 1: typical angina, Value 2: atypical angina, Value 3: non-anginal pain, Value 4: asymptomatic)
# 4) trestbps: The person's resting blood pressure (mm Hg on admission to the hospital)
# 5) chol: The person's cholesterol measurement in mg/dl
# 6) fbs: The person's fasting blood sugar (if > 120 mg/dl, 1 = true; 0 = false)
# 7) restecg: Resting electrocardiographic measurement (0 = normal, 1 = having ST-T wave abnormality, 2 = showing probable or definite left ventricular hypertrophy by Estes' criteria)
# 8) thalach: The person's maximum heart rate achieved
# 9) exang: Exercise induced angina (1 = yes; 0 = no)
# 10) oldpeak: ST depression induced by exercise relative to rest ('ST' relates to positions on the ECG plot)
# 11) slope: the slope of the peak exercise ST segment (Value 1: upsloping, Value 2: flat, Value 3: downsloping)
# 12) ca: The number of major vessels (0-3)
# 13) thal: A blood disorder called thalassemia (1 = normal; 2 = fixed defect; 3 = reversable defect)
# 14) target: Heart disease (0 = no, 1 = yes) 

# Looking at information of heart disease risk factors led me to the following: high cholesterol, 
# high blood pressure, diabetes, weight, family history and smoking 3. According to another source 4, 
# the major factors that can't be changed are: increasing age, male gender and heredity. Note that thalassemia, 
# one of the variables in this dataset, is heredity. Major factors that can be modified are: Smoking, 
# high cholesterol, high blood pressure, physical inactivity, and being overweight and having diabetes. 
# Other factors include stress, alcohol and poor diet/nutrition.

# Loading the required library
library('corrplot')
library('tidyverse')
library('ggcorrplot')
library('ggplot2')
library('plyr') # to use count function
library('caret')
library('caTools')

# Importing the data file
data <- read_csv(file.choose())

# DISPLAY THE FIRST FEW ROWS OF DATA
head(data)

# DISPLAY THE NUMBER OF ROWS AND COLUMNS
nrow(data)
ncol(data)

# Understanding the datatype of dataset
str(data)

# DISPLAY THE SUMMARY
summary(data)

# After analysing the summary of data we observe that no null/NA is present in our data but 
# all the columns are considered as numeric even the categorical data as well. 
# So, we will perform some data preprocessing steps to convert the categorical column to factor.

# Displaying the coralation matrix
corr <- cor(data)

# Visualize the correlation matrix
corrplot(corr)

# After observing above matrix we observe the our dependent varible is not/least related with 
# fbs, chol, trestbps, restecg as they lies between [0.2, -0.2]. So, we can remove those varible from 
# the dataset in our next data wrangling step

## Data wrangling and counting missing values

# Deleting not related variables
data = subset(data, select = c(-fbs,-chol,-restecg))

# Coverting the categorical data to factor
data$sex <- as.factor(data$sex)
data$target <- as.factor(data$target)
data$cp <- as.factor(data$cp)
data$ca <- as.factor(data$ca)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$thal <- as.factor(data$thal)

# Summary after pre-processing the data
summary(data)

# DISPLAY THE NUMBER OF NAs IN EACH COLUMN
colSums(is.na(data))

# All the continous variables are represented with the histrogram and the categorical data is 
# represented using bar graph.

# Target variable Analysis

# Bar plot for target (Heart disease) 
data$target <- as.factor(data$target)
ggplot(data, aes(x=data$target, fill=data$target)) + 
  geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Analysis of Presence and Absence of Heart Disease") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absence", "Presence"))

# From the above plot, we can observe that the number of people with heart disease is more than with a number of 
# people having no heart disease. We will perform further analysis to find out more about the relevant parameter 
# for the causes of heart diseases.

# Age variable Analysis
# Counting the frequency of the values of the age
ageCount <- count(data, 'age')
ageCount <- subset(ageCount[which(ageCount$freq > 10), ])

#ploting the age with frquency greater than 10
ggplot(ageCount, aes(x=ageCount$age, y=ageCount$freq)) + 
  ggtitle("Age Analysis") +
  xlab("Age")  +
  ylab("Age Count") +
  geom_bar(stat="identity")

# From the above plot we can observe that the data is present for patient is normally distributed. 
# Majorirty of the patient are between 35-72 years of age. Few are also present for 30 years and 
# for 75 years old patient. On this range bases we will divide oue age into groups of young, middle, old.

# Group the different ages in three groups (young, middle, old)
young <- data[which((data$age<45)), ]
middle <- data[which((data$age>=45)&(data$age<55)), ]
elderly <- data[which(data$age>55), ]
groups <- data.frame(age_group = c("young","middle","elderly"), group_count = c(NROW(young$age), NROW(middle$age), NROW(elderly$age)))

#ploting different age groups
ggplot(groups, aes(x=groups$age_group, y=groups$group_count, fill=groups$age_group)) + 
  ggtitle("Age Analysis") +
  xlab("Age Group")  +
  ylab("group Count") +
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Age Group", labels = c("Elderly", "Middle", "Young"))

# Adding the age groups to the dataset
data <- cbind(data, groups = ifelse((data$age<45), 0, ifelse((data$age>=45)&(data$age<55), 1, 2)))
data$groups <- as.factor(data$groups)

# we will remove the age column as this is very generalised column and we have divided it, group, to include 
# that in our analysis more specifically.

data = subset(data, select = c(-age))

# Discrete vs Discrete vs Discrete variable: age_group ~ target ~ sex
ggplot(data, aes(x= factor(data$groups), y=data$sex, colour=target)) + 
  geom_boxplot(stat = "boxplot",
               position = "dodge2") +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2) +
  xlab("Age Groups") +
  ylab("Gender") +
  ggtitle("Analysis of gender with different age group with presence or absense of heart disease")

# From the above graph, As the age is increasing the chances of heart diseases decreases, which is just opposite 
# to what I observe after doing basic research on the domain. 

# Bar plot for sex
ggplot(data, aes(x= data$sex, fill=data$target)) + 
  geom_bar() +
  xlab("Gender") +
  ylab("Gender Count") +
  ggtitle("Analysis of Gender") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# As 0 representing Female and 1 representing male data. From the graph, we can observe that 
# the count of male patients is double as compared to female patients. We can conclude that the 
# ratio of male to female is 2:1 and heart diseases are more common among females as compared with 
# male. After doing the doamin research I got to know that heart disease doen't depend on the gender of the 
# patients as it depends upon other factors like ECG, heart rate, blood pressure etc. Hence, we can remove this 
# paramter before moving forward to next step of model implementation.

data = subset(data, select = c(-sex))

# Bar plot for The chest pain experienced 
ggplot(data, aes(x= cp, fill=cp)) + 
  geom_bar() +
  xlab("Chest Pain Type") +
  ylab("Count") +
  ggtitle("Analysis of Chest Pain Experienced") +
  scale_fill_discrete(name = "Chest Pain Type", labels = c("Typical angina pain", "Atypical angina pain", "Non-Anginal pain", "Asymptomatic pain"))

# From the plot, we can conclude that most of the patients have experienced typical angina type of 
# chest pain.

# Bar plot for The chest pain ~ target
ggplot(data, aes(x= cp, fill=target)) + 
  geom_bar() +
  xlab("Chest Pain Type") +
  ylab("Count") +
  ggtitle("Analysis of Chest Pain Experienced") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# From the above graph, we can observe that patient with typical angina pain has the least chances of heart disease, 
# which is the most common type of cheast pain. Rest all other type have a equal chances of having a heart disease.
 
# Bar for ca (number of major vessels (0-3))
ggplot(data, aes(x= ca, fill=ca)) + 
geom_bar() +
  xlab("number of major vessels") +
  ylab("Count") +
  ggtitle("Analysis of number of major vessels") +
  theme(legend.position="none")

# From the graph, we can observe that most of the population doesn't have any major vessels. I can see no reference 
# to the 'number of major vessels', but given that the definition of heart disease is "...what happens when your 
# heart's blood supply is blocked or interrupted by a build-up of fatty substances in the coronary arteries", 
# it seems logical the more major vessels is a good thing, and therefore will reduce the probability of heart disease.

# Bar for ca (number of major vessels (0-3))
ggplot(data, aes(x= ca, fill=target)) + 
  geom_bar(position = 'dodge') +
  xlab("number of major vessels") +
  ylab("Count") +
  ggtitle("Analysis of number of major vessels") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# From the above plot, we can see that as the number of major blood vessels increases, 
# the probability of heart disease decreases. That makes sense, as it means more blood can get to the heart.

# Histogram for trestbps (resting blood pressure)
ggplot(data, aes(x=trestbps)) + 
  geom_histogram() +
  xlab("Resting blood pressure") +
  ylab("Count") +
  ggtitle("Analysis of blood pressure")

# From the above histogram, we can analyse that the data is now normally distributed but we can also 
# observe that outliers are present in the data. So, we remove the outliers from the data.

# removing the outliers
data$trestbps = ifelse(data$trestbps > 180, NA, data$trestbps)
data$trestbps = ifelse(is.na(data$trestbps), median(data$trestbps[which(!is.na(data$trestbps))]), data$trestbps)

# After the removal of outliers
ggplot(data, aes(x=trestbps)) + 
  geom_histogram() +
  xlab("Resting blood pressure") +
  ylab("Count") +
  ggtitle("Analysis of blood pressure")

# Normal range for blood pressure is between 90-140 and we can observe that most of the population have their 
# blood pressure under this range. Now, let analyse its impact on heart disease.

# Density graph for trestbps (resting blood pressure)
ggplot(data, aes(x = trestbps, fill = target)) +
  geom_density(alpha=0.5) +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# From the above graph, we can observe that patients equal chances of having heart disease irrespective of the 
# value of blood pressure. This observation is just opposite to what we thought. So, we can remove this parameter 
# before model implementation.

data = subset(data, select = c(-trestbps))

# Histogram for oldpeak (ST depression induced by exercise relative to rest)
ggplot(data, aes(x=oldpeak)) + 
  geom_histogram() +
  xlab("ST depression induced by exercise relative to rest") +
  ylab("Count") +
  ggtitle("Analysis of ST depression induced by exercise relative to rest")

# As the above histogram, we can observe that the graph is right-skewed so, we performed the 
# transformation on the data. As, the data contains o's as value so we will transformed it using log1p()

# From the above histogram 
data$oldpeak <- log1p(data$oldpeak)

ggplot(data, aes(x=oldpeak)) + 
  geom_histogram() +
  xlab("ST depression induced by exercise relative to rest") +
  ylab("Count") +
  ggtitle("Analysis of ST depression induced by exercise relative to rest")

# After the transformation, we can analyse that the data is now normally distributed. But we observe that there is a 
# peak on for the zero value. Now, Let's analyse the effect of oldpeak on chances of patients to have heart disease.

# Density plot for oldpeak ~ target
ggplot(data, aes(x = oldpeak, fill = target)) +
  geom_density(alpha=0.5) +
  xlab("ST depression induced") +
  ylab("Count") +
  ggtitle("Analysis of ST depression induced and presence of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# From the above graph, we can observe that as the value oldpeak increases the chances of heart disease decreases.

# Bar plot for slope (slope of the peak exercise ST segment) 
data$slope <- ifelse(data$slope == 0, 1, print(data$slope))
data$slope <- as.factor(data$slope)
ggplot(data, aes(x=data$slope, fill=data$slope)) + 
  geom_bar() +
  xlab("Slope of ST segment") +
  ylab("Count") +
  ggtitle("Analysis of slope of the peak exercise ST segment") +
  scale_fill_discrete(name = "Slope of ST segment", labels = c("Upsloping", "Flat", "Downsloping"))

# From the graph above, we can observe almost equal number of paients lies in the flat and downsloping slope.

# Plot for slope ~ target
ggplot(data, aes(x= slope, fill=target)) + 
  geom_bar(position = 'dodge') +
  xlab("slope of the peak exercise ST segment") +
  ylab("count") +
  ggtitle("Analysis of slope of the peak exercise ST segment with presence or absense of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# From the above graph we can observe that the paients with downsloping slope have higher chances of having a 
# heart disease as compared to flat slope.

# Histogram for thalach (maximum heart rate achieved)
ggplot(data, aes(x=thalach)) + 
geom_histogram() +
  xlab("Maximum heart rate achieved") +
  ylab("Count") +
  ggtitle("Analysis of maximum heart rate achieved")

# From the above histogram, we can analyse that the data is now normally distributed but we can also 
# observe that outliers are present in the data. So, we remove the outliers from the data.

# Replacing the outlier value with median value of thalach
data$thalach = ifelse(data$thalach < 75, NA, data$thalach)
data$thalach = ifelse(is.na(data$thalach), median(data$thalach[which(!is.na(data$thalach))]), data$thalach)

ggplot(data, aes(x=thalach)) + 
  geom_histogram() +
  xlab("Maximum heart rate achieved") +
  ylab("Count") +
  ggtitle("Analysis of maximum heart rate achieved")

# After the outlier replacement we can analyse that the data is now normally distributed. Now, let's analyse this 
# parameter's effect on presence of heart disease

# Density plot for thalach ~ target
ggplot(data, aes(x = thalach, fill = target)) +
  geom_density(alpha=0.5) +
  xlab("Maximum Heart Rate Achieved") +
  ylab("Count") +
  ggtitle("Analysis of relation of heart rate with presence of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# From the plot we can observe that as the value of heart rate inceases the chances of having a heart disease 
# increses.

# Bar graph for thal (blood disorder called thalassemia)
ggplot(data, aes(x=thal)) + 
geom_bar() +
  xlab("Blood disorder type") +
  ylab("Count") +
  ggtitle("Analysis of blood disorder (thalassemia)")

# Form the above graph, we can observe that some patients having 0 value as this is a invalid value as 
# valid categories are 2 = 'normal' 3 = 'fixed defect' 4 = 'reversable defect'
# so, will replace this value by taking mode of all values.

# Replacing the invalid value with mode value of thal
data$thal = ifelse(data$thal == 0, 2, data$thal)
data$thal <- as.factor(data$thal)

ggplot(data, aes(x=thal, fill=thal)) + 
  geom_bar() +
  xlab("Blood disorder type") +
  ylab("Count") +
  ggtitle("Analysis of blood disorder (thalassemia)") +
  scale_fill_discrete(name = "Blood disorder", labels = c("Normal", "Fixed defect", "reversable defect"))

# We can observe from the above graph that maximum population in our dataset has either fixed the 
# defect or don't have the disorder.Now, let's analyse this parameter's effect on presence of heart disease

# Bar plot for thal ~ target
ggplot(data, aes(x= thal, fill=target)) + 
  geom_bar(position = 'dodge') +
  xlab("blood disorder") +
  ylab("count") +
  ggtitle("Analysis of blood disorder with presence or absense of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

# From the above graph, we can observe that patients got treated for thalassemia has higher chances of having heart 
# disease. We can observe that heart diseases highly influenced by hereditary diseases  

# Conculsion of EDA
# At the start,I hypothesised, using (Googled) domain knowledge that factors such as cholesterol and age would be 
# major factors in the model. This dataset didn't show that. Instead, the number of major factors and aspects of ECG 
# results dominated. I actually feel like I've learnt a thing or two about heart disease!

# Model implementation

# As the paremeter (target), I am analysing is a binary variable having values as 0 and 1. So, I am implementing 
# logistic regression for predicting the dependent variable values.

# Dividing the data set in train and test datasets

# To get the same set every time we run the code
set.seed(123)

# Rearranging the columns to make the target as the last column
data <- data[, c(1, 2, 3, 4, 5, 6, 7, 9, 8)]

dataSample <- sample.split(data[,ncol(data)-1], SplitRatio=0.80)
trainSet = subset(data,dataSample == TRUE)
testSet = subset(data,dataSample == FALSE)

# Creating a logistic model
logisticmodel <- glm(target~.,data = trainSet, family = "binomial")

# Summary of the created model
summary(logisticmodel)

# Summary output interpretation

# In the first part, we see the deviance residuals, which are a measure of model fit. This part of output shows the distribution of 
# the deviance residuals for individual cases used in the model.

# The next part of the output shows the coefficients, their standard errors, the z-statistic and the 
# associated p-values. Both gre and gpa are statistically significant, as are the three terms for rank. The logistic regression coefficients give the change in the log odds of the outcome for a one unit increase in the predictor variable.
# For every one unit change in gre, the log odds of admission (versus non-admission) increases by 0.002.
# For a one unit increase in gpa, the log odds of being admitted to graduate school increases by 0.804.
# The indicator variables for rank have a slightly different interpretation. For example, having attended an undergraduate institution with rank of 2, versus an institution with a rank of 1, changes the log odds of admission by -0.675.
# Below the table of coefficients are fit indices, including the null and deviance residuals and the AIC. Later we show an example of how you can use these values to help assess model fit.

# Making prediction with the above model
predictdata <- predict(logisticmodel, newdata = testSet[, -12], type="response")
pred <- ifelse(predictdata>=0.5,1,0)
pred <- as.factor(pred)
observed <- testSet[,12]

# Checking the accuracy of the model
confusionMatrix(pred, observed)

# From the confusion matrix we can observe, our model is 81% accurate. 20 values of o's predicted correctly and 
# 30 values of 1's is predicted corrected out of total 61 values.

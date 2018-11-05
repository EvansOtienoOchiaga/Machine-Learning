#Machine Learning Introduction

#clear the working area
ls()
rm(list=ls())


#Step 1:Read in the dataset;
#Read in the dataset;
iris<- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), 
                header = FALSE) 
#print first lines 
head(iris)
#

#explore the dataset by making sime graphs(histogram or boxplot)
#it is important to see any correlation between two variables
#scatterplots is done using ggvis package 


#Step 2:Understand the dataset;
#load in ggvis package
install.packages('ggvis', dependencies = T)
library(ggvis)

#rename variable names before doing plots;
library(plyr)
names(iris)
iris <- rename(iris, c('V1'='Sepal.Length', 'V2'='Sepal.Width', 'V3'='Petal.Length', 'V4'='Petal.Width', 'V5'='Species'))

#iris scatter plot for sepal length and width
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()

#iris scatter plot for petal length and petal width
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()

#the scatter plot shows correlation in petal width and  petal length for different species, it also shows correlation in sepal width and length
#for iris-setosa. However futher etst is needed to prove this.

#overall correlation 'petal.length' and 'petal.Width'
cor(iris$Petal.Length,iris$Sepal.Width)

#return values of iris level;
x<-levels(iris$Species)
x

#print setosa correlation matrix
print(x[1])
cor(iris[iris$Species==x[1], 1:4])

#pront vesicolor correlation matrix
print(x[2])
cor(iris[iris$Species==x[2],1:4])

#print virginica correlation matrix
print(x[3])
cor(iris[iris$Species==x[3],1:4])

#the best way to insepct the dataset is by using head() or str() instead of orinting the whole dataset
head(iris)
str(iris) #machine learning classifiers requires that the target feature coded as factor

#division of species
table(iris$Species)

#percentages division of species
round(prop.table(table(iris$Species))*100, digits=1)

#the use of summary in understanding the dataset;
#summary overview of iris
summary(iris)

#refined sumamry overview
summary(iris[c('Petal.Width', 'Sepal.Width')])


#Step 3 now you need to understand what is relevent to the dataset
#here involve the use of KNN algorithm, K-Nearest algorithm
#The use of KNN algorithmn(this is contained in package class)

#this code will tell whether the needed package is sintsalled or not
any(grepl("class", installed.packages()))

library(class)

#while using KNn make sure that the data is normalized . if not then you need to nomalized it 
#one way is to use summary(  if minimum and maximum are far apart the the data is normalized
#iris dataset is normalized, however there is need to have code for nomalization for future use

# Build your own `normalize()` function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# Normalize the `iris` data
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))

# Summarize `iris_norm`
summary(iris_norm)

#pick from training and test sets
#it is important to divide the dataset into two, training and tests set before modelling
#you first need to set . The major advantage of setting a seed is that you can get the same 
#sequence of random numbers whenever you supply the same seed in the random number generator.
#in dividing the dataset we use the function sample()

set.seed(1234)
ind<-sample(2, nrow(iris),replace=TRUE, prob=c(0.67, 0.33))
ind

#then use the sample that is stored in the variable ind to define your training and test
#compose training set
iris.training<-iris[ind==1, 1:4]

#inspect training set
head(iris.training)

#compose test set
iris.test<-iris[ind==2, 1:4]

#inspect test set
head(iris.test)

#store the class labels in factor vectors and divide them over the training
iris.trainlabels <- iris[ind==1,5]; iris.trainlabels

#inspect result
print(iris.trainlabels)

#compose iris test labels
iris.testlabels<-iris[ind==2,5]

#inspect result
print(iris.testlabels)


#The use KNN model;
#i want to find k nearest neighbors of training set
#this we use knn() funtion, this function uses Euclidian distance
#build the model
iris_pred<-knn(train=iris.training, test=iris.test,cl=iris.trainlabels, k=3)

#inspect iris_pred
iris_pred

#Evaluation of the model
#essential step in machine learning is evaluating the perfomance of the mdoel
#putiris.testlabels in a data frame
irisTestLabels<- data.frame(iris.testlabels)

#merge iris_pred and iris.testLabels
merge<-data.frame(iris_pred, iris.testlabels)

#Specify column names for merge
names(merge)<-c('Predicted Species', 'Observed Species')

#inspect merge
merge

#further analysis
install.packages('gmodels' , dependencies = T)
library(gmodels)

#corss tab teh mdoel
CrossTable(x=iris.testlabels, y=iris_pred,prop.chisq = F)

#Machine Learnng in R with caret


 





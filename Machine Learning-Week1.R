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

#pick from training and training and test sets





#Step 1: Install and load libraries
#Installing libraries
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')
install.packages("RGtk2")
install.packages("e1071")

#Loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)
library(e1071)

rm(list=ls())

#Step 2: Import the data set
#Reading the data set as a dataframe
mushrooms <- read.csv ("mushrooms.csv")
view(mushrooms)
#Now, to display the structure of the data set, 
#you can make use of the R function called str():
# structure of the data
str(mushrooms)

#Step 3: Data Cleaning
# number of rows with missing values
nrow(mushrooms) - sum(complete.cases(mushrooms))

# deleting redundant variable `veil.type`
mushrooms$veil.type <- NULL

#Step 4: Data Exploration and Analysis
# analyzing the odor variable
table(mushrooms$class,mushrooms$odor)

number.perfect.splits <- apply(X=mushrooms[-1], MARGIN = 2, FUN = function(col){
  t <- table(mushrooms$class,col)
  sum(t == 0)
})

# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]

# Plot graph
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,
        main="Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,col="wheat")

#Step 5: Data Splicing
#data splicing
set.seed(12345)
train <- sample(1:nrow(mushrooms),size = ceiling(0.80*nrow(mushrooms)),replace = FALSE)
# training set
mushrooms_train <- mushrooms[train,]
# test set
mushrooms_test <- mushrooms[-train,]

# penalty matrix
penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)

#Step 6: Building a model
# building the classification tree with rpart
tree <- rpart(class~.,
              data=mushrooms_train,
              parms = list(loss = penalty.matrix),
              method = "class")

#Step 7: Visualising the tree
# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)

#Step 8: Testing the model
#Testing the model
pred <- predict(object=tree,mushrooms_test[-1],type="class")

#Step 9: Calculating accuracy
#Calculating accuracy
t <- table(mushrooms_test$class,pred) 
confusionMatrix(t) 



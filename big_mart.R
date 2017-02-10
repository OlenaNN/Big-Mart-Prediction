# Variable	Description
# Item_Identifier	Unique product ID
# Item_Weight	Weight of product
# Item_Fat_Content	Whether the product is low fat or not
# Item_Visibility	The % of total display area of all products in a store allocated to the particular product
# Item_Type	The category to which the product belongs
# Item_MRP	Maximum Retail Price (list price) of the product
# Outlet_Identifier	Unique store ID
# Outlet_Establishment_Year	The year in which store was established
# Outlet_Size	The size of the store in terms of ground area covered
# Outlet_Location_Type	The type of city in which the store is located
# Outlet_Type	Whether the outlet is just a grocery store or some sort of supermarket
# Item_Outlet_Sales	Sales of the product in the particulat store. This is the outcome variable to be predicted.
if(!require("ggthemes")) install.packages("ggthemes")
if(!require("mice")) install.packages("mice")
if (!require("reshape2")) install.packages("reshape2")
if (!require("Amelia")) install.packages("Amelia")
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("devtools")) install.packages("devtools")
# if (!require("car")) install.packages("car")#3d graph
# if (!require("rgl")) install.packages("rgl")#3d graph
if (!require("factoextra")) install.packages("factoextra")
if (!require("ade4")) install.packages("ade4")
library(ade4)#for PCA 
library(ggplot2) # visualization
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(mice) # imputation
library(randomForest) # classification algorithm
library(reshape2)
library(Amelia)#missing value visualization
library(dummies)
library(Hmisc)#for functions: imput,  bystats (returns a matrix with the sample size, number missing y, and fun(non-missing y)
library(plyr) # data manipulation
library("RColorBrewer")
library(car)#basic 3d graph

rm(list=ls())

#load train and test file
train_first<- read.csv("Train_Big.csv", na.strings = "NA")
test_first <- read.csv("Test_Big.csv")
train<-train_first
test<-test_first
summary(train)
summary(test)

list(train$Item_Identifier)
levels(train$Item_Identifier) = c(1:1559)

list(train$Outlet_Identifier)
levels(train$Outlet_Identifier) = c(1:10)

list(train$Item_Fat_Content)
train$Item_Fat_Content=as.character(train$Item_Fat_Content)
train$Item_Fat_Content[train$Item_Fat_Content=='low fat']<-'Low Fat'
train$Item_Fat_Content[train$Item_Fat_Content=='LF']<-'Low Fat'
train$Item_Fat_Content[train$Item_Fat_Content=='reg']<-'Regular'
train$Item_Fat_Content=as.factor(train$Item_Fat_Content)
levels(train$Item_Fat_Content)
#apply the impute function from the Hmisc package on a per-Item_Type basis 
#to assign the median of the available Weight to the missing Weight(s). 
Item_Type.na.train<-c("Baking Goods","Breads","Breakfast","Canned","Dairy",
                      "Frozen Foods","Fruits and Vegetables","Hard Drinks",
                      "Health and Hygiene","Household","Meat","Others","Seafood",
                      "Snack Foods","Soft Drinks","Starchy Foods")

bystats(train$Item_Weight, train$Item_Type, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))
  
imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])}
  return (impute.var)}
train$Item_Weight <- imputeMedian(train$Item_Weight, train$Item_Type, Item_Type.na.train)
summary(train)


g <- ggplot(data=train, aes(x=Outlet_Size, y=Item_Outlet_Sales))+geom_point(aes(color=Outlet_Type)) 
# Stacked bar plot
g 


table(train$Outlet_Size, train$Outlet_Type)
levels(train$Outlet_Size)
levels(train$Outlet_Size)[1] <- "Other"
str(train)


#Item_Visibility is equal to zero for some Item_Type. Let's repair this problem
# The helper function below (data_summary()) will be used to calculate the mean and 
# the standard deviation (used as error), for the variable of interest, in each group. 
# The plyr package is required.
data_summary <- function(data, varname, grps){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, grps, .fun=summary_func, varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
df <- data_summary(train, varname="Item_Visibility", grps= "Outlet_Type")
df


# fill by groups and change color manually
bp <- ggplot(train, aes(x=Outlet_Size, y=Item_Visibility))
bp <- bp + geom_boxplot(aes(fill = Outlet_Size))
bp+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#99FF66"))


p <- ggplot(train, aes(x=Outlet_Type, y=Item_Visibility))
p <- p + geom_boxplot(aes(fill = Outlet_Type))
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#99FF66"))

pp <- ggplot(train, aes(x=Item_Type, y=Item_Visibility))
pp <- pp + geom_boxplot(aes(fill = Item_Type))
pp+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#99FF66",
                              "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", 
                              "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC",
                              "#00FF00","#FFFF00","#FF3366","#000033"))
#impute 0 with median 
Outlet_Type.na.train<-c("Grocery Store","Supermarket Type1","Supermarket Type2","Supermarket Type3")
train$Item_Visibility[ which( train$Item_Visibility == 0 )] <- NA
bystats(train$Item_Visibility, train$Outlet_Type, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))

train$Item_Visibility <- imputeMedian(train$Item_Visibility, train$Outlet_Type, Outlet_Type.na.train)
summary(train)


train$Item_Identifier<-as.character(train$Item_Identifier)
train$Item_Identifier<-as.factor(train$Item_Identifier)

# We’ll convert these categorical variables into numeric using one hot encoding.
# #load library
#library(dummies)

#create a dummy data frame
new_my_data <- dummy.data.frame(train, names = c("Item_Fat_Content","Item_Type","Outlet_Identifier",
                                                   "Outlet_Establishment_Year","Outlet_Size",
                                                   "Outlet_Location_Type","Outlet_Type"))
str(new_my_data)

# Splitting the dataset into the Training set and Test set
if (!require("caTools")) install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(new_my_data, SplitRatio = 2/3)
training_set = subset(new_my_data, split == TRUE)
test_set = subset(new_my_data, split == FALSE)
activ_train<-training_set[,2:53]
# The package factoextra is used for the visualization of the principal
# component analysis results factoextra can be installed as follow :
library(factoextra)
res.pca <- dudi.pca(activ_train, scannf = FALSE, nf = 5)
#Returns a list of classes pca and dudi (see dudi) containing the used
#information for computing the principal component analysis :
summary(res.pca)
#eigenvalue
eig.val <- get_eigenvalue(res.pca)
head(eig.val)
#The function screeplot() can be used to represent the amount of inertia
#(variance) associated with each principal component
screeplot(res.pca, main ="Screeplot - Eigenvalues")
#You can also customize the plot using the standard barplot() function.
barplot(eig.val[, 2], names.arg=1:nrow(eig.val), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type="b", pch=19, col = "red")

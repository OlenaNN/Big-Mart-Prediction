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
# library(car)#basic 3d graph

rm(list=ls())

#load train and test file
train_first<- read.csv("Train_Big.csv", na.strings = "NA")
test_first <- read.csv("Test_Big.csv")
train<-train_first
test<-test_first
summary(train)
summary(test)
test$Item_Outlet_Sales <- 1
combi <- rbind(train, test)

list(combi$Item_Identifier)
levels(combi$Outlet_Identifier) = c(1:10)
list(combi$Item_Fat_Content)
combi$Item_Fat_Content=as.character(combi$Item_Fat_Content)
combi$Item_Fat_Content[combi$Item_Fat_Content=='low fat']<-'Low Fat'
combi$Item_Fat_Content[combi$Item_Fat_Content=='LF']<-'Low Fat'
combi$Item_Fat_Content[combi$Item_Fat_Content=='reg']<-'Regular'
combi$Item_Fat_Content=as.factor(combi$Item_Fat_Content)
levels(combi$Item_Fat_Content)
#apply the impute function from the Hmisc package on a per-Item_Type basis 
#to assign the median of the available Weight to the missing Weight(s). 
Item_Type.na.combi<-c("Baking Goods","Breads","Breakfast","Canned","Dairy",
                      "Frozen Foods","Fruits and Vegetables","Hard Drinks",
                      "Health and Hygiene","Household","Meat","Others","Seafood",
                      "Snack Foods","Soft Drinks","Starchy Foods")

bystats(combi$Item_Weight, combi$Item_Type, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))
  
imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])}
  return (impute.var)}
combi$Item_Weight <- imputeMedian(combi$Item_Weight, combi$Item_Type, Item_Type.na.combi)
summary(combi)


g <- ggplot(data=combi, aes(x=Outlet_Size, y=Item_Outlet_Sales))+geom_point(aes(color=Outlet_Type)) 
# Stacked bar plot
g 


table(combi$Outlet_Size, combi$Outlet_Type)
levels(combi$Outlet_Size)
levels(combi$Outlet_Size)[1] <- "Other"
str(combi)


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
df <- data_summary(combi, varname="Item_Visibility", grps= "Outlet_Type")
df


# fill by groups and change color manually
bp <- ggplot(combi, aes(x=Outlet_Size, y=Item_Visibility))
bp <- bp + geom_boxplot(aes(fill = Outlet_Size))
bp+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#99FF66"))


p <- ggplot(combi, aes(x=Outlet_Type, y=Item_Visibility))
p <- p + geom_boxplot(aes(fill = Outlet_Type))
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#99FF66"))

pp <- ggplot(combi, aes(x=Item_Type, y=Item_Visibility))
pp <- pp + geom_boxplot(aes(fill = Item_Type))
pp+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#99FF66",
                              "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", 
                              "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC",
                              "#00FF00","#FFFF00","#FF3366","#000033"))
#impute 0 with median 
Outlet_Type.na.combi<-c("Grocery Store","Supermarket Type1","Supermarket Type2","Supermarket Type3")
combi$Item_Visibility[ which( combi$Item_Visibility == 0 )] <- NA
bystats(combi$Item_Visibility, combi$Outlet_Type, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))

combi$Item_Visibility <- imputeMedian(combi$Item_Visibility, combi$Outlet_Type, Outlet_Type.na.combi)
summary(combi)

# We’ll convert these categorical variables into numeric using one hot encoding.
# #load library
#library(dummies)

#create a dummy data frame
new_my_data <- dummy.data.frame(combi, names = c("Item_Fat_Content","Item_Type",
                                                 "Outlet_Establishment_Year","Outlet_Size",
                                                 "Outlet_Location_Type","Outlet_Type"))#"Outlet_Identifier"))
str(new_my_data)

# Splitting the dataset into the Training set and Test set
# if (!require("caTools")) install.packages("caTools")
# library(caTools)
# set.seed(123)
# split = sample.split(new_my_data, SplitRatio = 0.8)
training_set = new_my_data[1:nrow(train),]
test_set = new_my_data[-(1:nrow(train)),]
activ_train<-subset(training_set, select = -c(Item_Outlet_Sales, Item_Identifier, Outlet_Identifier)) 
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
# Make the scree plot using the package factoextra
fviz_screeplot(res.pca, ncp=10)
# The coordinates of the variables on the factor map are :
# Column coordinates
head(res.pca$co)
# Graph of variables
s.corcircle(res.pca$co)
?s.corcircle()
fviz_pca_var(res.pca, col.var="steelblue")+
  theme_minimal()


# The squared coordinates of variables are called cos2. A high cos2 indicates a
# good representation of the variable on the principal component. In this case
# the variable is positioned close to the circumference of the correlation
# circle. A low cos2 indicates that the variable is not perfectly represented by
# the PCs. In this case the variable is close to the center of the circle. The
# cos2 of the variables are :
# relative contributions of columns
inertia <- inertia.dudi(res.pca, row.inertia = TRUE,
                        col.inertia = TRUE)
var.cos2 <- abs(inertia$col.rel/10000)
head(var.cos2)
# squared coordinates
head(res.pca$co^2)

fviz_pca_var(res.pca, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=55) + theme_minimal()
# The contributions can be printed in % as follow :
# absolute contribution of columns
var.contrib <- inertia$col.abs/100
head(var.contrib)

# It provides a list of matrices containing all the results for the active
# variables (coordinates, correlation between variables and axes, squared cosine
# and contributions).
var <- get_pca_var(res.pca)
names(var)
#Contributions of variables
head(var$contrib)
fviz_pca_var(res.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=50) + theme_minimal()
# The most important variables for a given PC can be visualized using the
# function fviz_pca_contrib()[factoextra package] : (factoextra >= 1.0.1 is
# required)
# Contributions of variables on PC1
fviz_pca_contrib(res.pca, choice = "var", axes = 1)
# Contributions of variables on PC2
fviz_pca_contrib(res.pca, choice = "var", axes = 2)
#The coordinates of the individuals on the factor maps can be extracted as follow :
# The row coordinates
head(res.pca$li)
prin_comp <- prcomp(activ_train, scale. = T)
names(prin_comp)
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

prin_comp$rotation
prin_comp$rotation[1:5,1:4]
dim(prin_comp$x)
biplot(prin_comp, scale = 0)
std_dev <- prin_comp$sdev
#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

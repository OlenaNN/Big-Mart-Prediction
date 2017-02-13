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
library(caret)
library(rattle)#fot tree visualization

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

#hypothesis: outlet_size is depend of outlet_location

levels(combi$Outlet_Size)
levels(combi$Outlet_Size[which(combi$Outlet_Type=="Grocery Store")])[1] <- "Small"
str(combi)

combi %>%
  filter(Outlet_Type== "Supermarket Type1")%>%
  filter(Outlet_Location_Type== "Tier 2")%>%
  filter(Outlet_Identifier!="7")
levels(combi$Outlet_Size[which(combi$Outlet_Type=="Supermarket Type1")])[1] <- "Medium"
str(combi) 

combi$Outlet_Size=as.character(combi$Outlet_Size)
combi$Outlet_Size=as.factor(combi$Outlet_Size)
levels(combi$Outlet_Size)
g <- ggplot(data=combi, aes(x=Outlet_Size, y=Item_Outlet_Sales))+geom_point(aes(color=Outlet_Type))
g 


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
bp+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))#"#99FF66"))


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

combi$Outlet_Establishment_Year2<-1
combi$Outlet_Establishment_Year2[combi$Outlet_Establishment_Year<1995]<-'1985-1994'
combi$Outlet_Establishment_Year2[combi$Outlet_Establishment_Year<2005&combi$Outlet_Establishment_Year>=1995]<-'1995-2004'
combi$Outlet_Establishment_Year2[combi$Outlet_Establishment_Year>=2005]<-'2005-2009'
as.factor(combi$Outlet_Establishment_Year2)
summary(combi$Outlet_Establishment_Year2)
# We’ll convert these categorical variables into numeric using one hot encoding.
# #load library
#library(dummies)

#create a dummy data frame
new_my_data <- dummy.data.frame(combi, names = c("Item_Fat_Content","Item_Type",
                                                 "Outlet_Establishment_Year2","Outlet_Size",
                                                 "Outlet_Location_Type","Outlet_Type"))#"Outlet_Identifier"))
str(new_my_data)


tr_set<-subset(new_my_data,select = -c(Item_Outlet_Sales, Item_Identifier, Outlet_Identifier,Outlet_Establishment_Year))
training_set = new_my_data[1:nrow(train),]
test_set = new_my_data[-(1:nrow(train)),]
activ_train<-subset(training_set, select = -c(Item_Outlet_Sales, Item_Identifier, Outlet_Identifier,Outlet_Establishment_Year)) 
# The package factoextra is used for the visualization of the principal
# component analysis results factoextra can be installed as follow :
install.packages(("FactoMineR"))
library(factoextra)
library(FactoMineR)
#The correlation between variables
cor.mat <- round(cor(activ_train),2)
head(cor.mat[, 1:6])

# install.packages("corrplot")
library("corrplot")
?corrplot
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.cex=0.5,tl.srt=85)

#Make a scatter plot matrix showing the correlation coefficients 
#between variables and the significance levels : the package PerformanceAnalytics 
#is required.
#install.packages("PerformanceAnalytics")
#library("PerformanceAnalytics")
#?chart.Correlation()
#chart.Correlation(activ_train[, 1:6], histogram=TRUE, pch=30)

#Center and scale the data
#activ_train.scaled <- scale(activ_train, center = TRUE, scale = TRUE)
#activ_train<-activ_train.scaled
res.pca <- PCA(activ_train, graph = FALSE)
print(res.pca)

eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])

fviz_screeplot(res.pca, ncp=30)


fviz_pca_var(res.pca)
# 1. Correlation matrix
res.cor <- cor(activ_train)
round(res.cor, 2)
# 2. Calculate eigenvectors/eigenvalues
res.eig <- eigen(res.cor)
res.eig

res.pca2 <- dudi.pca(activ_train, scannf = FALSE, nf = 10)
#Returns a list of classes pca and dudi (see dudi) containing the used
#information for computing the principal component analysis :
summary(res.pca2)
#eigenvalue
eig.val <- get_eigenvalue(res.pca2)
head(eig.val)
#The function screeplot() can be used to represent the amount of inertia
#(variance) associated with each principal component
screeplot(res.pca2, main ="Screeplot - Eigenvalues")
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
fviz_screeplot(res.pca2, ncp=26)
# The coordinates of the variables on the factor map are :
# Column coordinates
head(res.pca2$co)
# Graph of variables
s.corcircle(res.pca2$co)
?s.corcircle()
fviz_pca_var(res.pca2, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.4) + theme_minimal()

# The squared coordinates of variables are called cos2. A high cos2 indicates a
# good representation of the variable on the principal component. In this case
# the variable is positioned close to the circumference of the correlation
# circle. A low cos2 indicates that the variable is not perfectly represented by
# the PCs. In this case the variable is close to the center of the circle. The
# cos2 of the variables are :
# # relative contributions of columns The contributions of variables in
# accounting for the variability in a given principal component are (in
# percentage) : (variable.cos2 * 100) / (total cos2 of the component)
#Contributions of variables on PC1
# The red dashed line on the graph below indicates the expected average
# contribution. For a given component, an observation with a contribution larger
# than this cutoff could be considered as important in contributing to the
# component.
fviz_contrib(res.pca, choice = "var", axes = 1,font.tickslab=9)+
  theme(axis.text.x = element_text(angle=50))
?fviz_contrib()
# Contributions of variables on PC2
fviz_contrib(res.pca, choice = "var", axes = 2,font.tickslab=9)

# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "var", axes = 1:2,font.tickslab=9)
# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib",font.tickslab=9)
?fviz_pca_var()
# Change the gradient color
fviz_pca_var(res.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=6) + theme_minimal()

#The function dimdesc()[in FactoMineR] can be used to identify the most
#correlated variables with a given principal component.
res.desc<-dimdesc(res.pca,axes=c(1,2),proba=0.05)

# Description of dimension 1
res.desc$Dim.1

# Description of dimension 2
res.desc$Dim.2




?fviz_contrib()
head(res.pca2)
#The coordinates of the individuals on the factor maps can be extracted as follow :
# The row coordinates
head(res.pca2$li)
prin_comp <- prcomp(activ_train, scale. = T)
names(prin_comp)
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

prin_comp$rotation
prin_comp$rotation[1:5,1:4]
dim(prin_comp$x)
biplot(prin_comp, scale = 0)
?biplot()
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

train.data <- data.frame(Item_Outlet_Sales = train$Item_Outlet_Sales, prin_comp$x)

# Splitting the dataset into the Training set2 and Test set2
#we are interested in first 27 PCAs
train.data2<-train.data[1:5523,1:28]
test.data2<-training_set[5524:8523,]
#run a decision tree
# install.packages("rpart")
library(rpart)
rpart.model <- rpart(Item_Outlet_Sales ~ .,data = train.data2, method = "anova")
rpart.model
fancyRpartPlot(rpart.model)
test.data <- predict(prin_comp, newdata = test.data2)
test.data <- as.data.frame(test.data)

#select the first 27 components
test.data <- test.data[,1:27]
#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data)
str(rpart.prediction)
summary(rpart.prediction)
levels(rpart.prediction)
rpart.prediction
control<-train$Item_Outlet_Sales[5524:8523]


#check your solution 
control.data <- data.frame(rpart.prediction, control)
cov(control.data$rpart.prediction,control.data$control)
?var()

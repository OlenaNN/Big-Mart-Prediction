#From https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/

if(!require("dummies")) install.packages("dummies")
if(!require("rpart")) install.packages("drpart")
if(!require("rpart.plot")) install.packages('rpart.plot')
if(!require("rattle")) install.packages('rattle')
if(!require("RColorBrewer")) install.packages('RColorBrewer')
library(dummies)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
# Remember, PCA can be applied only on numerical data. Therefore, 
# if the data has categorical variables they must be converted to numerical. 
# Also, make sure you have done the basic data cleaning prior 
# to implementing this technique. 
# Let’s quickly finish with initial data loading and cleaning steps:


#load train and test file
train <- read.csv("Train_Big.csv")
test <- read.csv("Test_Big.csv")

#add a column
test$Item_Outlet_Sales <- 1

#combine the data set
combi <- rbind(train, test)

#impute missing values with median
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)

#impute 0 with median 
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),                                   combi$Item_Visibility)

#find mode and impute
table(combi$Outlet_Size, combi$Outlet_Type)
levels(combi$Outlet_Size)
levels(combi$Outlet_Size)[1] <- "Other"
str(combi)


# Now we are left with removing the dependent (response) variable and other 
# identifier variables( if any). As we said above, we are practicing an unsupervised 
# learning technique, hence response variable must be removed.

#remove the dependent and identifier variables
my_data <- subset(combi, select = -c(Item_Outlet_Sales, Item_Identifier,
                                     Outlet_Identifier))         

#check available variables
colnames(my_data)

# Since PCA works on numeric variables, let’s see if we have any variable 
# other than numeric.
#check variable class
str(my_data)

# We’ll convert these categorical variables into numeric using one hot encoding.
# #load library
#library(dummies)

#create a dummy data frame
new_my_data <- dummy.data.frame(my_data, names = c("Item_Fat_Content","Item_Type",
                                                     "Outlet_Establishment_Year","Outlet_Size",
                                                     "Outlet_Location_Type","Outlet_Type"))
str(new_my_data)

#divide the new data
pca.train <- new_my_data[1:nrow(train),]
pca.test <- new_my_data[-(1:nrow(train)),]

# The base R function prcomp() is used to perform PCA. 
# By default, it centers the variable to have mean equals to zero. 
# With parameter scale. = T, we normalize the variables 
# to have standard deviation equals to 1.

#principal component analysis
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)

# center and scale refers to respective mean and standard deviation 
# of the variables that are used 
# for normalization prior to implementing PCA
#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

prin_comp$rotation
prin_comp$rotation[1:5,1:4]
dim(prin_comp$x)
biplot(prin_comp, scale = 0)


#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]


# We aim to find the components which explain the maximum variance. 
# This is because, we want to retain as much information as possible 
# using these components. So, higher is the explained variance, 
# higher will be the information contained in those components.

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

# This shows that first principal component explains 10.3% variance. 
# Second component explains 7.3% variance. Third component explains 6.2% variance 
# and so on. So, how do we decide how many components should we select for 
# modeling stage ?
# The answer to this question is provided by a scree plot. 
# A scree plot is used to access components or factors which explains 
# the most of variability in the data. It represents values in descending order.

#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

# The plot above shows that ~ 30 components explains around 98.4% variance in the data set.
# In order words, using PCA we have reduced 44 predictors to 30 without compromising 
# on explained variance. This is the power of PCA> Let’s do a confirmation check, 
# by plotting a cumulative variance plot. This will give us a clear picture 
# of number of components.

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
#Let’s now understand the process of predicting on test data using these components. 

#add a training set with principal components
train.data <- data.frame(Item_Outlet_Sales = train$Item_Outlet_Sales, prin_comp$x)

#we are interested in first 30 PCAs
train.data <- train.data[,1:31]

#run a decision tree
# install.packages("rpart")
# library(rpart)
rpart.model <- rpart(Item_Outlet_Sales ~ .,data = train.data, method = "anova")
rpart.model
fancyRpartPlot(rpart.model)
#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)

#select the first 30 components
test.data <- test.data[,1:30]

#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data)

#For fun, finally check your score of leaderboard
sample <- read.csv("SampleSubmission_TmnO39y.csv")
final.sub <- data.frame(Item_Identifier = sample$Item_Identifier, Outlet_Identifier = sample$Outlet_Identifier, Item_Outlet_Sales = rpart.prediction)
write.csv(final.sub, "pca.csv",row.names = F)
rpart.prediction


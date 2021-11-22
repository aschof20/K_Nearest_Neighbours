#Import Libraries
library(ggpubr)
library(car) 
library(class)
library(psych)

###############################################################################
#                                DATA PROCESSING                              #
###############################################################################
wren.df = read.csv(file="Wren.csv", head=TRUE, sep=',')
attach(wren.df)

#Declare categorical variable as factor
survive = as.factor(survive)

###############################################################################
#                         EXPLORATORY DATA ANALYSIS                           #
###############################################################################
# Pairs plots of numerical variables.
pairs(wren.df[, -c(1)] ,lower.panel = panel.smooth, upper.panel = panel.cor)
# Refined selection of variables.
pairs(wren.df[, -c(1, 2, 5, 10)] ,lower.panel = panel.smooth, upper.panel = panel.cor)

# Correlation matrix
cor.prob(wren.df[2:10])

# All response-predictor variable boxplots
par(mfrow=c(3,3))
# Predictor variable vector
predictor.vector<- 2:10
# Vector of the predictor names
name.vector <- names(wren.df[2:10,])
# Loop to iterate over the predictor variables and produce boxplots
for(i in predictor.vector){
  x<- wren.df[,i]
  if(name.vector[i] != "weight"){
    Boxplot(x~wren.df$survive, ylab = paste(name.vector[i], " (mm)"), xlab = "Survival",  pch=19, col= "grey64", data =wren.df)
  }else{
    Boxplot(x~wren.df$survive, ylab = paste(name.vector[i], " (g)"), xlab = "Survival",  pch=19, col= "grey64", data =wren.df)
  }
}

# Refined response-predictor variable boxplots
par(mfrow=c(2,2))
predictor.vector<- c(2,4,9,10)
# Vector of the predictor names
name.vector <- names(wren.df[2:10,])
# Loop to iterate over the predictor variables and produce boxplots
for(i in predictor.vector){
  x<- wren.df[,i]
  if(name.vector[i] != "weight"){
    Boxplot(x~wren.df$survive, ylab = paste(name.vector[i], " (mm)"), xlab = "Survival",  pch=19, col= "grey64", data =wren.df)
  }else{
    Boxplot(x~wren.df$survive, ylab = paste(name.vector[i], " (g)"), xlab = "Survival",  pch=19, col= "grey64", data =wren.df)
  }
}
par(mfrow=c(1,1))

# Boxplot summary statistics
aggregate(length~survive, data = wren.df, summary)
aggregate(weight~survive, data = wren.df, summary)
aggregate(wskull~survive, data = wren.df, summary)
aggregate(lkeel~survive, data = wren.df, summary)

# 70:30 Training:Testing partition.
set.seed(911)

# The data was partitioned such that the training and test subsets are a 
# disjunctive union of the wren data-set. 
# Rows were randomly selected to avoid selection bias.

# 70% (rounded down) of the rows in the wren data-set were randomly selected.
dataPartition <- sample(nrow(wren.df),  floor(0.7*nrow(wren.df)), replace = FALSE)

# Create data.frame of the training subset
train <- wren.df[dataPartition,]

# Testing set dataframe
test <- wren.df[-dataPartition,]

###############################################################################
#                        K-NEAREST NEIGHBOURS MODEL                          #
###############################################################################
# Loop to obtain the optimal k
k.optm=1
for (i in 1:39){
  set.seed(1) # set seed for each iteration to consistently tie-break.
  knn.mod <- knn(train=train, test=test, cl=train$survive, k=i)
  k.optm[i] <- 100 * sum(test$survive == knn.mod)/NROW(test$survive)
  k=i
  # data.frame of k and k.optm tuples
  k_value<- as.data.frame(cbind(1:length(k.optm), k.optm))
}

# Plot of the prediction accuracy of k-values
plot(k.optm, type="b", xlab="K- Value",ylab="Prediction Accuracy (%)", pch=19)

# Int variable to identify the k associated with the max(k.optm)
get.optm = for(i in 1:NROW(k_value)){
  if(k_value[i, 2] == max(k.optm)){
    k = i
    print(k)
    return (k)
  }
}

# Confirm optimal result
set.seed(1)
knn.pred=knn(train, test, train$survive, k=k)

# Confusion matrix for testing data
table(knn.pred,test$survive)
# Accurately predicted
mean(knn.pred==test$survive)
# Error rate of knn model
mean(knn.pred!=test$survive)
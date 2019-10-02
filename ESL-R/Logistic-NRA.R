# Logistic Regression
# Newton-Raphson Algorithm
setwd("~/Documents/Programming/ESL-Practice")
sa <- read.table('SAheart.data.txt', sep = ',', header = 1)
# or 
#sa <- read.table("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data",
#                 sep=",",head=T,row.names=1)
sa <- sa[,c(-1,-5,-7)]

sa$chd <- as.factor(sa$chd)
str(sa)
sa_stddz <- apply(sa[,c(-4, -8)],2,scale)
sa_stddz <- cbind(sa_stddz,sa[,c(4,8)])

# Logistic regression -----------------------

# I. Binomial response logistic regression:
# glm()
# family：a description of the error distribution and link function to be used in the model
# common family：
# binomal(link=’logit’) —-response follows binomial distribution, link function is logit, which is logistic regression
# binomal(link=’probit’) —-response follows binomial distribution, link function is probit
# poisson(link=’identity’) —-response follows poission distribution 
# control: control the calculation error and maximal times of convergence 

# II. Multi-class response logistic regression:
# Ordered Logit Regression: polrb in package "MASS"
# Multinomial Logit Regression: multinom in package "nnet"
# Conditional logistic Regression: clogit in package "survival"

lr_all <- glm(chd ~ .,
           family = binomial(link = logit), data = sa)

summary(lr_all)

# logistic regression of standardized data
pairs(sa[,c(-4, -8)])

lr_stdd_all <- glm(chd ~ .,
              family = binomial(link = logit), data = sa_stddz)

summary(lr_stdd_all)

# stepwise logistic regression (both direction)
logit.step <- step(lr_all, direction = "both")
summary(logit.step)


# forward seletion
logit.step <- step(lr_all, direction = "forward")
summary(logit.step)

# backward selection
logit.step <- step(lr_all, direction = "backward")
summary(logit.step)

# L1 Regulized Logistic Regression
library(glmpath) # For the LASSO
require(glmnet)
library(lars) # Used for the LASSO
head(sa)
X <- sa[,-8]
# Make sure that family history is numeric
X$famhist <- as.numeric(X$famhist) 
# Ensure that the explanatory variables are in a matrix form
X <- as.matrix(X) 
# Ensure that the respones variable is numeric and in matrix form
y <- sa[,8]
y <- as.matrix(as.numeric(y)) 

SA_LARS <- glmpath(X, y, family=binomial, 
                   standardize = T, max.norm=100000*ncol(X), 
                   min.lambda = 0.1)

summary(SA_LARS)

# Standardized
X_st <- sa_stddz[,-8]
# Make sure that family history is numeric
X_st$famhist <- as.numeric(X_st$famhist) 
# Ensure that the explanatory variables are in a matrix form
X_st <- as.matrix(X_st) 
# Ensure that the respones variable is numeric and in matrix form
y_st <- sa_stddz[,8]
y_st <- as.matrix(as.numeric(y_st)) 

SA_LARS <- glmpath(X_st, y_st, family=binomial,
                   max.norm=100000*ncol(X), 
                   min.lambda = 0.1,trace = T)




data(heart.data)
attach(heart.data)
fit <- glmpath(x,y, family=binomial)
summary(fit)
detach(heart.data)
summary(SA_LARS)


# Next, visualize the model
par(oma=c(1,1,1,2)) # Change the outer margins to accomodate the error
plot(SA_LARS) # Plot the output of the graphs
plot(SA_LARS, plottype="Cp")
dev.off() # Turn the device off




# Reference from R-bloggers
require(data.table)
require(glmnet)
set.seed(123)
###reading data
housingData=fread('data/kc_house_data.csv')
##Removing non numeric var
housingData[,floors:=as.numeric(floors)][,c('zipcode','lat','long','id','date','sqft_basement'):=NULL]

###Splitting data
indexTrain=sample.int(nrow(housingData),nrow(housingData)*0.005)


##no Reg
lmNoReg=lm(price~.,housingData[indexTrain])
summary(lmNoReg)
pred_no_reg=predict(lmNoReg,housingData[-indexTrain])

sqrt(mean((pred_no_reg-housingData[-indexTrain]$price)^2))


plotCoeffEvolution=function(penalizedGlm,type='L1')
{
  require(ggplot2)
  lambda=penalizedGlm$lambda
  coeff=as.matrix(penalizedGlm$beta)
  rowName=rownames(coeff)
  coeff=data.table(coeff)
  coeff[,name:=rowName]
  coeff=melt(coeff,id.vars = 'name')
  coeff[,variable:=rep(lambda,each=length(unique(name)))]
  ggplot(coeff,aes(x=variable,y=value,color=name))+geom_line()+xlab(paste0(type,' regularisation'))+ylab('Value of coefficient')+scale_x_log10()
}

##Different L1 regularisation
fit = glmnet(as.matrix(housingData[indexTrain,-c('price'),with=F]),as.matrix(housingData[indexTrain]$price) , family="gaussian",alpha=1)

pred_L1_reg=data.table(predict(fit,as.matrix(housingData[-indexTrain,-c('price'),with=F])))
RMSE_L1=sqrt(apply(pred_L1_reg[,(.SD-housingData[-indexTrain]$price)^2,.SD=1:ncol(pred_L1_reg)],2,mean))
DF_plot=data.frame(lambda=fit$lambda,rmse=RMSE_L1)
ggplot(DF_plot,aes(x=lambda,y=rmse))+geom_line()+ggtitle("Evolution of test error vs lambda value")+scale_x_log10()
require(plotly)

## L2 regularisation
fit = glmnet(as.matrix(housingData[indexTrain,-c('price'),with=F]),as.matrix(housingData[indexTrain]$price) , family="gaussian",alpha=0)

pred_L2_reg=data.table(predict(fit,as.matrix(housingData[-indexTrain,-c('price'),with=F])))
RMSE_L2=sqrt(apply(pred_L2_reg[,(.SD-housingData[-indexTrain]$price)^2,.SD=1:ncol(pred_L2_reg)],2,mean))
DF_plot=data.frame(lambda=fit$lambda,rmse=RMSE_L2)
plotCoeffEvolution(fit,'L2')
require(ggplot2)
ggplot(DF_plot,aes(x=lambda,y=rmse))+geom_line()+ggtitle("Evolution of test error vs lambda value")+scale_x_log10()


##Different L1L2 regularisation
fit = glmnet(as.matrix(housingData[indexTrain,-c('price'),with=F]),as.matrix(housingData[indexTrain]$price) , family="gaussian",alpha=0.03)

pred_L1L2_reg=data.table(predict(fit,as.matrix(housingData[-indexTrain,-c('price'),with=F])))
RMSE_L1L2=sqrt(apply(pred_L1L2_reg[,(.SD-housingData[-indexTrain]$price)^2,.SD=1:ncol(pred_L1L2_reg)],2,mean))
DF_plot=data.frame(lambda=fit$lambda,rmse=RMSE_L1L2)
plotCoeffEvolution(fit,'Elastic')
require(ggplot2)
ggplot(DF_plot,aes(x=lambda,y=rmse))+geom_line()+ggtitle("Evolution of test error vs lambda value")+scale_x_log10()




# below to check
# Logistic Regression With LASSO
# Use logistic regression model to do classification, while choose predictor 
# variables using LASSO. Does the classification error decrease? 

# For this case, I am going to use L1 Regularized Regression.
# L1 regularized logistic regression is now a workhorse of
# machine learning: it is widely used for many classification
# problems, particularly ones with many features.


head(SA_newdata_post_PCA) # Review the previous data set


# To begin, we must define x and y in order to use glmpath
x <- SA_newdata_post_PCA[,c(2:5)] # Define the appropriate values of x (exclude chd)
x$PC1 <- as.numeric(x$PC1)
x$PC2 <- as.numeric(x$PC2)
x$PC3 <- as.numeric(x$PC3)
x$famhist <- as.numeric(x$famhist) # Make sure that family history is numeric
x <- as.matrix(x) # Ensure that the explanatory variables are in a matrix form

y <- SA_newdata_post_PCA[,1] # Let y by the first column of the matrix, the chd column
y <- as.numeric(y) # Ensure that the y value is numeric
y <- as.matrix(y) # Ensure that the respones variable is in matrix form

# Now, we train the model
SA_LARS = lars(x, y, type = "lasso")

# Next, we visualize the model
par(oma=c(1,1,1,2)) # Change the outer margins to accomodate the error
plot(SA_LARS) # Plot the output of the graphs
dev.off() # Turn the device off


# Plot the complexity parameter for the LASSO
png("c:/Users/Nate/Git/riemann/classification_PCA_lasso_cp.png")
plot(SA_LARS, plottype="Cp")
dev.off()


# By using cross-validation with the lasso, an optimal value for the "fraction" can be determined.
SA_LARS_cv <- cv.lars(x, y, type="lasso") # Note that a graph is generated here
SA_LARS_cv # Review the final object


SA_LARS_predict <- predict.lars(SA_LARS, newx=x, type="fit", mode="fraction", s=0.61)


# GAther together the predicted fitted values
SA_LARS_predict_fit <- as.matrix(SA_LARS_predict$fit)
SA_LARS_predict_fit # Review the created object


class.vec_LASSO <- ifelse(SA_LARS_predict_fit[,1]>(1 + .61), 1, 0) #Use the Cp parameter to
# convert the output into either ones or zeros ...
misclass_table_LASSO <- table(Actual=SA_newdata_post_PCA$chd, Classified=class.vec) # Generate a table using the predicted values


(misclass_table_LASSO <- as.matrix(misclass_table_LASSO)) #Turn the misclassification table into a matrix
(rate <- (misclass_table_LASSO[1,2] + misclass_table_LASSO[2,1]) / sum(misclass_table_LASSO)) # Calculate the misclassification rate)
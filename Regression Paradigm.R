#####Box-Cox Transformation with optimal Lamda###
df <- data.frame(x= c(7, 7, 8, 3, 2, 4, 4, 6, 6, 7, 5, 3, 3, 5, 8),y= c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 6, 7, 8))
# fitting the model
mod=lm(y ~ x,data=df)
summary(mod)
plot(mod)
plot(mod$residuals)
qqnorm(mod$residuals)
qqline(mod$residuals)
library(MASS)
bc=boxcox(y~x)
###Finding the optimal lambda value###
lambda=bc$x[which.max(bc$y)]
lambda
new_model=lm(((y^lambda-1)/lambda) ~ x)
new_model
new_model=lm(((y^lambda-1)/lambda) ~ x)
qqnorm(new_model$residuals)
qqline(new_model$residuals)
###############################################
#############################################
#####Box-Tidwell Transformation#####
##Calling the relevant R Library###
library(car)
data<-read_xlsx("C:/Users/DELL/Desktop/Datareg.xlsx")
###Feature Engineering for the model###
y<-data$y
x1<-data$x1
x2<-data$x2
x3<-data$x3
x4<-data$x4
x5<-data$x5
###Creating data in a matrix form###
data<-data.frame(x1,x2,x3,x4,x5,y)
pairs(data)
###Fitting the model#####
fittrial<-lm(y~x1+x2+x3+x4+x5)
###check the normality using residual plot###
plot(fittrial$residuals)
abline(0,0)
##Better understanding of the violation of normality assumption###
##using QQ Norm line technique###
qqnorm(fittrial$residuals)
qqline(fittrial$residuals)
##Checking which variables are creating trouble####
plot(x1,fittrial$residuals)
plot(x2,fittrial$residuals)
plot(x3,fittrial$residuals)
plot(x4,fittrial$residuals)
plot(x5,fittrial$residuals)
####Finally fitting the Model using Box-Tidwel Transformation###
fit1<-boxTidwell(y~x4+x5+x1,~x2+x3)
fit1
summary(fit1)
###Finding the lambda values of the variables which need to be transformed##
al1<-fit1$result[3,1]; al4<-fit1$result[1,1]; al5<-fit1$result[2,1]
X1 = x1^al1; X4 = x4^al4; X5 = x5^al5
###Finally fitting the transformed model###
fitfinal<-lm(y~X1+x2+x3+X4+X5)
par(mfrow=c(1,2))
plot(fittrial$residuals)
abline(0,0)
###Checking the residual plot of transformed variables##
plot(fitfinal$residuals)
abline(0,0)
####Residual analysis of old model###
qqnorm(fittrial$residuals)
qqline(fittrial$residuals)
####Residual plot of the new transformed model###
qqnorm(fitfinal$residuals)
qqline(fitfinal$residuals)
####################################################
####################################################
####################################################
####MULTICOLLINEARITY WITH RIDGE REGRESSION######
library(glmnet)
library(readxl)
library(car)
library(corrplot)
library(ggcorrplot)
library(rgl)
fish=read_excel("C:/Users/DELL/Downloads/Fish.xlsx")
fish
cor_matrix=cor(fish[,2:7])
print(cor_matrix)
corrplot(cor_matrix,method="color",tl.col="blue")
ggcorrplot(cor_matrix)
fit_lm=lm(fish$Weight~.,data=fish)
fit_lm
vif_values=vif(fit_lm)
vif_values
summary(vif_values)
plot(fit_lm$residuals,col="blue")
y=fish$Weight
x=fish[, c("Height", "Width", "Length1", "Length2", "Length3")]
ridge_fit=glmnet(x,y,alpha=0)
ridge_fit
summary(ridge_fit)
m=as.matrix(x)
cv_model=cv.glmnet(m, y, alpha = 0)
best_lambda=cv_model$lambda.min
best_lambda
plot(cv_model)
best_model=glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)
plot(ridge_fit, xvar = "lambda")
summary(cv_model)
#####Stepwise Regression Method###
fish_new=fish[,-1]
model1=lm(fish$Weight~.,data=fish_new)
summary(model1)
fish_new_step=step(model1,direction="backward")
summary(fish_new_step)
vif(fish_new_step)
vif(cv_model)
fish_imprv=lm(fish_new$Weight~fish$Length1+fish_new$Height,data=fish_new)
summary(fish_imprv)
vif(fish_imprv)
plot3d(fish_new$Weight,fish_new$Length1,fish_new$Height,col="red")
































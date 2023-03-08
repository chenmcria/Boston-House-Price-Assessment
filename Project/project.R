library(MASS)
library(leaps)
library(scatterplot3d)
library(ggplot2)
library(corrplot)
dd <- Boston
#check for missing values
sum(is.na(dd))
str(dd)
dd$chas <- as.factor(dd$chas)

#histogram of medv(response variable)
h <- hist(dd$medv, main="Histogram of MEDV", xlab="MEDV", ylab="Probablity", breaks = seq(from =5, to = 50, by = 1), col="darkblue", prob = TRUE)
h <- ggplot(dd,aes(x = medv))
h + geom_histogram(binwidth = 1,xlab="MEDV", ylab="Probablity",fill="blue",color="black",aes(y = ..density..))+ geom_density(size = 1)
#qq-plot
qqnorm(dd$medv, pch = 1, frame = FALSE)
qqline(dd$medv, col = "steelblue", lwd = 2)

# Outliers 
boxplot(dd$medv, ylab = "MEDV")


#new dataset after romoving the outliers
dd <- dd[dd$medv<50,]



#correlation matrix plot
corr_matrix<-cor(dd[,-4])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr_matrix, method="color",   
         type="upper",addCoef.col = "black"
)

#models
fit = lm(dd$medv~dd$lstat)
summary(fit)
fit2 = lm(dd$medv ~ dd$rm)
summary(fit2)

#plots of redisulas
plot(dd$medv~dd$lstat, xlab = "lstat", ylab = "medv", main = "MEDV vs. LSTAT")
plot(dd$medv~dd$rm, xlab = "rm", ylab = "medv", main = "MEDV vs. RM")


#full model (after removing the outliers)
fullmodel <- lm(medv~., data = dd)
summary(fullmodel)

#residual plot
plot(dd$medv,fullmodel$residuals, xlab = "MEDV", ylab = "Residuals", main = "Residuals vs. MEDV")
abline(h=0,col="red")


#reduced model1 (- indus - age - chas)
reduced1 <- lm(medv~.-indus-chas, data = dd)
summary(reduced1)

#reduced model1 residuals plot
plot(dd$medv,reduced1$residuals, xlab = "MEDV", ylab = "Residuals", main = "Residuals vs. MEDV")
abline(h=0,col="red")


#reduced model2 (-tax-rad-nox-indus-dis)
reduced2<- lm(medv~.-age-nox-indus-dis, data = dd)
summary(reduced2)

#reduced model2 residual plot
plot(dd$medv,reduced2$residuals, xlab = "MEDV", ylab = "Residuals", main = "Residuals vs. MEDV")
abline(h=0,col="red")

#model selection using mallows's cp statistic
sub1 <- regsubsets(medv~. ,data = dd, nbest = 1, nvmax = 28, method=c("exhaustive"))
summary(sub1)$which
cp = summary(sub1)$cp 
max(summary(sub1)$adjr2)
summary(sub1)$adjr2
new <- data.frame(cbind(1:length(cp),cp,summary(sub1)$adjr2))
new

#plot of cp vs p
plot(1:length(cp),cp, xlab = "p", ylab= "Cp")
abline(0,1)

#medv and lstat
plot(dd$medv ~ dd$lstat)
plot(dd$medv ~ log(dd$lstat), xlab = "log(lstat)", ylab = "medv", main = "Median Owner-Occupied Homes vs. Log(lstat)")
cor(log(dd$lstat), dd$medv)



#medv vs lstat
model9 <- lm(medv~ log(lstat), data = dd)
summary(model9)
plot(log(dd$lstat)~dd$medv)
plot(dd$medv, model9$residuals)


#further discussion (models)
furthermodel1 <- lm(medv~. + I(lstat^2), data = dd) 
summary(furthermodel1)

furthermodel2 <- lm(medv~. -indus-chas + I(lstat^2), data = dd) 
summary(furthermodel2)

furthermodel3 <- lm(medv~. -indus-chas+ log(lstat), data = dd) #largest R^2
summary(furthermodel3)

furthermodel4 <- lm(medv~. + log(lstat), data = dd) 
summary(furthermodel4)

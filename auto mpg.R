raw <- read.table("auto-mpg.data",  header = FALSE)
dataset <- subset(raw, select= -c(V9))
colnames(dataset) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration","model year", "origin")

library(dplyr)
glimpse(dataset)

unique(dataset$cylinders)
dataset$cylinders <- factor(dataset$cylinders,
                         levels=c(8, 4, 6, 3, 5),
                         labels=c(8, 4, 6, 3, 5))

unique(dataset$origin)
dataset$origin <- factor(dataset$origin,
                            levels=c(1, 2, 3),
                            labels=c(1, 2, 3))

unique(dataset$`model year`)
dataset$`model year` <- cut(dataset$`model year`, 4, labels =c(0, 1, 2, 3))

library(ggplot2)
library(car)
scatterplot.matrix(mpg~., data=dataset)
library(purrr)
library(tidyr)
library(ggplot2)

dataset %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

library(tidyverse)
library(GGally)
pairs(~., data= dataset, upper.panel= panel.smooth)

for (i in c(1,3,5,6)){
  boxplot(dataset[i])
}


dataset1 <- dataset[-c(which(dataset$horsepower == '?')), ]
unique(dataset1$horsepower)

library(hablar)
dataset1 <- dataset1 %>%
  convert(num(horsepower))
glimpse(dataset1)

model1 <- lm(mpg~., data= dataset1)
summary(model1)

dataset2 <- subset(dataset1, select = -c(acceleration))

model2 <- lm(mpg~., data= dataset2)
summary(model2)
plot(model2)

# Measure of Influence Point
library(olsrr)
# Cook's D Plot
ols_plot_cooksd_bar(model2) 
# DFBETAS Plot
ols_plot_dfbetas(model2)
#DFFITS Plot
ols_plot_dffits(model2) 
# Studentized Residual Plot
ols_plot_resid_stud(model2)

shapiro.test(dataset2$mpg)


#check for multicollinearity
#for vif calculation
car::vif(model2)

---------------------------------------------------------
#dont run
dummyknot= rep(0, length(dataset2$horsepower))
dummyknot[dataset2$horsepower>50]=1
diff <- dataset2$horsepower-50
diff * dummyknot
#------------------------------------------------------------
library(lmridge)
model3 <- lmridge(mpg ~. , dataset2, K= seq(0,0.10, 0.002))
plot(model3, type="ridge") #k= 0.004

model4 <- lmridge(mpg ~. , dataset2, K= 0.004)
#Future Code for checking best subset of the data
library(olsrr)
ols_step_best_subset(model2)

#pcr
xnew= subset(dataset2, select= -c(mpg,cylinders,`model year`, origin))
pcr=prcomp(xnew,center = T,scale=T)
plot(pcr,type='l')
summary(pcr)

biplot(pcr)
# Variability of each principal component: pr.var
pr.var <- pcr$sdev^2

# Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

#using pcr model
pcr_df <- data.frame(pcr$x)
dataset3 <- cbind(pcr_df, dataset2$mpg, dataset2$cylinders, dataset2$`model year`, dataset2$origin)
colnames(dataset3) <- c('PC1', 'PC2', 'PC3', 'mpg', 'cylinders', 'model_year', 'origin')
dataset3 <- subset(dataset3, select= -c(PC3))
model5 <- lm(mpg ~ ., data= dataset3)
summary(model5)

car::vif(model5)



#heteroscedasticity
#After Ridge Regression
plot(dataset2$mpg,residuals(model4))
abline(h=0)
plot(dataset2$displacement,residuals(model4))
abline(h=0)
plot(dataset2$horsepower,residuals(model4))
abline(h=0)
plot(dataset2$weight,residuals(model4))
abline(h=0)

#After PCR Regression

plot(dataset3$mpg,residuals(model5))
abline(h=0)
plot(dataset3$PC1,residuals(model5))
abline(h=0)
plot(dataset3$PC2,residuals(model5))
abline(h=0)

# Glejser's test
are <- abs(residuals(model4))

summary(lm(are~PC1, data= dataset3))
summary(lm(are~(1/PC1), data= dataset3))
summary(lm(are~sqrt(PC1), data= dataset3))
summary(lm(are~(1/sqrt(PC1)), data= dataset3))

summary(lm(are~PC2, data= dataset3))
summary(lm(are~(1/PC2), data= dataset3))
summary(lm(are~sqrt(PC2), data= dataset3))
summary(lm(are~(1/sqrt(PC2)), data= dataset3))


# Ridge Regression
#Goldfeld-Quandt test
library(lmtest)
gqtest(model4,fraction=1/3,order.by = ~ I(1/dataset3$PC1),alternative = c("less"))
gqtest(model4,fraction=1/3,order.by = ~ I(1/sqrt(dataset3$PC2)),alternative = c("less"))

#Autocorrelation
#et vs et-1
e=residuals(model4)
plot(e[-length(e)],e[-1],xlab="et -1", ylab="et")
cor(e[-length(e)],e[-1]) #0.25749
#Durbin-Watson test
library(lmtest)
dwtest(model4,alternative = c("two.sided")) #8.78e-08
dwtest(model4)  #4.39e-08

tss=sum((dataset2$mpg-mean(dataset2$mpg))^2)
r_sq_pred= function(m) {
  1-sum((press.lmridge(m))^2)/tss
}
r_sq_pred(model4) #85.34%




# PCR Regression

#Autocorrelation
e=residuals(model5)
plot(e[-length(e)],e[-1],xlab="et -1", ylab="et")
cor(e[-length(e)],e[-1]) #0.2942331
#Durbin-Watson test
library(lmtest)
dwtest(model5,alternative = c("two.sided")) #1.015e-09
dwtest(model5) # 5.075e-10

tss=sum((dataset2$mpg-mean(dataset2$mpg))^2)
r_sq_pred= function(m) {
  1-sum((press.lmridge(m))^2)/tss
}
r_sq_pred(model4)  #0.8533822

#PRESS
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

#R-sq predicted
pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}

pred_r_squared(model5) # 0.8418422
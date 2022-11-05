library(haven)
library(dplyr)
library(ggplot2)
library(lme4)
library(nlme)
library(car)
library(merTools)
library(tidyr)


#THIS FILE IS A TEST IF GEE MODELS ARE THE SAME AS WHAT HE CALLS A MUTIVARIATE LINEAR MODEL, BELOW I RUN THE SAME MODEL AS HIM ON THE GROWTH DATA AND GET THE SAME(ALMOST) RESULTS,
#I THINK THE DIFFERENCE IS THE TYPE OF CORRELATION STRUCTURE HE USES LEADS TO SLIGHTLY LARGET CONFIDENCE INTERVALS, NEVERTHELESS THE ESTIMATES ARE THE SAME

library(mice)
df<-potthoffroy


df$id<-as.factor(df$id)
data_long <- gather(df, age, measurement, d8:d14, factor_key=TRUE)
data_long$age<-as.character(data_long$age)
data_long[data_long=="d8"]<-"8"
data_long[data_long=="d10"]<-"10"
data_long[data_long=="d12"]<-"12"
data_long[data_long=="d14"]<-"14"



mf <- formula(measurement ~ 0+ age:sex)
gee1 <- geeglm(mf, data=data_long, id=id)#, corstr="unstructured")
mean(sqrt(resid(gee1)^2))
gee1
coef(gee1)
vcov(gee1)
summary(gee1)
coef(summary(gee1))
hist(resid(gee1),breaks = 50)
shapiro.test(resid(gee1))
acf(resid(gee1))
anova(gee1)
plot(gee1)


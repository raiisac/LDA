library(haven)
library(tidyverse)

trichotomization <- c(-100, 6, 25, 120)
data <- read_sas("E:/Downloads/hearing500lr.sas7bdat") %>%
  mutate(side = as.factor(side),
         id = as.factor(id),
         age_measurement = age + TIME,
         age_discrete = cut(age,
                            breaks = c(0,30,50,70,100),
                            labels = c("<30", "30-50", "50-70",">70")),
         y_discrete = cut(y,
                          breaks = trichotomization,
                          labels = c("Excellent", "Normal", "Hearing loss")),
         y_discrete_nb = cut(y,
                             breaks = trichotomization,
                             labels = c(3, 2, 1)),
         learning = 1*(TIME == 0))
mediansplit <- cut_number(data$y, n = 3)





library(gmodels)

CrossTable(data1$age_discrete,data1$y_discrete, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

fit$robust.variance

data1<-data[order(data$id, data$TIME),]


library("multgee")

fit <- ordLORgee(formula = y_discrete ~ age+ I(age^2) + TIME+learning,
                 link = "logit", id = id, data = data1,
                 LORstr = "time.exch")#category.exch"
summary(fit)


#fit1 <- update(fit, formula = ~. + I(TIME^2))
#summary(fit1)
#waldts(fit, fit1)


plot(x=(data1$age+data1$TIME),y=fit[["fitted.values"]][,1],col="blue",ylim = range(0,1))
points(x=(data1$age+data1$TIME),y=fit[["fitted.values"]][,2],col="red")
points(x=(data1$age+data1$TIME),y=fit[["fitted.values"]][,3],col="green")



fit[["fitted.values"]][,1]+fit[["fitted.values"]][,2]+fit[["fitted.values"]][,3]


library("ordinal")

fmm1 <- clmm(y_discrete ~ age+ TIME+learning+(1|id), data = data1,link="logit")	

summary(fmm1)


library(ggplot2)
plot(x=data1$id,y=fmm1$fitted.values)

data1<-cbind(data1,fmm1$fitted.values)

data1$id<-as.numeric(data1$id)


ggplot(data1, aes(x=(age+TIME),y=`fmm1$fitted.values`,group = interaction(y_discrete, id),color=y_discrete))+
  geom_line()+
  scale_color_manual(values = c("blue","red","green"))




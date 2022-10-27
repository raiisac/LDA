library(haven)
library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(merTools)

df <- read_sas("E:/Downloads/hearing500lr.sas7bdat")

df <- df %>% mutate(age_true = age + TIME)
df$id <- as.factor(df$id)
df_left <- df %>% filter(side == "left")
df_right <- df %>% filter(side == "right")

ggplot(data = df_left, aes(x = age_true, y = y, color = id)) +
  labs(title = "Left ear")+
  geom_point() +
  geom_line(
    stat = "smooth",
    method = "lm",
    formula = y ~ x,
    size = 1.2,
    alpha = 0.5) +
  ylim(-12, 75) +
  guides(color = "none") +
  theme_bw()


ggplot(data = df_right, aes(x = age_true, y = y, color = id)) +
  labs(title = "Right ear")+
  geom_point(alpha = 0.5) +
  geom_line(
    stat = "smooth",
    method = "lm",
    formula = y ~ x,
    size = 1.2,
    alpha = 0.5) +
  ylim(-12, 75) +
  guides(color = "none") +
  theme_bw()


db_lm = lm(y ~ age_true +id, data = df)
summary(db_lm)
plot(db_lm$residuals, type = "l")
plot(db_lm)
hist(db_lm$residuals,breaks=50)
shapiro.test(db_lm$residuals)
acf(db_lm$residuals)
anova(db_lm)
vcov(db_lm)
lh.out <- linearHypothesis(db_lm, hypothesis.matrix = c("age_true = 0","sideright=0"))
lh.out 

db_lm <-lm(y ~ age_true +side , data = df)
summary(db_lm)
plot(db_lm$residuals, type = "l")
plot(db_lm)
hist(db_lm$residuals,breaks=50)
shapiro.test(db_lm$residuals)
acf(db_lm$residuals)



db_2sls_1<-lm(age_true~id,data = df)
summary(db_2sls_1)

age_hat<-fitted.values(db_2sls_1)
age_hat<-as.factor(age_hat)

db_2sls_2<-lm(y~TIME+age_hat,data = df)
summary(db_2sls_2)








db_mixed = lmer(y ~ age_true + (1 | id), data = df)
summary(db_mixed)
plot(db_mixed)
hist(residuals(db_mixed), breaks = 50)
plot(residuals(db_mixed), type = "l")
shapiro.test(residuals(db_mixed))
acf(residuals(db_mixed))
confint(db_mixed)
ranef(db_mixed)$id %>% head(5)
coef(db_mixed)$id %>% head(5)
REsim(db_mixed)
plotREsim(REsim(db_mixed))


db_mixed2 = lmer(y ~ age_true + side + (1 | id), data = df)
summary(db_mixed2)
plot(db_mixed2)
hist(residuals(db_mixed2), breaks = 50)
plot(residuals(db_mixed2), type = "l")
shapiro.test(residuals(db_mixed2))
acf(residuals(db_mixed2))
confint(db_mixed2)
ranef(db_mixed2)$id %>% head(5)
coef(db_mixed2)$id %>% head(5)
REsim(db_mixed2)
plotREsim(REsim(db_mixed2))



db_mixed3 = lmer(y ~ age_true + side + (1 + side | id), data = df)
summary(db_mixed3)
plot(db_mixed3)
hist(residuals(db_mixed3), breaks = 50)
plot(residuals(db_mixed3), type = "l")
shapiro.test(residuals(db_mixed3))
acf(residuals(db_mixed3))
confint(db_mixed3)
ranef(db_mixed3)$id %>% head(5)
coef(db_mixed3)$id %>% head(5)
REsim(db_mixed3)
plotREsim(REsim(db_mixed3))




library(fitdistrplus)
library(car)
library(MASS)
test<-df$y+13
descdist(test)
qqp(test, "norm")
qqp(test, "lnorm")

nbinom <- fitdistr(test, "Negative Binomial")
qqp(test, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

poisson <- fitdistr(test, "Poisson")
qqp(test, "pois", lambda=poisson$estimate)

gamma <- fitdistr(test, "gamma")
qqp(test, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])


library(fitdistrplus)
beta <- fitdist(test/100, "beta",method = "mle")
plot(beta, las = 1)



community_count<-df %>% group_by(id) %>% 
  sample_n_groups(round(length(unique(df$id)))) %>% 
  count(id, sort=TRUE) %>% filter(n==max(n))




db_lm = lm(y ~ age_true +id, data = df)
summary(db_lm)
plot(db_lm$residuals, type = "l")
plot(db_lm)
hist(db_lm$residuals,breaks=50)
shapiro.test(db_lm$residuals)
acf(db_lm$residuals)
anova(db_lm)
vcov(db_lm)
lh.out <- linearHypothesis(db_lm, hypothesis.matrix = c("age_true = 0","sideright=0"))
lh.out 



library(nlme)



#df_grouped<-groupedData(y~age_true|id,
                        #data = df,
                        #order.groups=FALSE)

res.list <- lmList(y ~ age_true|side/id, data=df)
plot(augPred(res.list, grid=TRUE))
b <- lapply(res.list, coef)
V <- lapply(res.list, vcov)
estm <- rep(c("intercept","slope"), length(b))
subj <- rep(names(b), each=3)

library(metafor)
b <- unlist(b)
V <- bldiag(V)
res1 <- rma.mv(b ~ estm - 1, V, random = ~ estm | subj, struct="UN")
res1


res2 <- lme(y ~ age+TIME, random = ~ age+ TIME | id, data=df)
summary(res2)






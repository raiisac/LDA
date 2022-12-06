library(gee)
library(geepack)
library(multgee)
library(haven)
library(tidyverse)
library(lme4)
library(multgee)

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
         learning = 1*(TIME == 0))
mediansplit <- cut_number(data$y, n = 3)

data$y_discrete<-factor(data$y_discrete,levels = c( "Hearing loss", "Normal","Excellent"),order = TRUE)

data<-data %>% mutate(y_discrete_loss = ifelse(y_discrete == "Hearing loss", 1, 0),
                      y_discrete_Normal = ifelse(y_discrete == "Normal", 1, 0),
                      y_discrete_Excellent= ifelse(y_discrete == "Excellent", 1, 0),
                      age_round=round(age_measurement))

data_left <- data %>% filter(side == "left")
data_right <- data %>% filter(side == "right")



gee_Excellent <- gee(y_discrete_Excellent ~ age+TIME+learning,
               data = data, 
               id = id, 
               family = binomial,
               corstr = "independence")

summary(gee_Excellent)


exp(summary(gee_Excellent)[["coefficients"]][2,1])



gee_Normal <- gee(y_discrete_Normal ~ age+TIME+learning,
                     data = data, 
                     id = id, 
                     family = binomial,
                     corstr = "independence")

summary(gee_Normal)


exp(summary(gee_Normal)[["coefficients"]][3,1])


gee_loss <- gee(y_discrete_loss ~ age+TIME,
                  data = data, 
                  id = id, 
                  family = binomial,
                  corstr =  "unstructured")

summary(gee_loss)

QIC(gee_loss)[2]


exp(summary(gee_loss)[["coefficients"]][,1])



mf <- formula(y_discrete_Normal ~ age+TIME+learning)
gee_loss  <- geeglm(mf, data=data, id=id,family = binomial, corstr="exchangeable")
#MuMIn::QIC(gee_loss)
geepack::QIC(gee_loss)

summary(gee_loss)
exp(summary(gee_loss)[["coefficients"]][,1])


glmm_loss<- glmer(y_discrete_Normal ~ age+TIME+( 1 | id),
                  data = data, family = binomial)

summary(glmm_loss)

data <- data %>%
  mutate(pred_values_glmm_loss = predict(glmm_loss),
         pred_values_glmm_loss_response = predict(glmm_loss, type = "response"),
         pred_values_gee_loss_response = predict(gee_loss, type = "response"))

ggplot(data, aes(x = TIME, y = pred_values_glmm_loss)) +
  geom_line(aes(group = id), linetype = 2) +
  theme_bw(base_size = 16) +
  xlab("TIME") +
  ylab("Predicted Values")

ggplot(data, aes(x = TIME+age, y = pred_values_glmm_loss)) +
  geom_line(aes(group = id), linetype = 2) +
  theme_bw(base_size = 16) +
  xlab("Age+Time") +
  ylab("Predicted Values")

ggplot(data, aes(x = TIME+age, y = pred_values_glmm_loss_response)) +
  geom_line(aes(group = id), linetype = 2) +
  theme_bw(base_size = 16) +
  xlab("Age+Time") +
  ylab("Predicted Values")


ggplot(data, aes(x = TIME+age, y = pred_values_gee_loss_response)) +
  geom_line(aes(color = age)) +
  theme_bw(base_size = 16) +
  xlab("Age+TIME") +
  ylab("Probability of hearing loss")



data_loss_glmm <- data %>%
  group_by(age_round) %>%
  summarise(prob = mean(pred_values_glmm_loss_response))

data_loss_gee <- data %>%
  group_by(age_round) %>%
  summarise(prob = mean(pred_values_gee_loss_response))

data_agg = bind_rows(
  mutate(data_loss_glmm, model = "GLMM"),
  mutate(data_loss_gee, model = "GEE")
)

ggplot(data_agg, aes(x = age_round, y = prob)) +
  geom_line(aes(color = model, linetype = model), size = 1) +
  theme_bw(base_size = 16) + #xlim(0,90)+
  xlab("Age") +
  ylab("Prob of hearing Loss")

data1<-data

mf <- formula(y_discrete ~ age+TIME)
gee_loss  <- ordgee(mf, data=data, id=id, 
                    corstr="exchangeable",
                    int.const = TRUE,
                    mean.link = "logit",
                    corstr = "independence")
predict(gee_loss, type = "response")

summary(glmm_loss)

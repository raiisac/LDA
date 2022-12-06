library(tidyverse)
library(haven)
library(rprojroot)
trichotomization <- c(-100, 6, 25, 120)
data <- read_sas(find_root_file("data/hearing500lr.sas7bdat",
                                criterion = has_file("LDA.Rproj"))) %>%
  mutate(side = as.factor(side),
         side_integer =as.integer(side),
         id = as.factor(id),
         id_integer = as.integer(id),
         age_measurement = age + TIME,
         age_scale = unname(scale(age, center = TRUE, scale = TRUE)),
         age_discrete = cut(age,
                            breaks = c(0,30,50,70,100),
                            labels = c("<30", "30-50", "50-70",">70")),
         y_discrete = factor(as.factor(cut(y,
                          breaks = trichotomization,
                          labels = c("Excellent", "Normal", "Hearing loss"))),
                          levels = c("Excellent", "Normal", "Hearing loss"),
                          ordered = TRUE),
         y_integer = as.integer(y_discrete),
         learning = 1*(TIME == 0)) %>%
  arrange(id)
#---------------------------------GEE models-----------------------------------#
library(geepack)
geemod <- ordgee(y_discrete ~ age*TIME + learning + I(age^2),
                 data = data, id = id,
                 int.const = TRUE,
                 mean.link = "logit",
                 corstr = "unstructured"
                 )
summary(geemod)
#---------------------------Random effects models------------------------------#
library(ordinal)
remod <- clmm(as.factor(y_integer) ~ age*TIME + learning + I(age^2) +
               (1 |id_integer), #doesn't work with random slopes
             data = data)
summary(remod)
agecat <- c(20,30,40,50,60,70)
nDF <- expand.grid(age = agecat,
                   TIME = seq(0, 22, length.out = 60)) %>%
  mutate(learning = (TIME == 1),
         `I(age^2)` = age^2,
         `age:TIME` = age*TIME)

pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
  Theta <- c(-1000, theta, 1000)
  sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))}


predictions <- fake.predict.clmm(remod, newdata = nDF)


library(MPDiR)
polmer(y_discrete ~ age*TIME + learning + I(age^2) +
         (1 |id),
       data = data)

library(GLMMadaptive)
cr_vals <- cr_setup(data$y_discrete)
cr_data <- data[cr_vals$subs, ]
cr_data$y_new <- cr_vals$y
cr_data$cohort <- cr_vals$cohort

mm <- GLMMadaptive::mixed_model(y_new ~ cohort + age_scale*TIME + learning + I(age_scale^2),
                                data = cr_data,
            random = ~1 + TIME|id,
            family = binomial())
mm

scale2sd <- function(ref, scale_by, variable) {
  ((ref[[variable]]) - mean(scale_by[[variable]], na.rm = TRUE)) / (2 * sd(scale_by[[variable]], na.rm = TRUE))
}
agecat <- c(20,30,40,50,60,70)
centered_ages <- scale(agecat,
                       center = attr(data$age_scale,"scaled:center"),
                       scale = attr(data$age_scale,"scaled:scale"))[,1]

nDF <- with(cr_data, expand.grid(cohort = levels(cohort), learning = 0,
                                 age_scale = centered_ages,
                                 #`I(age_scale^2)` = centered_ages^2,
                                 TIME = seq(0, 22, length.out = 60)))
plot_data <- effectPlotData(mm, nDF)
expit <- function (x) exp(x) / (1 + exp(x))
my_panel_bands <- function(x, y, upper, lower, fill, col, subscripts, ..., font,
                           fontface) {
  upper <- upper[subscripts]
  lower <- lower[subscripts]
  panel.polygon(c(x, rev(x)), c(upper, rev(lower)), col = fill, border = FALSE, ...)
}
library(lattice)
xyplot(expit(pred) ~ TIME |factor(as.factor(age_scale),
                                  labels = paste0("age = ", agecat)),
       group = paste(cohort), data = plot_data,
       upper = expit(plot_data$upp), low = expit(plot_data$low), type = "l",
       panel = function (x, y, ...) {
         panel.superpose(x, y, panel.groups = my_panel_bands, ...)
         panel.xyplot(x, y, lwd = 2,  ...)
       }, xlab = "Follow-up time", ylab = "Continuation Ratio Probabilities")

#Marginal probabilities
plot_data_m <- effectPlotData(mm, nDF, CR_cohort_varname = "cohort",
                              direction = "forward") #does not work

key <- list(space = "top", rep = FALSE,
            text = list(levels(data$y_discrete)[1:2]),
            lines = list(lty = c(1, 1), lwd = c(2, 2), col = c("#0080ff", "#ff00ff")),
            text = list(levels(data$y_discrete)[3]),
            lines = list(lty = c(1), lwd = c(2), col = c("darkgreen")))

xyplot(expit(pred) ~ TIME | factor(as.factor(age_scale),
                                   labels = paste0("age = ", agecat)),
       group = ordinal_response, data = plot_data_m,
       upper = expit(plot_data_m$upp), low = expit(plot_data_m$low), type = "l",
       panel = function (x, y, ...) {
         panel.superpose(x, y, panel.groups = my_panel_bands, ...)
         panel.xyplot(x, y, lwd = 2, ...)
       }, xlab = "Follow-up time", ylab = "Marginal Probabilities", key = key)

#to marginalize over the random effects as well you will need to set the marginal argument of effectPlotData() to TRUE, e.g.,
plot_data_m2 <- effectPlotData(mm, nDF, CR_cohort_varname = "cohort",
                               direction = "forward", marginal = TRUE,
                               cores = 2) #does not work

xyplot(expit(pred) ~  TIME | factor(as.factor(age_scale),
                                    labels = paste0("age = ", agecat)),
       group = ordinal_response, data = plot_data_m2,
       upper = expit(plot_data_m2$upp), low = expit(plot_data_m2$low), type = "l",
       panel = function (x, y, ...) {
         panel.superpose(x, y, panel.groups = my_panel_bands, ...)
         panel.xyplot(x, y, lwd = 2,  ...)
       }, xlab = "Follow-up time",
       ylab = "Marginal Probabilities\nalso w.r.t Random Effects",
       key = key)

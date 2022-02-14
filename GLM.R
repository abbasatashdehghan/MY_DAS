library(ggplot2)
library(tidyverse)
library(GGally)
library(dplyr)
library(readr)
library(Stat2Data)
data(MedGPA)
#################################################################

bollywood <-
  read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/bollywood_boxoffice.csv"))
head(bollywood)
bollywood %>% ggplot(aes(y = Gross, x = Budget)) + 
  geom_point(col = "#66a61e") +
  scale_x_continuous("Budget") + scale_y_continuous("Gross")

bollywood %>% ggplot(aes(y = log(Gross), x = log(Budget))) + 
  geom_point(col = "#66a61e") +
  scale_x_continuous("Budget") + scale_y_continuous("Gross")

bol.lm <- lm(log10(Gross) ~ log10(Budget), data = bollywood)

summary(bol.lm)

bollywood %>% ggplot(aes(y = log10(Gross), x = log10(Budget))) + 
  geom_point(col = "#66a61e") +
  scale_x_continuous("Budget") + scale_y_continuous("Gross") + 
  geom_smooth(method = "lm", se = F, col = "#e7298a")

newdata <- data.frame(Budget = c(10, 50, 100))

predict(object = bol.lm, newdata =newdata )

###################
head(MedGPA)

MedGPA %>% ggplot(aes(GPA, Acceptance)) + 
  geom_jitter(width = 0, height = 0.01, alpha = 0.5, col = "#984ea3") + 
  geom_smooth(method = "lm", se = F, col =  "#984ea3", fullrange = TRUE)

mde.lm <- MedGPA %>% lm(data = ., Acceptance ~ GPA)
mde.lm

# GLM
med.glm <- MedGPA %>% glm(data = ., Acceptance ~ GPA, family = binomial)
med.glm
vcov(med.glm)
summary(med.glm, correlation = T)
MedGPA %>% ggplot(aes(GPA, Acceptance)) + 
  geom_jitter(width = 0, height = 0.01, alpha = 0.5, col = "#984ea3") + 
  geom_smooth(method = "lm", se = F, col =  "#984ea3") +
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"),
              col = "orange")

newdata <- data.frame(GPA = c(2.5, 3, 4))
predict(med.glm, newdata = newdata, type = "response")
1/(1 + exp(-(med.glm$coefficients[1] + med.glm$coefficients[2]*4)))

sqrt(diag(vcov(med.glm)))
med.glm$coefficients[2]/1.579169 

confint(object = med.glm)

qchisq(0.95, 1)

success <- 1:10
plot(success, dbinom(success, size=5, prob=.6),type='h')

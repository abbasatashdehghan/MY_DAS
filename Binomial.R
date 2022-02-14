library(ggplot2)
library(tidyverse)
library(GGally)
library(dplyr)
library(readr)
library(strengejacke)
library(Stat2Data)
library(rcompanion)
library(sjPlot)
###################################
beetles <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/beetles.csv"))
beetles
str(beetles)
beetles$propkilled <- beetles$killed/ beetles$number

beetles %>% ggplot(aes(dose, propkilled)) + 
  geom_point(size = 1) + labs(x = "Dose", y = "Proportion killed") 


beetles.mat <- cbind(beetles$killed, beetles$number - beetles$killed)


m1 <- glm(beetles.mat ~ beetles$dose,
          family = binomial(link = logit))
summary(m1)
qchisq(df = 1, p = 0.95)
qchisq(df = 6, p = 0.95)

dim(beetles.mat)

p.hat <- predict(m1, type = "response")

fitted <- p.hat*beetles$number
fitted

cbind(beetles$killed, round(fitted, 2) )

m2 <- glm(beetles.mat ~ beetles$dose, family = binomial(link = probit))
summary(m2)

m3 <- glm(beetles.mat ~ beetles$dose, family = binomial(link = "cloglog"))
summary(m3)

beet_p <- data.frame(beetles,
                     logit = fitted(m1),
                     probit = fitted(m2),
                     cloglog = fitted(m3))
beet_p %>% ggplot(aes(x = dose, y = propkilled)) + 
  geom_point() + xlab("Dose") + ylab("Proportion killed") +
  geom_line(aes(x = beetles$dose, y = logit, colour = "Logit")) +
  geom_line(aes(x = beetles$dose, y = probit, colour = "Probit")) +
  geom_line(aes(x = beetles$dose, y = cloglog, colour = "cloglog")) + 
  guides(colour = guide_legend("Method"))

#Example 2 (Challenger disaster
library("faraway")
library(dslabs)
orings %>% ggplot(aes(x = temp, y  = damage/6)) + 
  geom_point() + xlim(c(25, 85)) + ylim(c(0,1)) + 
  xlab ("Temperature (F)") + ylab("Probability of damage") + 
  theme_gray()
orings.mat <- cbind(orings$damage, 6- orings$damage)

m.logit <- glm(orings.mat ~ temp, family = binomial(link = logit),
               data = orings)
summary(m.logit)
qchisq(df = 1, 0.95)
qchisq(df = 21, 0.95)
data.frame(orings, predicted = round(fitted(m.logit), 2))

m.probit <- glm(orings.mat ~ temp, family = binomial(link = "probit"),
                data = orings)
summary(m.probit)
qchisq(df = 1, 0.95)
qchisq(df = 21, 0.95)

m.cloglog <- glm(orings.mat ~ temp, family = binomial(link = "cloglog"),
                 data = orings)
summary(m.cloglog)
data.frame(orings, logit = fitted(m.logit), 
           probit = fitted(m.probit),
           cloglog = fitted(m.cloglog)) %>% 
  ggplot(aes(x = temp, y = damage/6)) + 
  geom_point() + xlab("Temp") + ylab("Proportion damaged") +
  geom_line(aes(x = temp, y = logit, colour = "Logit")) +
  geom_line(aes(x = temp, y = probit, colour = "Probit")) +
  geom_line(aes(x = temp, y = cloglog, colour = "cloglog")) + 
  guides(colour = guide_legend("Method"))


pred1 <- predict(m.logit, data.frame(temp = seq(25,85, len = 23)), type = "response")
pred2 <- predict(m.probit, data.frame(temp = seq(25,85, len = 23)), type = "response")
pred3 <- predict(m.cloglog, data.frame(temp = seq(25,85, len = 23)), type = "response")
data.frame(logit = pred1, probit = pred2, cloglog = pred3,
           px = seq(25,85, len = 23), orings) %>% 
  ggplot(aes(x = px, y = damage/6)) + geom_point(size = 1) +
  xlim (c(25,85)) + ylim(c(0,1)) +
  xlab ("Temperature (F)") + ylab("Probability of damage") +
  geom_line(aes(x = px, y = logit, color = "Logit")) + 
  geom_line(aes(x = px, y = probit, color = "Probit"))+
  geom_line(aes(x = px, y = cloglog, color = "Complementary log-log"))
  


compareGLM(m.logit, m.probit, m.cloglog)
anova(m.logit, m.probit, m.cloglog)

predict(object = m.logit, newdata = data.frame(temp = 31), type = "response")
predict(object = m.probit, newdata = data.frame(temp = 31), type = "response")
predict(object = m.cloglog, newdata = data.frame(temp = 31), type = "response")



#Example 3
yl<- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/yl53.csv"))
head(yl)
yl$hear <- factor(yl$hear)
levels(yl$hear)

yl %>% ggplot((aes(y = age, x = hear))) +
  geom_boxplot() + 
  xlab("What do you hear?") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(fill = NA, colour = "black", size = 1))

plot_xtab(yl$hear,yl$gender, show.values = FALSE, show.total = FALSE,
          axis.labels = c("Laurel", "Yanny"),
          axis.titles=c("What do you hear?"))

yl %>% group_by(gender, hear) %>% summarise(n = n()) %>% 
  ggplot(aes(x = hear, y = n, fill = gender))  +  
  geom_col(position = "dodge")

levels(yl$hear)
#y1 is Tanny
mod.yl <- glm( hear ~ age, family = binomial(link = logit), data = yl)
summary(mod.yl)
confint(mod.yl)

plot_model(mod.yl,type="pred",terms=c("age"), axis.title=c("Age", "Prob(hear Yanny)"),
           title="", ci.lvl=NA)
mod.yl2 <- yl %>% 
  glm(data = ., hear ~ gender, family = "binomial"(link = logit))
summary(mod.yl2)


##example 4
titanic <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/titanic.csv"))
titanic$passenger.class <- factor(titanic$passenger.class)
head(titanic)
plot_xtab(titanic$survived, titanic$gender, show.total = F, show.values = F,
          axis.labels = c("Died", "Survived"), legend.title = "Gender")

titanic %>% group_by(gender, survived) %>% summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>% 
  ggplot(aes(factor(survived), freq, fill = gender)) + 
  geom_col(position = "dodge")

plot_xtab(titanic$survived, titanic$passenger.class, show.values = F, show.total = F,
          axis.labels = c("Died", "Survived"), legend.title = "Class")

titanic %>% group_by(passenger.class,survived) %>% summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>% 
  ggplot(aes(factor(survived), freq, fill = passenger.class)) + 
  geom_col(position = "dodge")

mod.titan <- titanic %>% glm(data = ., survived ~ gender + passenger.class + 
                               age, family = binomial(link = "logit"))
summary(mod.titan)

plot_model(mod.titan, show.values = T)

plot_model(mod.titan, type = "pred", terms = c("age","passenger.class", "gender"))


##Example 6
source(url("http://www.chrisbilder.com/categorical/Chapter5/AllGOFTests.R"))
file.edit("AllGOFTests.R")
HLTest(mod.yl, g=10)

HLTest(mod.yl, g=6)

#example 7
dres.yl <- resid(mod.yl, type = "deviance")
pres.yl <- resid(mod.yl, type = "pearson")
pred.yl <- predict(mod.yl, type = "response")

data.frame(pred.yl = pred.yl, dres.yl = dres.yl, pres.yl = pres.yl) %>% 
  ggplot(aes(x = pred.yl, y = dres.yl)) + geom_point() + 
  labs(x = "Prediction", "Residuals(Deviance)")

dres.b <- resid(m1, type ="deviance")
pres.b <- resid(m1, type = "pearson")
pred.b <- predict(m1, type= "response")

data.frame(pred.b = pred.b, dres.b = dres.b, pres.b = pres.b) %>% 
  ggplot(aes(x = pred.b, y = dres.b)) + geom_point() + 
  labs(x = "Prediction", "Residuals(Deviance)")

#Example 7
library(ROCR)
library(lattice)
library(caret)
titanic$Prid <- predict(object = mod.titan, titanic, type =  "response")
score <- prediction(titanic$Prid, titanic$survived)

perf <- performance(score,"tpr","fpr")
auc <- performance(score,"auc")
auc@y.values[1][[1]]
data.frame(FPR = perf@x.values[1][[1]], TPR = perf@y.values[1][[1]]) %>% 
  ggplot(aes(x= FPR, y=TPR)) + geom_line() +
  xlab("False positive rate") + ylab("True positive rate") +
  ggtitle(paste("Area under the curve:", round(auc@y.values[[1]], 3)))
plot(perf)
cutoffs <- data.frame(cut = perf@alpha.values[[1]], fpr = perf@x.values[[1]],
                      tpr = perf@y.values[[1]])

cutoffs <- cutoffs[order(cutoffs$tpr, decreasing = T), ]

head(cutoffs[cutoffs$fpr <0.2,])
head(subset(cutoffs, fpr < 0.2))

# titanic$survived <- factor(titanic$survived)
# 
# probs <- seq(0, 1, length.out = 10)
# 
# 
# guessing <- map_df(probs, function(p){
#   y_hat <- ifelse(titanic$Prid > p, 1, 0) %>% 
#     factor(levels = levels(titanic$survived))
#   list(method = "ROC",
#        FPR = 1 - specificity(y_hat, titanic$survived),
#        TPR = sensitivity(y_hat, titanic$survived))
#   
# })
# library(ggrepel)
# data.frame(guessing, probs = round(probs,2)) %>% ggplot(aes(FPR, TPR, label = probs)) + 
#   geom_point() + geom_line() + 
#   geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)



#Example 8
trump <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/trump.csv"))
trump$Source <- 0
trump$Source[trump$source == "Android"] <- 1

trump$sentiment[is.na(trump$sentiment)] <- "none"
trump$sentiment <- factor(trump$sentiment)
levels(trump$sentiment)
trump.logit <- trump %>% 
  glm(data = ., Source ~ nwords + contains_url + sentiment + dow + 
        day + hour, family = binomial(link = "logit"))
summary(trump.logit)
anova(trump.logit)
trump$Prid <- predict(trump.logit, newdata = trump, type = "response")
trump.score <- prediction(trump$Prid, trump$Source)
trump.perf <- performance(trump.score, "tpr", "fpr")
trump.auc <- performance(trump.score, "auc")
trump.auc@y.values[1][[1]]

data.frame(FPR = trump.perf@x.values[[1]], TPR = trump.perf@y.values[[1]]) %>% 
  ggplot(aes(x = FPR, y = TPR)) + geom_point()

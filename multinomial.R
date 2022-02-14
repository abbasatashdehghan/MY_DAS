library(nnet)
dcars <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/cars.csv"))
head(dcars)
dcars$response <- factor(dcars$response, 
                         levels = c("no/little", "important", "very important"))
dcars$age <- factor(dcars$age,
                    levels = c("18-23", "24-40", "> 40"))
str(dcars)
dcars %>% ggplot(aes(x = age, y = frequency, fill = response)) + 
  geom_bar(stat = "identity",position = "dodge")  +
  xlab("Age groups" ) + ylab("Frequency" ) +
  theme(legend.position = "none")
dcars %>% ggplot(aes(x = sex, y = frequency, fill = response)) + 
  geom_bar(stat = "identity",position = "dodge") + 
  xlab("Sex" ) + ylab("Frequency" ) +
  scale_fill_discrete(name = "Response category") +
  theme(legend.position = "bottom")
m1 <- multinom(data = dcars, response ~ age + sex, weights =  frequency)
summary(m1)

nullm <-dcars %>% multinom(data = ., response ~ 1, weights = frequency)
summary(nullm, correlation)

dcars %>% select(age) %>% unique()

dcars <- dcars %>% mutate(agelin = ifelse(age == "18-23", 0, 
                                          ifelse(age == "24-40", 1, 2)))

m2 <- multinom(data = dcars, formula = response ~ agelin + sex, weights = frequency)
summary(m2)

qchisq(df = 24, 0.95)

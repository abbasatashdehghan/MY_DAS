df<-datasets::cars
df

my_linear_model <- lm(dist~speed,data = df)

#Prints the model results 
my_linear_model


#Creating a data frame
variable_speed<-data.frame(speed=c(11,11,12,12,12,12,13,13,13,13))

#fiting the linear model
liner_model<-lm(dist~speed,data = df)

#predicts the future values
predict(liner_model,newdata = variable_speed)

liner_model$coefficients[1] + liner_model$coefficients[2] * 11


1/ (1 + pnorm(1,2,4)/pnorm(1,1,4))
1/(1 + exp(-1/8))

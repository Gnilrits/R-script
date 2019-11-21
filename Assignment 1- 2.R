#Installing reccomended packages 
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
install.packages("statsr")
install.packages("corrplot")
#Loading said packages 
library(tidyverse)
library(funModeling)
library(Hmisc)
library(statsr)
library(corrplot)
#First look at the data 
glimpse(icecream)
df_status(icecream)

#A quick look at categoricals- Country and Season 
freq(icecream,6)
freq(icecream,7)

#A chart of the numerical variables 
hist(x=icecream$icecream_sales)
hist(x=icecream$income)
hist(x=icecream$price)
#Normal histrogram wasnt detailed enough for temperature
hist(x=icecream$temperature, breaks=50)

#Quantatitave analysis of numerical variables 
summary(icecream)

icecream %>%
  summarise(count=n(), mu = mean(price), price_med = median(price), 
            sigma = sd(price), price_iqr = IQR(price),
            price_min = min(price), price_max = max(price),
            price_q1 = quantile(price, 0.25),  # first quartile, 25th percentile
            price_q3 = quantile(price, 0.75))  # third quartile, 75th percentile
icecream %>%
  summarise(count=n(), mu = mean(income), income_med = median(income), 
            sigma = sd(income), income_iqr = IQR(income),
            income_min = min(income), income_max = max(income),
            income_q1 = quantile(income, 0.25),  # first quartile, 25th percentile
            income_q3 = quantile(income, 0.75))  # third quartile, 75th percentile
icecream %>%
  summarise(count=n(), mu = mean(temperature), temp_med = median(temperature), 
            sigma = sd(temperature), temp_iqr = IQR(temperature),
            temp_min = min(temperature), temp_max = max(temperature),
            temp_q1 = quantile(temperature, 0.25),  # first quartile, 25th percentile
            temp_q3 = quantile(temperature, 0.75))  # third quartile, 75th percentile
icecream %>%
  summarise(count=n(), mu = mean(icecream_sales), sales_med = median(icecream_sales), 
            sigma = sd(icecream_sales), sales_iqr = IQR(icecream_sales),
            sales_min = min(icecream_sales), sales_max = max(icecream_sales),
            sales_q1 = quantile(icecream_sales, 0.25),  # first quartile, 25th percentile
           sales_q3 = quantile(icecream_sales, 0.75))  # third quartile, 75th percentile
# Hypothesis testing
#A first look at the data, the exploratory analysis 
ggplot(data = na.omit(icecream), 
       aes(x = country, y= icecream_sales, colour=country))    +   
  geom_boxplot() + xlab("Country")      +   
  ylab("Sales")                       +        
  ggtitle("Sales by country")  +  
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=1, size=3)   

#Testing the inference
inference(y= icecream_sales, x = country, data = icecream, 
          statistic = c("mean"), 
          type = c("ci"), 
          null = 0,
          alternative = c("twosided"), 
          method = c("theoretical"), 
          conf_level = 0.95,
          order = c("A","B"))
#Testing the hypothesis
inference(y= icecream_sales, x = country, data = icecream, 
          statistic = c("mean"), 
          type = c("ht"), 
          null = 0,
          alternative = c("twosided"), 
          method = c("theoretical"), 
          conf_level = 0.95,
          order = c("A","B"))
#Modelling for explanation 
icecream %>% 
  select(icecream_sales, price, income, temperature) %>% 
  cor()
#Plotting the only positive correlation

  p1 <- ggplot(icecream, aes(x = price, y = icecream_sales)) +
  geom_point() +
  labs(x = "Price (in £)", y = "Ice cream sales(in £)", title = "Relationship between sales and price") +
  geom_smooth(method = "lm", se = FALSE)
  p2 <- ggplot(icecream, aes(x = temperature, y = icecream_sales)) +
    geom_point() +
    labs(x = "temperature", y = "Ice cream sales (in £)", title = "Relationship between temperature and sales") +
    geom_smooth(method = "lm", se = FALSE)
  p1
  p2
# Building the model
  Icecream_model <- lm(icecream_sales ~ price + temperature + income + season + country, data = icecream) 
  summary(Icecream_model) 
  
#Coefficients testing
  summary(Icecream_model)$coef
#Testing the model
  test0 <- data.frame(income = 20000, price = 4.5 , temperature = 25 , season = "Spring" , country = "A")
  test1 <- data.frame(income = 30000, price = 4.5 , temperature = 25 , season = "Spring" , country = "B")
predict(Icecream_model, test0)
predict(Icecream_model, test1)
1464.09-1396.13
test2 <- data.frame(income = 20000, price = 4.5 , temperature = 25 , season = "Spring" , country = "A")
test3 <- data.frame(income = 20000, price = 5 , temperature = 27 , season = "Spring" , country = "A")
  predict(Icecream_model, test2)
  predict(Icecream_model, test3)  
  1420.249-1396.13
#Confidence intervals on explanatory variables to 90% confidence level
  confint(Icecream_model, "price", level=0.90)
  confint(Icecream_model, "temperature", level=0.90)
  confint(Icecream_model, "income", level=0.90)
  confint(Icecream_model, "season", "spring", level=0.90)
  confint(Icecream_model, "country", level=0.90)
#Testing to see if my data meets the regression conditions.
  plot(Icecream_model$residuals ~ icecream_sales$price)
  plot(Icecream_model$residuals ~ icecream_sales$temperature)
#Transforming the bimodal data into normal for testing 
  log_transform <- icecream %>% mutate(logtemperature = log(temperature)) 
  # Run regression using the log(temperature)
  Icecream_model_log <- lm(icecream_sales ~ logtemperature, data = log_transform) 
  # Residual plot for log(income)
  plot(Icecream_model_log$residuals~ log_transform$logtemperature)

#Prediction
pred1 <- data.frame(income = 30000, price = 3 , temperature = 23 , season = "Spring" , country = "A")
predict(Icecream_model, pred1, interval = "prediction", level = 0.95)

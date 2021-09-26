---
title: 'Task 2: Experimentation and uplift testing'
author: "Yixi Deng"
date: "7/23/2021"
output:
  html_document:
    df_print: paged
    toc: true
    toc_float: true
  pdf_document: default
  word_document: default
header-includes: \usepackage{float}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Load required libraries and datasets
```{r message=FALSE}
#### Load the required libraries
library(data.table)
library(stringr)
library(lubridate)
library(tidyverse)
library(modelr)
library(knitr)
library(rpart)
library(dplyr)

#### Load the dataset
filePath <- "C:/Users/User/Desktop/JobSeeking/Project/Forage/ANZ/"
transactionData <- fread(paste0(filePath,"ANZ synthesised transaction dataset.csv"))
```


## Identify the annual salary for each customer

### Examining transaction data
We can use `str()` to look at the format of each column and see a sample of data.
```{r}
str(transactionData)
```

### Change the format for some variables
```{r Convert variables to proper formats}
#### Convert status column to a factor format
transactionData$status <- as.factor(transactionData$status)
#### Convert date column to a date format
transactionData$date <- as.Date(transactionData$date, format = "%m/%d/%Y")
#### Convert gender column to a factor format
transactionData$gender <- as.factor(transactionData$gender)
#### Convert merchant_state column to a factor format
transactionData$merchant_state <- as.factor(transactionData$merchant_state)
#### Convert movement column to a factor format
transactionData$movement <- as.factor(transactionData$movement)
str(transactionData)
```

### Create weekly salary data frame
```{r}
salary_week_df <- transactionData[movement == "credit",
                                  c("account", "date", "amount", "movement")]
salary_week_df <- salary_week_df %>% group_by(account) %>% 
                  summarise(avg_week_salary = mean(amount))
head(salary_week_df)
```

We created a `salary` dataframe with unique account id and the average weekly salary, which we found from the credit movement.

### Create annual salary data frame
```{r}
salary_annual_df <- cbind(salary_week_df,annual_salary = salary_week_df$avg_week_salary*52)
head(salary_annual_df)
```

Then, we got the annual salary by timing the weekly salary with 52, for there are 52 weeks in a year.

## Explore correlations between annual salary and various customer attributes

### Create a new transactionData dataframe with annual salary
```{r}
transactionData1 <- merge(transactionData,salary_annual_df[,-2], by = "account")
str(transactionData1)
```

We updated the transaction dataframe with the annual salary found the last section. And then, we began to explore the corrleations between annual salary and other customer attributes.

### Explore correlations between annual salary and age
```{r}
cor(transactionData1$annual_salary, transactionData1$age)
plot(transactionData1$annual_salary, transactionData1$age,
     xlab = "Annual Salary (AUD$)",
     ylab = "Age",
     xlim = c(28000,60000))
lines(predict(lm(transactionData1$annual_salary~transactionData1$age)),col = "blue") #? trendline 
```

A weak negative correlation exists between `age` and `annual_salary`.

### Explore correlations between annual salary and gender

```{r}
### Scatter plot
plot(transactionData1$gender,transactionData1$annual_salary,
      xlab = "Gender",
      ylab = "Annual Salary (AUD$)",
      ylim = c(28000,500000))
```

```{r}
### Chi-squared test for annual salary and gender
chisq.test(transactionData1$annual_salary, transactionData1$gender, correct = FALSE)
```

We have a high chi-squared and a p-value of less than 0.05 signifance level, so we reject the null hypothesis and conclude that `age` and `annual_salary` have a significant relationship. 

### Explore correlations between annual salary and states
```{r}
account_loc_inf <- transactionData1[match(unique(transactionData$account),
                                          transactionData1$account), c("account","long_lat")]
account_loc_inf$long <- as.numeric(substr(account_loc_inf$long_lat,1,6))
account_loc_inf$lat <- as.numeric(substr(account_loc_inf$long_lat,8,13))
```

```{r}
coords_state <- function(coords_state, loc_inf,var_name,state_name){
        var_name <- ifelse(loc_inf$long <= coords_state[[1]][3] & 
                                 loc_inf$long >= coords_state[[1]][1] &
                                 loc_inf$lat <=  coords_state[[1]][4] & 
                                 loc_inf$lat >= coords_state[[1]][2], 
                                 state_name,var_name)
}
```


```{r}
coords_NSW <- as.data.frame(c(140.9993, -37.50503, 153.6299, -28.15703 ),
                            c("xmin","ymin","xmax","ymax"))
coords_VIC <- as.data.frame(c(140.9617, -39.13396, 149.9763, -33.99605 ),
                            c("xmin","ymin","xmax","ymax"))
coords_QLD <- as.data.frame(c(137.9943, -29.17719, 153.5522, -9.229287 ),
                            c("xmin","ymin","xmax","ymax"))
coords_SA <- as.data.frame(c(129.0013, -38.06, 141.003, -25.99638 ),
                           c("xmin","ymin","xmax","ymax"))
coords_WA <- as.data.frame(c(112.9211, -35.12228, 129.0019, -13.74142 ),
                           c("xmin","ymin","xmax","ymax"))
coords_TAS <- as.data.frame(c(143.8353, -43.63203, 148.4472, -39.45196 ),
                            c("xmin","ymin","xmax","ymax"))
coords_NT <- as.data.frame(c(129.0005, -25.99862, 137.9991, -10.97392 ),
                           c("xmin","ymin","xmax","ymax"))
coords_ACT <- as.data.frame(c(148.7628, -148.7628, 149.3972, -35.12442 ),
                            c("xmin","ymin","xmax","ymax"))
```

```{r}
account_loc_inf$state <- NA

account_loc_inf$state <- coords_state(coords_NSW, account_loc_inf, 
                                      account_loc_inf$state, "NSW")
account_loc_inf$state <- coords_state(coords_VIC, account_loc_inf, 
                                      account_loc_inf$state, "VIC")
account_loc_inf$state <- coords_state(coords_QLD, account_loc_inf, 
                                      account_loc_inf$state, "QLD")
account_loc_inf$state <- coords_state(coords_SA, account_loc_inf, 
                                      account_loc_inf$state,"SA")
account_loc_inf$state <- coords_state(coords_WA, account_loc_inf, 
                                      account_loc_inf$state, "WA")
account_loc_inf$state <- coords_state(coords_TAS, account_loc_inf, 
                                      account_loc_inf$state, "TAS")
account_loc_inf$state <- coords_state(coords_NT, account_loc_inf, 
                                      account_loc_inf$state, "NT")
account_loc_inf$state <- coords_state(coords_ACT, account_loc_inf, 
                                      account_loc_inf$state, "ACT")

head(account_loc_inf)
```

```{r}
#### Create a new transactionData with state variable
transactionData2 <- merge(transactionData1,account_loc_inf[,c(1,5)],by = "account")
```


```{r}
transactionData2$state <- as.factor(transactionData2$state)

### Scatter plot
plot(transactionData2$state,transactionData2$annual_salary,
      xlab = "State",
      ylab = "Annual Salary (AUD$)",
      ylim = c(28000,500000))
```

```{r}
### Chi-squared test for annual salary and state
chisq.test(transactionData2$annual_salary, transactionData2$state, correct = FALSE)
```

We have a high chi-squared and a p-value of less than 0.05 signifance level, so we reject the null hypothesis and conclude that `state` and `annual_salary` have a significant relationship. 

Now, let's build simple regression models based on the selected attributes.

## Build a Simple Regression Model
### Build a simple regression model to predict the annual salary for each customer
```{r}
model1 <-  annual_salary ~ age + gender
summary(lm(formula = model1, data = transactionData2))
```

The p-values of all attributes in model1 are less than 0.05; however, the R-squared is too small to show it is an accurate model.

```{r}
model2 <-  annual_salary ~ age + gender + state
summary(lm(formula = model2, data = transactionData2))
```

The p-values of all attributes in model2 are less than 0.05. And the R-squared in model2 is larger than that in model1, but it is still too small to show it is an accurate model. Next, we should try to fit decision-tree models to see whether we could a more accurate model.

## Build a Decision-Tree Model

### Data partition

```{r}
set.seed(123)
ind <- sample(2, nrow(transactionData2), replace = T , prob =  c(0.5,0.5))
train <- transactionData2[ind == 1, ]
test <- transactionData2[ind == 2, ]
### Tree Classification
tree1 <- rpart(model1, data = train)
tree2 <- rpart(model2, data = train)
```

### Prediction and Accuarcy
```{r}
### Prediction
p1 <- predict(tree1, train)
### Root Mean Square Error
sqrt(mean(train$annual_salary - p1)^2)
### R Square
(cor(train$annual_salary,p1))^2
```
```{r}
### Prediction
p2 <- predict(tree2, train)
### Root Mean Square Error
sqrt(mean(train$annual_salary - p2)^2)
### R Square
(cor(train$annual_salary,p2))^2
```

Overall, the decision-tree models have much higher accuracy than the simple regression models have.

The decision-tree model with model2 has the higher accuracy compared to the decision-tree model with model1. As a result, we would recommend the decision-tree model with model2 as our final model for prediction.
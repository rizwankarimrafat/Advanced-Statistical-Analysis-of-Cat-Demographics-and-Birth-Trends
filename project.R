---
title: "Project 1 - Advanced Statistical Methods"
output: pdf_document
---

Name: Rizwan Karim Rafat

Student ID: 22061272

Subject Code: MATH7002

By including this statement, we the authors of this work, verify that:

* I hold a copy of this assignment that we can produce if the original is lost or damaged.

* I hereby certify that no part of this assignment/product has been copied from any other student’s work
or from any other source except where due acknowledgement is made in the assignment.

* No part of this assignment/product has been written/produced for us by another person except where
such collaboration has been authorised by the subject lecturer/tutor concerned.

* I am aware that this work may be reproduced and submitted to plagiarism detection software programs
for the purpose of detecting possible plagiarism (which may retain a copy on its database for future
plagiarism checking).

* I hereby certify that we have read and understand what the School of Computing and Mathematics
defines as minor and substantial breaches of misconduct as outlined in the learning guide for this unit.

Note: An examiner or lecturer/tutor has the right not to mark this project report if the above
declaration has not been added to the cover of the report.

\newpage

```{r, warning=FALSE}
library(mixtools)
```

# Cat Breeds

## The RCSS want to investigate which times of the year have the most cats births. Use the data to provide a density plot of cat births throughout the year. Examine the plot and provide an appropriate model of the density plot and its fitted coefficients. Present the equation of the model in your report.

```{r}
data <- read.csv("cats2023.csv")
head(data)
```

```{r}
str(data)
```

```{r}
summary(data)
```

```{r}
dim(data)
```

```{r}
data$date <- as.Date(data$date)
```

```{r}
rep_data_date <- rep(data$date, data$litter_size)
rep_data_date <- as.numeric(rep_data_date)
hist(rep_data_date)
```

In the histogram above we can see a distribution that has two peaks, so we can suggest that this is a bimodal distribution. 

```{r, warning=FALSE}
density_model <- density(rep_data_date)
plot(density_model)
```

```{r}
as.Date(density_model$x[which.max(density_model$y)], origin = "1970-01-01")
```

From the density plot of the cats' births above we can observe that most cats are born during March-April and October-November.

```{r, warning=FALSE}
x = rep_data_date

negloglik2 = function(mu1,s1, mu2,s2, lambda)
  -sum(log(lambda*dnorm(x, mu1, s1) + (1-lambda)*dnorm(x,mu2,s2)))

require(stats4)
fit0 = mle(negloglik2, 
    start=list(mu1=min(x), s1=sd(x), mu2=max(x), s2=sd(x), lambda=0.5))

summary(fit0)
```

Equation of the model:

f(x) = lambda * exp(-(x - mu1)^2 / (2 * s1^2)) + (1 - lambda) * exp(-(x - mu2)^2 / (2 * s2^2))

where:

* x is the value of date

* lambda is the mixing parameter

* mu1 and mu2 are the means of the two normal distributions

* s1 and s2 are the standard deviations of the two normal distributions

# Breed vs Family Income

## Certain breeds of cat seem to be chosen to represent social status, we want to investigate if the data shows this relationship. Estimate the probability of each cat breed conditioned on the family income. Provide estimates of the proportion of each cat breed, given that the family income is $80,000.

```{r}
range(data$income)
unique(data$breed)
```

```{r, warning=FALSE}
boxplot(income ~ breed, data = data, horizontal = TRUE)
```

The boxplot shows that the median income for Shorthair cats is the highest, followed by Burmese cats and then Bengal cats. The IQR for Shorthair cats is also the smallest, followed by Burmese cats and then Bengal cats. This means that the income for Shorthair cats is more tightly clustered around the median than the income for Burmese cats or Bengal cats.

The boxplot also shows that there are no outliers in the data for Shorthair cats or Burmese cats. However, there are a few outliers in the data for Bengal cats. This means that there are a few Bengal cats that have incomes that are much higher or lower than the median income for Bengal cats.

```{r}
lo <- range(data$income)[1]
hi <- range(data$income)[2]

f <- density(data$income, from = lo, to = hi)
f1 <- density(data$income[data$breed == unique(data$breed)[1]], from = lo, to = hi) #Bengal
f2 <- density(data$income[data$breed == unique(data$breed)[2]], from = lo, to = hi) #Shorthair
f3 <- density(data$income[data$breed == unique(data$breed)[3]], from = lo, to = hi) #Burmese
```

```{r, warning = FALSE}
plot(f, main="Density Plots")

lines(f1, col="red")
lines(f2, col="blue")
lines(f3, col="green")

legend("topright", legend=c("f", "f1", "f2", "f3"), col=c("black", "red", "blue", "green"), lty=1)

```

```{r}
p1 <- mean(data$breed == unique(data$breed)[1]) #Bengal
p1
p2 <- mean(data$breed == unique(data$breed)[2]) #Shorthair
p2
p3 <- mean(data$breed == unique(data$breed)[3]) #Burmese
p3
```

This means that 45.96% of the cats in the data set are Bengals, 37.27% are Shorthairs, and 16.77% are Burmese.

```{r}
plot(f$x, f1$y*p1/(p1*f1$y+p2*f2$y+p3*f3$y), type = "l", main = paste('Bengal'), xlab = "income", ylab = "breed | income")
abline(v = 80000, col = "red")
abline(h = 0.36, col = "red")
plot(f$x, f2$y*p2/(p1*f1$y+p2*f2$y+p3*f3$y), type = "l", main = paste('Shorthair'), xlab = "income", ylab = "breed | income")
abline(v = 80000, col = "red")
abline(h = 0.535, col = "red")
plot(f$x, f3$y*p3/(p1*f1$y+p2*f2$y+p3*f3$y), type = "l", main = paste('Burmese'), xlab = "income", ylab = "breed | income")
abline(v = 80000, col = "red")
abline(h = 0.11, col = "red")
```

Given that the family income is $80,000 from the graphs above we can make the following estimates:

Bengal: 0.36

Shorthair: 0.535

Burmese: 0.11

# Gestation period and Income

## There is a belief that family income and the mental state of a cat are correlated, and that mental state is dependent on its time in the womb. Provide a model for the gestation period conditioned on the family income and estimate the coefficients to determine if there is a relationship. If any unobserved variables are identified, speculate what they might represent.

Since, in our dataset each row contains equal to or more than one cat birth, we need to replicate the rows for both income and gestation with respect to the litter size.

```{r}
rep_gestation <- rep(data$gestation, data$litter_size)
rep_income <- rep(data$income, data$litter_size)
```

```{r}
par(mfrow = c(1, 2))
hist(rep_income)
hist(rep_gestation)
```

From the histogram of rep_income we can observe that most cats are born in families with an annual income of between \$60,000 and $80,000.

The gestation histogram shows that most of the cats have a gestation period of 50 to 70 days.

For further investigation, we will produce a scatter plot to check if there is any relationship between family income and gestation period.

```{r}
plot(rep_income, rep_gestation)
```

From the initial scatter plot above, we can observe that there is a positive relationship between the family income and gestation period but there seems to be two separate clusters. For further investigation we need to fit our data into a regression mixture model.

```{r}
fit <- normalmixEM(rep_income, k = 2)
cov_matrices <- fit$Sigma

for (i in 1:2) {
  is_different_sigma <- !identical(cov_matrices[[i]], cov_matrices[[1]])
  cat("Component", i, "has a different sigma:", is_different_sigma, "\n")
}
```

From the above, we can observe that rep_income have a single variance. Thus for the reg_model_1 where we have used regmixEM to fit our model, we have used arbvar = FALSE.

```{r}
reg_model_1 <- regmixEM(x = rep_income, y = rep_gestation, k = 2, arbvar = FALSE)
plot(reg_model_1, whichplots = 2)
```

```{r}
reg_model_2 <- regmixEM(x = rep_income, y = rep_gestation, k = 3, arbvar = FALSE)
plot(reg_model_2, whichplots = 2)
```

```{r}
summary(reg_model_1)
summary(reg_model_2)
```

```{r}
aic <- c(-2*reg_model_1$loglik+2*(3*3-1),
         -2*reg_model_2$loglik+2*(3*4-1))
aic
```

From the summary above, we can observe that the loglik at estimate for reg_model_1 is `r reg_model_1$loglik` and for reg_model_2 is `r reg_model_2$loglik`.

We have also calculated the AIC for the two models and we can see that the AIC for reg_model_2 is lower referring that it is a better model.

Coefficients:

```{r , echo=FALSE}
summary(reg_model_2)
```

Lambda refers to the mixing proportions for each component.

Sigma refers to the standard deviation for each component.

Beta1 refers to the intercept and beta2 refers to the slope for each component.

In reg_model_2 comp 2 which has a very low lambda value is an unobserved variable. It could be due to one or more of the following issues:

* health

* diet

* environment

* measurement error

# Birth Suburbs

## The RCSS want a simple model of birth place of the cats, to identify which suburbs produce the most cats. Provide a model of the density of the of the birth locations, and provide the fitted coefficients. Use the model to identify if more cats are likely to be born in Sydney or Parramatta.

```{r}
rep_latitude <- rep(data$latitude, data$litter_size)
rep_longitude <- rep(data$longitude, data$litter_size)
rep_lat_long_df <- data.frame(rep_latitude, rep_longitude)
```

```{r, warning = FALSE}
plot(rep_lat_long_df)
```

```{r, warning = FALSE}
reg_model_3 <- mvnormalmixEM(rep_lat_long_df, k = 2)
plot(reg_model_3, whichplots = 2)
abline(v = -33.81167161635268, col = "red")
abline(h = 151.02512573859647, col = "blue")
abline(v = -33.86395922654538, col = "red")
abline(h = 151.20818397512764, col = "blue")
```

```{r, warning = FALSE}
reg_model_4 <- mvnormalmixEM(rep_lat_long_df, k = 3)
plot(reg_model_4, whichplots = 2)
abline(v = -33.81167161635268, col = "red")
abline(h = 151.02512573859647, col = "blue")
abline(v = -33.86395922654538, col = "red")
abline(h = 151.20818397512764, col = "blue")
```

```{r}
summary(reg_model_3)
summary(reg_model_4)
```

From the summary above, we can observe that the loglik at estimate for reg_model_1 is `r reg_model_3$loglik` and for reg_model_2 is `r reg_model_4$loglik`. Since the loglik at estimate for reg_model_4 is higher, we can suggest that it is a better model.

Since the coordinates for Parramatta intersects closer to the center of the cluster, we can suggest that cats are likely to be born in Parramatta.
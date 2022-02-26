# BC#1 - Basic Concepts - Statistics

# Author: Augusto Gontijo
# LinkedIn: https://www.linkedin.com/in/augusto-gontijo/?locale=en_US
# GitHub: https://github.com/augusto-gontijo 


# === WHAT IS THIS PROJECT? ===

# This is a compilation of some basic concepts about statistics. Contents:

# Data Types
#   Qualitative Variables
#   Quantitative Variables
# Frequency Distribution
# Measures of Central Tendency
#   Mean
#   Median
#   Mode
# Separation Measures
#   First Quartile Q1
#   Second Quartile Q2
#   Third Quartile Q3
# Dispersion Measures
#   Range
#   Variance
#   Median Absolute Deviation
#   Standard Deviation
# Probability Distributions
#   The Bernoulli Experiment
#   Binomial Distribution
#   Geometric Distribution
#   Negative Binomial Distribution
#   Poisson Distribution
#   Normal Distribution
#   Standard Normal Distribution
#   F Distribution (Fisher-Snedecor)
#   Student's T Distribution
#   Chi-Squared Distribution
#   Central Limit Theorem (CLT)
# Confidence Level, Significance and Confidence Intervals
#   Confidence interval for sample mean by the Standard Normal distribution
#   Confidence interval for sample mean by the Student's T Distribution
#   Confidence interval for the proportion
#   Confidence Interval for the mean using Bootstrap
# Hypothesis Testing
#   Validating if a random variable follows a normal distribution
#   T Test for difference of means (two independent samples)
#   T Test for difference of means (two dependent samples)
#   Chi-Squared Test for association between categorical variables
#   F Test for variance analysis (ANOVA)
# Linear Regression



# === LIBRARY INSTALL ===

# You'll need to install the libraries below to run this project:

# install.packages('dplyr')
# install.packages('forecast')
# install.packages('ggplot2')
# install.packages('truncnorm')
# install.packages('plotly')
# install.packages('pROC')



# === LIBRARY IMPORT ===

# Once installed, load the libraries:

library(dplyr)
library(forecast)
library(ggplot2)
library(truncnorm)
library(plotly)
library(pROC) 



# === CLEAR R MEMORY ===

rm(list=ls(all=TRUE))



# === DISABLE SCIENTIFIC NOTATION ===

options(scipen=999)



# === SETTING YOUR FOLDER PATH ===

# We'll use 5 different data sets in this project, which are: 
# coffee.csv
# drinks.csv
# people.csv
# sales.csv
# tests.csv

# I recommend that your store all of them in the same folder.

# You'll need to map the folder path on the 'path' variable below:

path <- "C:/...YOUR COMPUTER PATH.../Datasets/"



# === DATA TYPE ===

# In this first section, we'll study data types. 

# Importing the 'people.csv' file:

data <- read.csv(file = paste(path, "people.csv", sep = ""), sep =';')

# Dataset preview (first rows):

head(data)

# As you can see, its a simple data set that contains the following variables:
# id (integer)
# gender (M for Male and F for Female)
# age (Years)
# height (meters)
# education (None, High School, Bachelor's, Master's, PhD)

# Statistical Summary:

summary(data)

# For the numerical variables, the summary function gives us the min, max and quantiles of each variable
# For the categorical variables, it only gives us the count.

# Qualitative Ordinal Variables:

# A qualitative ordinal variable is a qualitative variable with an order implied in the levels. 
# For instance, the education level in our dataset:
# None < High School < Bachelor's < Master's < PhD

unique(data$education)

# Qualitative Nominal Variables:

# A qualitative nominal variable is a qualitative variable where no ordering is possible or implied in the levels. 
# For example, the gender in our datset:

unique(data$gender)

# Quantitative Discrete Variables:

# Quantitative discrete variables are variables for which the values it can take are countable and have a finite number of possibilities.
# For exemple, the age (in whole years) in our dataset:

head(data$age)

# Quantitative Continuous Variables:

# Quantitative continuous variables are variables for which the values are not countable and have an infinite number of possibilities. 
# For example, the height in our dataset. An individual can be 1.736178 meters tall.

mean(data$height)



# === FREQUENCY DISTRIBUTION ===

# Frequency distribution in statistics is a representation that displays the number of observations within a given interval.

# The first step in an analysis work is understanding the behavior of the variables involved in the study.
# Using statistical techniques such as the analysis of FREQUENCY DISTRIBUTIONS and HISTOGRAMS we can better assess the way in which the phenomena under study are distributed.

# Distribution of age on a histogram:

hist(data$age)

# Distribution of height on a histogram:

hist(data$height)

# Distribution of gender on bar plot:

ggplot(data %>% count(gender), aes(x=gender, y=n)) + 
  geom_bar(stat = "identity")

# Distribution of education on bar plot:

ggplot(data %>% count(education), aes(x=education, y=n)) + 
  geom_bar(stat = "identity")



# === MEASURES OF CENTRAL TENDENCY ===

# A measure of central tendency is a summary statistic that represents the center point or typical value of a dataset.

# 1) Mean: also known as average, the average considers the sum of all values divided by the number of values. It is sensitive to outliers.

# Mean of age

mean(data$age)

# Mean of height

mean(data$height)

# 2) Median: splits the population or sample in half. It is the center measurement, where 50% of the values are < median and 50% are > median. It is not sensitive to outliers.

# Median of age

median(data$age)

# Median of height

median(data$height)

# 3) Mode: The mode is the value that appears most often in a set of data values. 

# Creating mode function:
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Age mode

result <- getmode(data$age)
print(result)

# Height mode

result <- getmode(data$height)
print(result)



# === SEPARATION MEASURES ===

# These are numbers that divide the ordered sequence of data into parts that contain the same number of elements of the series. 
# The most used measures are quartiles.


# 1) First Quartile (Q1): Splits off the lowest 25% of data from the highest 75%. This point corresponds to the point where 25% of the entire sample is below it.

# Q1 of age and height

quantile(data$age, probs = 0.25)
summary(data$age)

quantile(data$height, probs = 0.25)
summary(data$height)

# 2) Second Quartile (Q2): The second quartile and the Median are the same. Cuts data set in half. This point corresponds to the point where 50% of the entire sample is below it.

# Q2 of age and height

quantile(data$age, probs = 0.5)
summary(data$age)

quantile(data$height, probs = 0.5)
summary(data$height)

# 3) Third Quartile (Q3): Splits off the highest 25% of data from the lowest 75%. This point corresponds to the point where 75% of the entire sample is below it.

quantile(data$age, probs = 0.75)
summary(data$age)

quantile(data$height, probs = 0.75)
summary(data$height)



# === DISPERSION MEASURES ===

# A measure of statistical dispersion is a nonnegative real number that is zero if all the data are the same and increases as the data become more diverse.

# 1) Range: In statistics, the range of a set of data is the difference between the largest and smallest values.

# Range of age and height:

r.x <- range(data$age)
diff(r.x)

r.x <- range(data$height)
diff(r.x)

# 2) Variance: Variance is the expectation of the squared deviation of a random variable from its population mean or sample mean. 
# Meaning it is a measure of how far a set of numbers is spread out from their average value. variance is extremely sensitive to outliers.

# Variance of age and height:

var(data$age)

var(data$height)

# 3) Median Absolute Deviation (MAD): The median absolute deviation (MAD) is a robust measure of the variability of a univariate sample of quantitative data. 
# It can also refer to the population parameter that is estimated by the MAD calculated from a sample.

# MAD of age and height:

mad(data$age)

mad(data$height)

# 4) Standard Deviation: The standard deviation is a measure of the amount of variation or dispersion of a set of values. 
# A low standard deviation indicates that the values tend to be close to the mean (also called the expected value) of the set, 
# While a high standard deviation indicates that the values are spread out over a wider range.
# The standard deviation of a random variable or sample is the square root of its variance.

# Standard Deviation of age and height:

sd(data$age)

sd(data$height)



# === PROBABILITY DISTRIBUTIONS === 

# 0) The Bernoulli Experiment:

# It is simply an attempt at a random experiment. For example, a single coin flip will result heads or tails. Heads could be considered success (or vice-versa).
# In practice, it is the researcher who defines which event will be considered a success for a given study.

# 1) Binomial Distribution:

# In probability theory and statistics, the binomial distribution with parameters n and p is the discrete probability distribution of the number of successes in a sequence of n independent experiments,
# each asking a yes-no question, and each with its own Boolean-valued outcome: success (with probability p) or failure (with probability q = 1 ??? p).

# Example: Each time a customer enters the store, he/she has a 30% chance of buying something. The average number of customers that visits the store is 50 (per day).
# We want to know what is the probability of 15 sales on a single day.

dbinom (x = 15, size = 50, prob = 0.3)

# Given:
# x = number of success (customers that bought),
# size = number of attempts (customers entering the store),
# prob = probability of success (customer buy something)

# The following function generates random amounts of success from a number (size) of trials given the probability (prob) of success. 
# It is useful for carrying out experiments. 

# We can simulate what is the expected frequency of sales per ten customers, maintaining the probability of success (customer buy) at 30%

rv_binomial <- rbinom(n = 100, size = 10, prob = 0.3)

# Given:
# n =  the number of times the experiment will be repeated
# size = the number of attempts of each experiment 
# prob = probability of success of each attempt

hist(rv_binomial, breaks = 10) # The taller bar on this histogram represents the amount of sales expected

# We may also want the probability that UP TO ten customers buying, instead of knowing the probability of exactly 15 buying.

# Example: Each time a customer enters the store, he/she has a 30% chance of buying something. The average number of customers that visits the store is 50 (per day).
# The store needs to sell to at least 10 customers to cover the operational costs.
# We want to know what is the probability of 10 sales (OR LESS) on a single day.

# The probability that up to 10 customers buy is:
# (probability that 0 customers will buy) + (probability that 1 customer will buy) + ... + (probability that 10 customers will buy)

# Formula: P(X<=10) = P(X=0) + P(X=1) + ... + P(X=10)

pbinom(q = 10, size = 50, prob = 0.3)

# The probability of 10 sales (OR LESS), Given that 50 customers visited the store, is 7,88%


# 2) Geometric Distribution

# The geometric distribution is the probability distribution of the number X of Bernoulli trials needed to get one success.
# In other words, repeating a Bernoulli experiment x times until the first success occurs. It is the number of failures until the first success.

# Example: Each time a customer enters the store, he/she has a 30% chance of buying something.
# What is the probability that the first sale will occur when the fourth (x) customer enters the store?

dgeom(x = 4, prob = 0.3)

# Given:
# x = number of trials
# prob = probability of success (customer buy)

# We can use the same function to get the probability of success on the 1st, 2nd ... 20th attempt:

rv_geometric <- dgeom(x = 1:20, prob = 0.3)
rv_geometric
plot(rv_geometric) 

# Observe how the probabilities go down.
# It is very likely that a successful attempt occurs in the first few attempts.

# We can use the cumulative geometric distribution to find out what is the probability of the first success occurring
# on the 1st OR on the 2nd OR on the ... 5th attempt

# Formula: P(X<=5)

rv_geometric_c <- pgeom(0:5, prob = 0.3)
plot(rv_geometric_c)

# By observing the plot above, we can see that its VERY likely that the first success occurs on the first 5 attempts.


# 3) Negative Binomial Distribution

# The negative binomial distribution is a discrete probability distribution that models the number of successes in a sequence of independent and 
# identically distributed Bernoulli trials before a specified (non-random) number of failures occurs.

# Example: Each time John visits a client, he has a 30% chance to make a sale.

# What's the probability of John needing to visit 6 clients until the 3rd sale occurs?

# Given:
# x = number of successful attempts
# size = amount of attempts
# prob = success probability

dnbinom(x = 3, size = 6, prob = 0.3)


# 4) Poisson Distribution

# A discrete probability distribution that expresses the probability of a given number of events occurring in a fixed interval of time or space 
# if these events occur with a known constant mean rate and independently of the time since the last event.

# Example: A store has an average of 35 client visits per day.
# What's the probability of 40 clients visiting the store in one day?

dpois(x= 40,lambda = 35)

# Given:
# x = the quantity to be tested
# lambda = average occurrence of the event in a given interval of time

# We can use the same function to get the probability of a range of client visit amounts:

rv_poison <- dpois(x = 15:70, lambda = 35)
plot(rv_poison)

# The lambda value is the mean (and also the standard deviation) of the Poisson Distribution

# We can also obtain the cumulative probability of UP TO 50 clients  visiting the store in one day

# In other words: P(X<=5)

rv_poison <- ppois(1:50, lambda = 35)
plot(rv_poison)


# 5) Normal Distribution 

# The Normal (or Gaussian) curve very adequately describes the behavior of a random variable that is distributed symmetrically around a central value.
# The two parameters that characterize it are the mean (which specifies the central value) and the variance (which defines its variability around the mean).

# Example: Assume that the distribution of monthly salaries of employees in a company follows a normal distribution. 
# With mean of 6500 and standard deviation of 1500

# When randomly selecting an individual from this population, what is the probability of his/her salary to be between 2000 and 4000?

# Firstly we need to find the probability of this individual's salary be 4000, and then subtract from the probability of this individual's salary be 2000

# P(X<=4000)
prob_upto_4000 <- pnorm(q = 4000, mean = 6500, sd = 1500)

# P(X<=2000)
prob_upto_2000 <- pnorm(q = 2000, mean = 6500, sd = 1500)

# P(X<=4000) - P(X<=2000)
prob_upto_4000 - prob_upto_2000

# We can also generate X random numbers for a normal distribution with mean 5000 and standard deviation of 2500

rv_normal <- rnorm(n = 10000, mean = 6500, sd = 1500)
hist(rv_normal)


# 6) Standard Normal Distribution

# The standard normal distribution, also called the z-distribution, is a special normal distribution where the mean is 0 and the standard deviation is 1.
# Any normal distribution can be standardized by converting its values into z-scores. Z-scores tell you how many standard deviations from the mean each value lies.

# The scale() command standardized a random variable.

# By applying the scale() function to the random variable "rv_normal" we've just created, it will have a mean of 0 and standard deviation of 1.

rv_standard_normal <- scale(rv_normal)
hist(rv_standard_normal)

# Example: Assume that the distribution of monthly salaries of employees in a company follows a normal distribution. 
# With mean of 6500 and standard deviation of 1500

# When randomly selecting an individual from this population, what is the probability of his/her salary to be above 7500?

# The first step is to standardize the value of 7500

z <- (7500-6500)/1500
pnorm(z, mean = 0, sd = 1)

# Or simply:
pnorm(z)

# We can visualize where is the z-value relative to the mean

plot(density(scale(rv_normal))) # Density curve plot
abline(v = 0,col = 'blue') # Generates a blue line over the mean, which is zero (because we standardized the distribution)
abline(v = z ,col = 'red') # Generates a red line over the z-value

# The distance between the z-value (0.74) and the normalized mean (0) is the same of the 7500 value to the mean (6500)


# 7) F Distribution (Fisher-Snedecor)

# It is a positively skewed distribution, it does not admit negative values. 
# It is generally used to test for variances, and depends on two parameters called degrees of freedom. 
# The degrees of freedom are directly associated with the sample size. 

# If a random variable X follows an F distribution with v1 and v2 degrees of freedom, we can say that: X ~ F(v1, v2) 

# Generating a random sample of 5000 numbers following an F Distribution

# Given:
# n = amount of numbers
# df1 = first degree of freedom
# df2 = second degree of freedom 

rv_f <- rf( n= 5000, df1 = 5 , df2 = 33 )
hist(rv_f)

# By increasing the degrees of freedom, the distribution will get closer to an normal distribution

rv_f <- rf( n= 5000, df1 = 200 , df2 = 300 )
hist(rv_f)

# Extra: A random variate of the F-distribution with parameters d1 and d2 arises as the ratio of two appropriately scaled chi-squared variates


# 8) Student's T-distribution

# It is symmetrical and similar to the standard normal curve, and depends on a single parameter, 
# which is also a degree of freedom. It is widely used to test averages. 

# Generates a random sample of 3000 numbers following a T-Distribution
 
rv_t <- rt(3000, df = 6)
hist(rv_t)

# Note that the T-Distribution is centered at zero (like the Standard Normal Distribution)

# As we increase the degree of freedom, it gets similar to the Normal Distribution.

rv_t <- rt(3000, df = 800)
hist(rv_t)


# 9) Chi-Squared Distribution

# The chi-squared distribution with k degrees of freedom is the distribution of a sum of the squares of k independent standard normal random variables.

# Generates a random sample of 2000 numbers following a Chi-Squared Distribution

rv_chisq <- rchisq(2000, df = 5)
hist(rv_chisq)


# 10) Central Limit Theorem (CLT)

# The central limit theorem (CLT) states that the distribution of sample means approximates a normal distribution as the sample size gets larger, 
# regardless of the population's distribution.
# Sample sizes equal to or greater than 30 are often considered sufficient for the CLT to hold

# Example: A fair die with 20 sides (also known as D20) can be modeled with a discrete random variable with outcome 1 through 20, each with the equal probability of 1/20.

# Creating the die sides:

die_sides <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

# Creating the outcome of throwing the die 200 times:

outcome <- sample(die_sides, 200, replace = TRUE)

# Plotting the outcome (the red line represents the mean):

dev.off()

hist(outcome, 
     breaks = 10, 
     col ="light blue")
abline(v = mean(die_sides),
       col = "red",lty = 1)

# Next, we will create 3 vectors:
# t15 = a list of 200 numbers, each one represents the mean of a sample of 15 random numbers
# t500 = a list of 200 numbers, each one represents the mean of a sample of 500 random numbers
# t10000 = a list of 200 numbers, each one represents the mean of a sample of 10000 random numbers

t15 <- c()
t500 <- c()
t10000 <- c()

k = 200

for (i in 1:k) {
  t15[i] = mean(sample(die_sides,15, replace = TRUE))
  t500[i] = mean(sample(die_sides,500, replace = TRUE))
  t10000[i] = mean(sample(die_sides,10000, replace = TRUE))
}

# Plotting the 3 histograms of the 3 vectors
# Note that as the sample size gets larger, the histogram approximates a normal distribution

par(mfrow=c(1,3))

hist(t15, 
     col ="light green",
     main="n=15", 
     xlab = "d20 roll")
abline(v = mean(t15), 
       col = "red")
shapiro.test(t15)


hist(t500, 
     col ="light blue", 
     main = "n=500", 
     xlab = "d20 roll")
abline(v = mean(t500), 
       col = "red")
shapiro.test(t500)


hist(t10000, 
     col ="pink", 
     main = "n=10000", 
     xlab = "d20 roll")
abline(v = mean(t10000), 
       col = "red")
shapiro.test(t10000)

dev.off()


# === CONFIDENCE LEVEL, SIGNIFICANCE AND CONFIDENCE INTERVALS ===

# The confidence level (1 - alpha) represents the probability of success.
# In a complementary way, the significance level (alpha) expresses the probability of error. 

# The confidence level represents the degree of confidence that the result would be within a certain range.
# When we set a confidence level of 95%, for example, 
# we are assuming that there is a 95% probability that the results are a good representation of the reality.

# 1) Confidence interval for sample mean by the Standard Normal distribution 

# To obtain the confidence interval for a variable that has:
# mean = 30
# std_deviation = 7.31
# n = 30

# We have to define the confidence level of our interval

# We can obtain the quantile value for the desired confidence level using the qnorm() function

# The quantile in the standard normal distribution for a confidence level of 95%:

ci <- 0.95
alpha <- 1 - ci
1 - (alpha / 2 ) # this results in 0.975
qnorm(0.975)

# Storing the values in objects:

mean <- 30
std_deviation_p <- 7.31
n <- 30
quantile_95 <- qnorm(0.975)


# Superior limit = mean + quantile_95 * (sd_p / sqrt(n))
# Inferior limit = mean - quantile_95 * (sd_p / sqrt(n))

# Applying the limits:

sup_limit <- mean + quantile_95 * (std_deviation_p / sqrt(n))
inf_limit <- mean - quantile_95 * (std_deviation_p / sqrt(n))

paste("With 95% of confidence, we can estimate that the mean varies between",inf_limit," and ",sup_limit)


# 2) Confidence interval for sample mean by the Student's T Distribution 

# The Student's T distribution should be used when the standard deviation of the population is unknown.

# In this case, we will assume the standard deviation based on a sample.

# Storing the values in objects:

mean <- 30
std_deviation_s <- 7.31
n <- 30
quantile_95_t <- qt(0.975,df = n-1) #entender melhor e descrever

# Superior limit = mean + quantile_95 * (sd_s / sqrt(n))
# Inferior limit = mean - quantile_95 * (sd_s / sqrt(n))

# Applying the limits:

sup_limit_t <- mean + quantile_95_t * (std_deviation_s / sqrt(n))
inf_limit_t <- mean - quantile_95_t * (std_deviation_s / sqrt(n))

paste("With 95% of confidence, we can estimate that the mean varies between",inf_limit_t," and ",sup_limit_t)

# If our data is already on a R data frame, there is a function that can generate the confidence interval in a much easier way

# Generating a random variable using the rnorm() function:

# Given:
# mean = 30, 
# std_deviation = 7.31
# n = 30

rv <- rnorm(n = 30, mean = 30, sd = 7.31)

# Visualizing the random variable:
hist(rv)

# Calculating the confidence interval of 95% in a Student's T distribution using the t.test() function:

CI <- t.test(rv, conf.level = 0.95)
CI$conf.int

# Note that the results are slightly different from calculating it manually.


# 3) Confidence interval for the proportion

# Assume that 500 customers were randomly chosen, of those 500 customers, 138 made a complaint about the product.
# Therefore, the proportion of clients that made a complaint is 138/500 = 0,276 (or 27.6%).

# Next we will calculate the interval for the proportion

# Storing the values in objects:

complaints <- 138
n <- 500
quantile_95 <- qnorm(0.975)
proportion <- complaints/n

# Superior limit proportion = proportion + quantile_95 * sqrt(proportion * (1 - proportion)/n)
# Inferior limit proportion = proportion - quantile_95 * sqrt(proportion * (1 - proportion)/n)

# Applying the limits:

sup_limit_prop <- proportion + quantile_95 * sqrt(proportion * (1 - proportion)/n)
inf_limit_prop <- proportion - quantile_95 * sqrt(proportion * (1 - proportion)/n)

paste("With 95% of confidence, we can estimate that the proportion varies between",inf_limit_prop," and ",sup_limit_prop)

# we can obtain the confidence interval for the proportion in a much easier way using the prop.test() function:

CI_proportion <- prop.test(x = 138, n = 500, conf.level = 0.95)
CI_proportion$conf.int

# Note that the results are slightly different from calculating it manually.


# 4) Confidence Interval for the mean using Bootstrap

# The bootstrap method is a statistical technique for estimating quantities about a population by averaging estimates from multiple small data samples.
# The next examples uses the Central Limit Theorem that we previously reviewed on this project.

# Creating a random variable following a Chi-Squared distribution with 60 observations and 3 degrees of freedom

rv <- rchisq(n = 60, df = 3)
hist(rv) # note how asymmetrical is the distribution

# Creating the variables

mean <- c() # Each value of this vector represents the mean of each sample taken
R <- 1000 # Number of samples taken

# Bootstrap
for (i in 1:R) {
  resample <- sample(rv, size = 50, replace = T) # Takes a random sample with replacement
  mean[i] <- mean(resample) # Stores the mean of the sample
}

# Distribution of the mean of the samples
hist(mean)

# Note that even though the original distribution DIT NOT follow a normal distribution, 
# The Central Limit Theorem assures that the distribution of the mean of the samples will be always close to normal 

# Next, we'll need to find the 2 values that mark the beginning and the end of our confidence interval.
# Assuming that we want 95% of confidence:

(1 - 0.95) / 2 
1 - ( 1 - 0.95) / 2

# Therefore, we can obtain the confidence interval for 95% of confidence:

quantile(mean, probs = c(0.025, 0.975)) 

# Let's try another experiment:

# Generating a random variable with:
# mean = 30  
# std deviation = 7.31  
# n = 30

rv <- rnorm(n = 30, mean = 30, sd = 7.31)

# This time, we'll calculate the confidence interval using the Bootstrap Method on a Student's T distribution:

mean <- c() # Each value of this vector represents the mean of each sample taken
R <- 10000 # Number of samples taken

# Bootstrap
for (i in 1:R) {
  resample <- sample(va, size = 25, replace = T) # Takes a random sample with replacement
  mean[i] <- mean(resample) # Stores the mean of the sample
}

# Distribution of the mean of the samples
hist(mean)

# Superior and inferior limits of the interval (Bootstrap)
quantile(mean, probs = c(0.025, 0.975))

# Superior and inferior limits of the interval (Student's T)
CI <- t.test(rv, conf.level = 0.95)
CI$conf.int


# === HYPOTHESIS TESTING ===

# Hypothesis test is a method of statistical inference used to determine a possible conclusion from two different, and likely conflicting, hypotheses.
# It helps in decision making in order to reject or not hypotheses in a scientific experiment. 

# 1) Validating if a random variable follows a normal distribution

# Setting a seed
set.seed(10)

# Generating a random variable (Normal Distribution) with:
# n = 70
# mean = 40
# standard deviation = 8

rv_normal <- rnorm(n = 70, mean = 40, sd = 8)

# Generating a random variable (F Distribution) with:
# n = 15 
# 2 degrees of freedom on the numerator 
# 10 degrees of freedom on the denominator

rv_F <- rf(n = 15, df1 = 2, df2 = 10)

# Let's take a look on the histogram of each variable:

hist(rv_normal) # Note how the data is distributed around the mean (Normal Distribution)

hist(rv_F) # Note how the data is NOT distributed around the mean (F Distribution)

# Visualizing the QQ Plot:

# A Q-Q (quantile-quantile) plot is a probability plot, which is a graphical method for comparing 
# two probability distributions by plotting their quantiles against each other.

qqnorm(rv_normal) # Note how the data points follow the line
qqline(rv_normal) # This function adds the line

qqnorm(rv_F) # Note how the data points DO NOT follow the line
qqline(rv_F) # This function adds the line

# Next, we'll apply the Shapiro-Wilk hypothesis test. The test works under the hypotheses:
# H0: The population is normally distributed
# H1: The population is NOT normally distributed

# Now we need to set a significance level for our analysis. For this test, we'll assume 5%.

# If the p-value is lower than 5% we can reject H0 and assume that H1 is true.

shapiro.test(rv_normal) # Note that the p-value is higher than 5%, which means that we cannot reject H0. Therefore, we can assume that the population is normally distributed.

shapiro.test(rv_F) # Note that the p-value is LOWER than 5%, which means that we CAN reject H0 and assume that the population is NOT normally distributed.


# 2) T Test for difference of means (two independent samples)

# We can use the T distribution to check whether two means are statistically different or whether the difference between them is due to chance. 

# Consider the following context:

# We want to better understand the difference of sales of a product by placing it on shelf A or shelf B.
# For 25 days the product was placed on shelf A, the average of sales was 150.1 and the standard deviation was 17.
# For 30 days the product was placed on shelf B, the average of sales was 182.1 and the standard deviation was 19.2.

# Our goal is to test the following hypotheses:

# H0: The sales on shelf A and on shelf B are equal.
# H1: The sales on shelf A and on shelf B are different.

rm(list = ls()) # Clearing R memory

# Setting the objects:

mu1 <- 150.1 # mean of sales (shelf A)
mu2 <- 182.1 # mean of sales (shelf B)
s1 <- 17 # std deviation of sales (shelf A)
s2 <- 19.2 # std deviation of sales (shelf B)
n1 <- 25 # number of observations (shelf A)
n2 <- 30 # number of observations (shelf B)

# Manually calculating:

# Calculating the t value:
t <- (mu1 - mu2) / sqrt(s1 ^ 2 / n1 + s2 ^ 2 / n2)
t # Visualizing t value

# Calculating the degrees of freedom:
df <- (s1 ^ 2 / n1 +  s2^2 / n2)^2 / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2-1))
df # Visualizing the degrees of freedom

# Obtaining the quantile for a T distribution with 'df' degrees of freedom and alpha =  5% :
quantile <- qt(0.975,df = df)
quantile

# This is the aspect of a T distribution with 53 observations and n-1 degrees of freedom:
plot(density(rt(n = 53,df = df)),xlim = c(-7,7))

# Note where the quantiles are placed (blue lines)
abline(v = quantile,col = 'blue',lwd = 2)
abline(v = -quantile,col = 'blue',lwd = 2)
abline(v = t, col = 'red') # Note how the t-value is much lower, therefore the t-value is on the rejection zone 

# Obtaining the p-value
2 * pt(q = t, df = df) # The p-value is much lower than 5%. In this case, we can reject H0 and assume that H1 is true.

# Next, we'll perform the same experiment using the t.test() function:

shelf_A <- rnorm(n= 25, mean = 150.1, sd = 17)
shelf_B <- rnorm(n = 30, mean = 182.1, sd = 19.2)


# This function returns both the t-value and the p-value:
t.test(shelf_A, shelf_B, alternative = 'two.sided')

# The p-value is much lower than 5%. In this case, we can reject H0 and assume that H1 is true.

# This is the aspect of a T distribution with n observations and n-1 degrees of freedom
n <- 15
plot(density(rt(n = n, df = n-1)))

# Note that as we increase the degrees of freedom, the distribution gets closer to a normal distribution:

n <- 500
plot(density(rt(n = n, df = n-1)))

n <- 20000
plot(density(rt(n = n, df = n-1)))


# 3) T Test for difference of means (two dependent samples)

# We may be interested in measuring the effect of a treatment applied to a sample.

# For the next example, consider the following context:

# The sales manager wants to provide a special training to all 20 salespeople, in order to reduce the commercial discount.
# Before the special training the staff had a discount average of 123 and standard deviation of 18.
# After the special training the staff had a discount average of 110 and standard deviation of 28.

# We want to test the following hypotheses:

# H0: The discount average before and after the training are equal.
# H1: The discount average after the training is LOWER than the discount average before the training.

rm(list = ls()) # Clearing R memory

# We'll use the 'truncnorm' library to generate a normally distributed variable between a given interval.

set.seed(100)

# Generating a normally distributed variable where the minimum value is 100 and the maximum is 140, and:
# n = 20 
# mean = 123
# standard deviation = 18

before_training <- rtruncnorm(n=20, a=100, b=140, mean=123, sd=18) # This variable represents the discounts BEFORE the training

# Generating a normally distributed variable where the minimum value is 110 and the maximum is 130, and:
# n = 20 
# mean = 110
# standard deviation = 28

after_training <- rtruncnorm(n=20, a=110, b=130, mean=110, sd=28) # This variable represents the discounts AFTER the training

# Calculating the difference between the discounts after and before the training, for each individual

diff <- after_training - before_training

# Visualizing the distribution of the discount difference
hist(diff)

shapiro.test(diff) # Testing if the discount difference is normally distributed

# Applying the T Test with the following arguments:

t.test(after_training, before_training,
       paired = TRUE,
       alternative = "less", # one-sided left
       conf.level = 0.95 # 95% of confidence
)

# The p-value is HIGHER than 5%, which means that we do not have enough evidence to reject H0.
# Therefore, we can assume that The discount average before and after the training are equal.
# The training provided to the salespeople was ineffective.



# 4) Chi-Squared Test for association between categorical variables

# In scientific situations it is common to find situations in which we need to identify whether two qualitative variables are associated or independent. 
# For these situations, we can use the Chi-Square test. For this next example, consider the following context:

# In order to better understand the behavior of our customers, we want to investigate whether a product sells more when the customer is married or single. 
# For this, we observed 50 customers, married and single, who bought and did not buy the product. We think the customer buys regardless of marital status.

# Thereby, we want to test the following hypotheses:

# H0: The customer's marital status is NOT related to whether or not to buy
# H1: The customer's marital status IS related to whether or not to buy

rm(list = ls()) # Clearing R memory

# Importing the 'people.csv' file (remember to rerun the 'path' variable at the beginning of the script):

data <- read.csv(file = paste(path, "sales.csv", sep = ""), sep =';')

# Visualizing the data

View(data)

# Generating the contingency table:

table <- table(data$Client, data$Bought)
table
barplot(table)

# To perform the Chi-Squared test we can use the chisq.test() funcion:

test <- chisq.test(table, correct = F)
test

# As you can see, by observing the p-value even with a confidence level of 99% we have enough evidence to reject H0.
# Therefore, we can assume with 99% of confidence that the customer's marital status IS related to whether or not to buy (H1).


# 5) F Test for variance analysis (ANOVA)

# There are situations where we may want to test the difference between three or more means 
# and we have seen previously that the T Test allows us to compare only pairs of means. 
# We can then use an analysis of variance, also known as ANOVA (Analysis of Variance), 
# which uses an F Fest to identify whether there is significant variability when comparing the means of n populations. 

# For this next example, consider the following context:

# A restaurant owner asked you to study the expenditure on a special drink of three groups of customers (populations).
# You observed the spending on that drink from 17 singles, 98 married and 15 divorced customers.

# Thereby, we want to test the following hypotheses:

# H0: There is NO difference in average expenditure with drinks in any of the populations
# H1: There IS difference in average expenditure with drinks in AT LEAST ONE of the populations.

rm(list = ls()) # Clearing R memory

# Importing the 'drinks.csv' file (remember to rerun the 'path' variable at the beginning of the script):

data_anova <- read.csv(file = paste(path, "drinks.csv", sep = ""), sep =';')

View(data_anova) # Visualizing the data

# We'll use the 'ggplot2' library for data visualization

library(ggplot2)

ggplot(data = data_anova, aes(x = expenses, fill = status)) +
  geom_density(alpha=0.4)+
  xlim(-50,300)

# By visually analyzing the plot, we can see certain similarity between the Married and Divorced populations.
# However, the Single population seem to have a different behavior.

# We can also analyze the populations using a box plot:

boxplot(data_anova$expenses ~ data_anova$status)

# The aov() function generates the ANOVA table:

anova <- aov(expenses~ # Target variable
               status, # Variable that we want to test if has influence on target
             data = data_anova)

# Visualizing the ANOVA table:
summary(anova)

# Note that the p-value (Pr > F) is practically zero, which means:
# With 99% of confidence, we have enough evidence to reject H0. The average expenditure with drinks is different in at least one of the populations.


# === LINEAR REGRESSION ===

# In statistics, linear regression is a linear approach for modelling the relationship between a scalar response and one or more explanatory variables 
# (also known as dependent and independent variables)

rm(list = ls()) # Clearing R memory

# For the next examples we'll use the 'coffee.csv' data set, that contains the following variables:
# coffee_sales
# coffee_price
# on_sale
# milk_price

# By using linear regression and correlation, we'll try to answer some of these questions: 
# Is there a relationship between the coffee price and the quantity sold? 
# How does price explain the coffee sales? 
# Can I use price to predict sales? 

# Importing the 'coffee.csv' file (remember to rerun the 'path' variable at the beginning of the script):

data <- read.csv(file = paste(path, "coffee.csv", sep = ""), sep =';')

View(data) # Visualizing the data

# Exploring the data:

# Relationship between coffee price and coffee sales:

plot(y = data$coffee_sales,
     x = data$coffee_price,
     main = 'Coffee Sales VS Coffee Price',
     xlab = 'Coffee Price',
     ylab = 'Coffee Sales',
     pch = 16)
grid()

# By using the 'ggplot2' library we can generate a more sophisticated plot:

g1 <- ggplot(data = data, aes(y = coffee_sales, x = coffee_price)) + geom_point()

g1 + geom_smooth(method = 'lm') # Adding a regression line using the 'geom_smooth' argument

# By visually inspecting the plot, we can observe that as the prices go up the sales go down.

ggplotly(g1) # This function comes from the 'plotly' library. It allows the use of mouse hover on the plot dots.

# Next, we'll calculate the Pearson correlation between the variables:

# Pearson's correlation coefficient is a numerical value ranging from -1 to 1. 
# The closer to -1 the correlation, the stronger the correlation in a negative way (when one variable increases, the other decreases). 
# The closer to 1, it means a stronger positive correlation between the two variables (when one variable increases, the other also increases). 

# Pearson correlation between coffee sales and coffee prices:

cor(data$coffee_sales, data$coffee_price) # Strong negative correlation

# Relationship between coffee sales and milk price:

plot(y = data$coffee_sales,
     x = data$milk_price,
     main = 'Coffee Sales VS Milk Price',
     xlab = 'Milk Price',
     ylab = 'Coffe Sales',
     pch = 16)
grid()

# Pearson correlation between coffee sales and milk price:

cor(data$milk_price, data$coffee_sales) # Weak negative correlation

# Plotting a 3D plot with coffee sales, coffee price and milk price. Use the mouse to drag and turn the plot:

plot_ly(data, z = ~coffee_sales,
        x = ~coffee_price,
        y = ~milk_price) %>% add_markers()

# Relationship between coffee sales when the coffee is on sale (Yes) and not on sale (No):

boxplot(data$coffee_sales ~ data$on_sale)

# We can also use the 'ggplot' and 'plotly':

g2 <- ggplot(data = data, aes(y = coffee_sales, x = on_sale, col = on_sale)) + geom_boxplot()
ggplotly(g2)

par(mfrow = c(2,2)) # Configuring the screen to show multiple charts

plot(y = data$coffee_sales,
     x = data$coffee_price,
     pch = 16,
     main = 'Coffee Sales vs Coffee Price')
plot(y = data$coffee_sales,
     x = data$milk_price,
     pch = 16,
     main = 'Coffee Sales vs Milk Price')
boxplot(data$coffee_sales ~ data$on_sale,
        main = 'Coffee Sales vs Sale')
hist(data$coffee_sales,
     main = 'Coffee Sales Distribution')

dev.off()

# Fitting the linear regression model:

model <- lm(coffee_sales ~ coffee_price + milk_price + on_sale, data = data)

# Model Summary:

summary(model)

# Residual Diagnostic

par(mfrow = c(2,2))

  plot(model, pch = 16)

dev.off()

# Next, we'll create a new data frame with brand new data, although, this data frame will not have the target variable (coffee_sales):


prediction_data <- data.frame(coffee_price = c(4.77, 4.67, 4.75),
                                  on_sale = c("No", "No", "Yes"),
                                  milk_price = c(4.74, 4.81, 4.36) )

View(prediction_data) # Visualize the prediction data set

# Estimating the target variable (coffee_sales) for each observation in the 'prediction_data' data set:

predict <- predict(model, newdata = prediction_data)
View (data.frame(prediction_data, predict))

# So far, all variables were considered relevant for predicting the target. But what if we had a irrelevant variable in the data set?
# Next, we'll create a new irrelevant variable and explain how to spot it and remove it from the model.

# Creating a new variable following a Poisson distribution:
new_variable = rpois(n = 30, lambda = 2)

# Creating another model, including the 'new_variable'
model2 <- lm(coffee_sales ~ coffee_price + milk_price + on_sale + new_variable, data = data)

summary(model2) # Note that de p-value of the 'new_variable' is very high, which means that this variable does not contribute to the prediction model.

# Next, we'll apply the Stepwise Method for auto selection of the most relevant variables

model3 <- step(model2, direction = 'both')

summary(model3) # Note that the Stepwise removed the 'new_variable' from the model.
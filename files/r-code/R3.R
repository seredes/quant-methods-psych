################################
# Serena De Stefani
# 8/6/2017
# Third R session
###############################


install.packages("stats")
library(stats)

# SET WORKING DIRECTORY (must be done each time) -----------------
# Go to Session/Set Working Directory/Choose Directory

#**********************************
# SAMPLING DISTRIBUTION FOR THE MEAN
#**********************************

# Download file pop1.csv from Sakai ------------------------------
# or from http://pluto.huji.ac.il/~msby/StatThink/
# It is a CSV file that contains the information
# on sex and height of an entire adult population of some imaginary city.

# Read csv file into an object
pop.1 <- read.csv("pop1.csv")
# Let's look at a summary of the data:
summary(pop.1)
# look at the first five rows:
head(pop.1)
# take a sample of 100 elements and store it into a variable, X.samp
X.samp <- sample(pop.1$height,100)
# look at the variable:
X.samp

# calculate the mean of the sample
mean(X.samp)
# but what was the mean of the orginal population?
mean(pop.1$height)

# Let's do this again
X.samp <- sample(pop.1$height,100)
X.bar <- mean(X.samp)
X.bar

# and again
X.samp <- sample(pop.1$height,100)
X.bar <- mean(X.samp)
X.bar

# Now I want to build a sampling distribution
# I want to take 100,000 (10^5) samples, calculate their means, and
# draw an histogram
# What will be the shape of the histogram?

# Create a vector of zeros 
X.bar <- rep(0,10^5) 
# As I calculate the means in the loop, the mean values will substitute the zeroes
# Look at the first five rows
head(X.bar)
# The following loop takes a sample and calculates the mean, storing the mean in our
# vector, and does so for 100,000 times
for(i in 1:10^5)
  {
       X.samp <- sample(pop.1$height,100)
       X.bar[i] <- mean(X.samp)
   }


hist(X.bar)


#**********************************
# Z-TEST FOR PROPORTIONS
#**********************************

# prop.test(x, n, p = NULL,
#           alternative = c("two.sided", "less", "greater"),
#           conf.level = 0.95, correct = TRUE)

# x = number of successes
# n = total number of cases
# default is two-sided
# correct: Yates’s continuity correction

# ONE-PROPORTION Z-TEST
# 447 of the 998 individuals who ate beef curry were observed to have food poisoning symptoms. 
# We want to compare this rate with an hypothetical rate of 0.1.

# H0: The proportion of individuals who eat beefcurry and get sick is 0.1: true p = 0.1
# Ha: The proportion of individuals who eat beefcurry and get sick is not 0.1: true p ≠ 0.1

prop.test(447, 998, .1)

# TWO-PROPORTIONs Z-TEST
# In a hospital in North Carolina, the doctors registered the patients 
# who were involved in a car accident and whether they used seat belts. 

# The following matrix represents the number of survivors 
# and deceased patients in each group:

survivors <- matrix(c(1781,1443,135,47), ncol=2)
colnames(survivors) <- c('survived','died')
rownames(survivors) <- c('no seat belt','seat belt')
survivors

# are the proportions any different?
prop.test(survivors)

#**********************************
# T-TEST FOR MEANS
#**********************************

# t.test(x, y = NULL, 
#          alternative = c("two.sided", "less", "greater"), 
#          mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# If y is excluded, the function performs a one-sample t-test on the data contained in x, 
# if it is included it performs a two-sample t-tests using both x and y.

# mu: true value of the mean (or difference in means if you are performing a two sample test) 
# under the null hypothesis. 


# ONE SAMPLE T-TEST
# Ex. An outbreak of Salmonella-related illness was attributed to ice cream 
# produced at a certain factory. 
# Scientists measured the level of Salmonella in 9 randomly sampled batches of ice cream. 
# The levels (in MPN/g) were:
# 0.593 0.142 0.329 0.691 0.231 0.793 0.519 0.392 0.418
# ￼￼￼￼￼￼
# Is there evidence that the mean level of Salmonella in the ice cream 
# is greater than 0.3 MPN/g?
# Let mu ￼ be the mean level of Salmonella in all batches of ice cream.
# Here the hypothesis of interest can be expressed as:
# H0: mu￼ = 0.3 Ha:mu ￼ > 0.3
# Hence, we will need to include the options alternative="greater", mu=0.3. 
# Below is the relevant R-code:

# create a vector for the data:
x = c(0.593, 0.142, 0.329, 0.691, 0.231, 0.793, 0.519, 0.392, 0.418) 
t.test(x, alternative="greater", mu=0.3)

# why the CI goes to infinity? Because the test is one-tailed

# let's check a two-tailed test
t.test(x, alternative="two.sided", mu=0.3)


# TWO SAMPLES T-TEST

# Ex. Six subjects were given a drug (treatment group) 
# and an additional 6 subjects a placebo (control group). 
# Their reaction time to a stimulus was measured (in ms). 
# Control: 91, 87, 99, 77, 88, 91
# Treatment: 101, 110, 103, 93, 99, 104
# We want to perform a two-sample t-test for comparing 
# the means of the treatment and control groups.
# 
# Let mu1 be the mean of the population taking medicine 
# and mu2 the mean of the untreated population. 
# Here the hypothesis of interest can be expressed as:
#   H0: mu1- mu2=0 Ha: mu1- mu2<0

# Here we will need to include the data 
# for the treatment group in x 
# and the data for the control group in y. 
# We will also need to include the options alternative="less", mu=0. 
# Finally, we need to decide whether or not the POPULATION standard deviations
# are the same in both groups.
# ￼￼￼￼￼￼
# Assuming equal standard deviation in the POPULATION of reference
# create a vector for the data:
Control = c(91, 87, 99, 77, 88, 91)
Treat = c(101, 110, 103, 93, 99, 104)
t.test(Control,Treat,alternative="less", var.equal=TRUE)

# not assuming equal standard deviation
t.test(Control,Treat,alternative="less")

# PAIRED T-TEST
# Ex. A study was performed to test whether cars get better mileage 
# on premium gas than on regular gas. 
# Each of 10 cars was first filled with either regular or premium gas, 
# decided by a coin toss, and the mileage for that tank was recorded. 
# regular gas: 16, 20, 21, 22, 23, 22, 27, 25, 27, 28
# premium gas: 19, 22, 24, 24, 25, 25, 26, 26, 28, 32
# The mileage was recorded again for the same cars using the other kind of gasoline. 
# We use a paired t- test to determine whether cars get significantly better mileage 
# with premium gas.

# Below is the relevant R-code
# create a vector for the data:
reg = c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
prem = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
t.test(prem,reg,alternative="greater", paired=TRUE)
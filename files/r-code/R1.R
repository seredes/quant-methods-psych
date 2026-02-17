
################################
# Serena De Stefani
# 7/14/2020
# First R session
###############################

# Clear your working environment


# SET WORKING DIRECTORY (must be done each time) -----------------
# Go to Session/Set Working Directory/Choose Directory


# USEFUL SHORTCUTS -----------------------------------------------
# to run code: 
# place your cursor on a segment of code
# or highlight a section of the code you want to run
# and press: Ctrl + enter

# to comment/uncomment:
# place your cursur on a segment of code
# or highlight a section of the code and press: 
# MAC: command + Shift + C
# PC: Ctrl + Shift + C 

# to type the assignment <- symbol:
# MAC: option and the minus sign: option/alt + -
# PC: press alt and the minus sign: alt + -

# Many other shortcuts here:
# https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts

# Make sure that the datasets are in the working directory and that
# the file name is correct

# LOADING DATA ----------------------------------------------------
# You can read more about this dataset on page 140 of the book
# "Learning statistics with R"
load("parenthood.Rdata")

#returns a list of all the objects you just loaded 
# (and anything else in your environment)
ls() 

# we can also load data from a CSV file and rename it
# CS <- read.table("parenthood.csv", sep=",", header=TRUE)

# INSTALL PACKAGES -----------------------------------------------
# if we want to use additional features and functionalities
# we can install a package:
install.packages("lsr")
install.packages("psych")

# after installing a package we need to load the corresponding
# library. --> A library must be loaded every time
library(lsr)
library(psych)

# DATA -----------------------------------------------------------
# Let's use the function 'who' in the 'lsr' library. It will tells us
# what's in the dataset
who(expand = TRUE)

# in this case you have two datasets that are the same, one loaded
# from a Rdata file and one loaded from a CSV file

# These are the variables:
# $dan.sleep    number of hours the father (Dan) slept in the last 100 days     
# $baby.sleep   number of hours the baby slept in the last 100 days     
# $dan.grump    level of Dan's "grumpyness", measured on a scale from 1 to 100      
# $day          index of 100 days 


# Now let's look at the data in detail, using the function
# "describe" from the "psych" library
# For a full description of "describe"
# see: https://www.youtube.com/watch?v=vsaCS6l9XaY

describe(parenthood)


# HISTOGRAM ------------------------------------------------------
# Let's continue with some graphs.
# Graph a histogram of the number of hours the father slept:
hist(parenthood$dan.sleep
     )

# Now graph a histogram of the number of hours the baby slept:
hist(parenthood$baby.sleep
     )

# Let's adjust the limits on the x-axis 
# so that we can COMPARE the two histograms.
# I am adding the argument <xlim=c(0,14)>, where the 
# first number represents the minumum value displayed on the x-axis
# and the second number represents the maximum value displayed on the x-axis

# Look at the y-axis, that needs to be adjusted as well,
# with the argument <ylim=c(0,30)>

hist(parenthood$dan.sleep,
     xlim=c(0,14),
     ylim=c(0,30)
     )
hist(parenthood$baby.sleep,
     xlim=c(0,14),
     ylim=c(0,30)
     )

# I want to show these two graphs on the same panel
# I will use <par(mfrow=c(1,2))> where the first number
# represents the ROWS and the second number represents the COLUMNS.
# If I want to see two graphs side by side, I will need one row
# and two columns.


par(mfrow=c(1,2))

hist(parenthood$dan.sleep, 
     xlim=c(0,14),
     ylim=c(0,50)
)
hist(parenthood$baby.sleep, 
     xlim=c(0,14),
     ylim=c(0,50)
)

# After visualizing the graphs I want to reset the graphing
# device so that I can visualize the graphs one by one again.
# I will write <dev.off()> 

dev.off()

########################################################
# From now on I will only work on the graph for
# parenthood$dan.sleep
########################################################

# Now let's fix the bins using the argument <breaks=seq(0,14,by=1)>,
# where the first number represents the minumum value on the x-axis
# the second number represents the maximum value on the x-axis
# and the number after <by=> represents the width of the bins

hist(parenthood$dan.sleep, 
     xlim=c(0,14),
     breaks=seq(0,14,by=1)
     )

# Now I want to add the main title and axes titles

hist(parenthood$dan.sleep, 
     xlim=c(0,14), 
     breaks=seq(0,14,by=1),
     main = "Number of hours father is sleeping", 
     xlab = "Number of hours",
     ylab = "Frequency")

# Let's also add some color. 

hist(parenthood$dan.sleep, 
     xlim=c(0,14), 
     breaks=seq(0,14,by=1),
     main = "Number of hours father is sleeping", 
     xlab = "Number of hours",
     ylab = "Frequency",
     col= "Blue"
     )


# BOXPLOT --------------------------------------------------------
# I can also look at a boxplot

boxplot(parenthood$dan.sleep
        )

# And I want to consider the same interval I used for the histogram:
boxplot(parenthood$dan.sleep, 
        ylim=c(0,14)
)

# I can add the titles and color:

boxplot(parenthood$dan.sleep, 
        ylim=c(0,14),
        main = "Number of hours father is sleeping", 
        ylab = "Number of hours",
        col= "Blue"
        )

# SCATTERPLOT ---------------------------------------------------
# Now let's see if there is a correlation between the number of hours
# the father slept and how much grumpy he was:

plot( parenthood$dan.sleep,
      parenthood$dan.grump
      )

# Let's add titles, some color, and nicer symbols
# using the argument <pch=18>
# You can see a list of other symbols here:
# http://www.endmemo.com/program/R/pchsymbols.php

plot( parenthood$dan.sleep,
      parenthood$dan.grump,
      main = "Dan's grunpiness per hours slept",
      xlab = "Dan sleep (hours)",
      ylab = "Dan grumpiness (0-100)",
      col= "red",
      pch=18
)

# Then, how strong is the correlation?

cor(parenthood$dan.sleep,parenthood$dan.grump)



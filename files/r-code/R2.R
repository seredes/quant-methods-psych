
################################
# Serena De Stefani
# 7/25/2017
# Second R session
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


# SCATTERPLOT ---------------------------------------------------
# Now let's see if there is a correlation between the number of hours
# the father slept and how much grumpy he was:

plot( parenthood$dan.sleep,
      parenthood$dan.grump
)

# Let's add titles, some color, and nicer symbols
# using the argument <pch=20>
# You can see a list of other symbols here:
# http://www.endmemo.com/program/R/pchsymbols.php

plot( parenthood$dan.sleep,
      parenthood$dan.grump,
      main = "Dan's grunpiness per hours slept",
      xlab = "Dan sleep (hours)",
      ylab = "Dan grumpiness (0-100)",
      xlim = c(0,14),   # scale the x-axis
      ylim = c(0,100),    # scale the y-axis
      pch = 20,         # change the plot symbol
      frame.plot = FALSE, # don’t draw a box
      col= "red"
)


# Let's add the line of best fit

abline(
      lm(parenthood$dan.grump~parenthood$dan.sleep),
      col="red"    # regression line (y~x) 
      ) 

# Let's look at the lowess line (best fit by section)
# to see if we can spot trends
lines(lowess(parenthood$dan.sleep,
             parenthood$dan.grump), 
      col="blue"   # lowess line (x,y)
      ) 

# We can identify single points
identify(parenthood$dan.sleep,
         parenthood$dan.grump)

# Now we save the data from the regression function
# in lm.grump.sleep
lm.grump.sleep <- lm(parenthood$dan.grump ~ parenthood$dan.sleep)
# We look at the residuals:
plot(lm.grump.sleep, which = 1)  
# We look at the QQplot
plot(lm.grump.sleep, which = 2)

# There is a fancier way to design the scatterplot
# First we need to install a package...
install.packages("car")
# ... and load a library
library(car)


scatterplot( parenthood$dan.sleep,
      parenthood$dan.grump,
      main = "Dan's grunpiness per hours slept",
      xlab = "Dan sleep (hours)",
      ylab = "Dan grumpiness (0-100)",
      xlim = c(0,12),   # scale the x-axis
      ylim = c(0,100),    # scale the y-axis
      pch = 20,         # change the plot type
      frame.plot = FALSE, # don’t draw a box
      col= "red"
)


# Then, how strong is the correlation?

cor(parenthood$dan.sleep,parenthood$dan.grump)

cor( x = parenthood ) # calculate correlation matrix
pairs( x = parenthood ) # draw corresponding scatterplot matrix

# This is a comment

# Here is how we add numbers
3.5 + 11.2

# Here is how we multiply numbers
3.5*2.6

# Here is how we raise a number to a power
6^2

# Here is how we compute the square root of a number
sqrt(36)

# Here is how we create a numeric vector of data
my_data <- c(10.2,6.8,3.7,9.2,5.6,7.3,8.3)

# Here is how we create a character vector
my_char_data <- c("Math","Biology","Math","History","Biology","Philosophy","Philosophy","Math")

# Here is how we determine the length of a vector
length(my_data)
length(my_char_data)

# Here is how we load packages
library(tidyverse)
library(openintro)
library(ggformula)

# To obtain help on a function or data set from a package:
?loan50 # help on loan50 data set
?gf_point # help on scatterplot plotting function 

# Here are some common data frame commands
dim(possum)
glimpse(possum)
summary(possum)

# Here is how to compute the mean, median, variance, standard deviation, and IQR of 
# a numerical variable
mean(possum$head_l) # mean 
median(possum$head_l) # median 
var(possum$head_l) # variance
sd(possum$head_l) # standard deviation
IQR(possum$head_l) # interquartile range

# Here is how to obtain a boxplot and histogram for a numerical variable
gf_boxplot(~head_l,data=possum) # boxplot
# you may want to rotate the orientation, if so use
gf_boxplot(~head_l,data=possum) + coord_flip()
gf_histogram(~head_l,data=possum) # histogram
# you may want to change the number of bins, if so use
gf_histogram(~head_l,data=possum,bins=10)

# Here is how you obtain a barplot for a categorical variable
gf_bar(~sky,data=birds) # barplot

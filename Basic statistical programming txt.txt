library(datasets)

#load data#####

head(iris)

#plot data with PLOT() ####

?plot
plot(iris$Species) #categorical variable
plot(iris$Petal.Length)    #quantitative variable
plot(iris$Species, iris$Petal.Width)  #cat x quant
plot(iris$Petal.Length, iris$Petal.Width)  #Quant pair
plot(iris) #run command on entire dataset or frame


#plot with options
plot(iris$Petal.Length, iris$Petal.Width,
     col = "#cc0000",  #Hex code for datalab.cc red
     pch = 19,        #Use solid circles for points
     main = "Iris: Petal Length vs. Petal Width",
     xlab = "Petal Lenth",
     ylab = "Petal Width")

#plot with formular plot()####

plot(cos, 0, 2*pi)
plot(exp, 1, 5)
plot(dnorm, -3, +3)

#formular plot with options
plot(dnorm, -3, +3,
     col = "#cc0000",
     lwd = 5,
     main = "Standard Normal Distribution",
     xlab = "z-scores",
     ylab = "Density")


?mtcars
head(mtcars)

#To plot a barchart we need to have a table thats specific to a variable###
# we need a table with frequencies for each category
cylinders <-table(mtcars$cyl)   # to create a table with the data set using the cyl variable###
barplot(cylinders)              # to plot a bar chart###
plot(cylinders)                 # Default X-Y plot (lines)


?iris
head(iris)

#basic histogram #####
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)

#histogram by group  put graphs in 2 rows and 1 column it is use to change graphical parameters##### 
par(mfrow = c(2, 1))

#histogram for each species using options. this allows us to have or visualize the three data in the same scale and help us compare simultaneously####
hist(iris$Petal.Width [iris$Species == "setosa"],
     xlim = c(0, 2),
     breaks = 9,
     main = "Petal width for setosa",
     xlab = " ",
     col = "red")

hist(iris$Petal.Width [iris$Species == "versicolor"],
     xlim = c(0, 2),
     breaks = 9,
     main = "Petal width for versicolor",
     xlab = " ",
     col = "green")


table(iris$Species)
colnames(iris)
#histogram by group  put graphs in 2 rows and 1 column it is use to change graphical parameters##### 
par(mfrow = c(1, 1))


?mtcars
head(mtcars)

# Using scatter plot. we would check first for univariate distributions.###
hist(mtcars$wt)
hist(mtcars$mpg)

#checking for two variables - X-Y plot for two quantitative variables##
plot(mtcars$wt, mtcars$mpg)

#add some options
plot(mtcars$wt, mtcars$mpg,
     
     
     pch = 19,                  #solid circle - point character
     ceX = 1.5,                #make 150% size -  size of things
     col = "#CC0000",            #red color
     main = "MPG as a function of weight of cars",
     xlab = "Weight (in 1000 pounds)",
     ylab = "MPG")


#OVERLAYING PLOT######
#Data set ######

?lynx
head(lynx)

#lets make a default chart with histogram#####
hist(lynx)


#add some options###
hist(lynx,
     breaks = 14,          # "Suggests" 14 bins like how many bins
     freq   = FALSE,       # Axis shows density, not freq.
     col    = "thistle1",  # Color for histogram
     main   = paste("Histogram of Annual Canadian Lynx",
                    "Trappings, 1821-1934"),
     xlab   = "Number of Lynx Trapped")

# overlaying chart - Add a normal distribution
curve(dnorm(x, mean = mean(lynx), sd = sd(lynx)),
      col = "thistle4",  # Color of curve
      lwd = 2,           # Line width of 2 pixels
      add = TRUE)        # Superimpose on previous graph

# overlaying chart - Add two kernel density estimators
lines(density(lynx), col = "blue", lwd = 2)
lines(density(lynx, adjust = 3), col = "purple", lwd = 2)

# overlaying chart - Add a rug plot
rug(lynx, lwd = 2, col = "gray")


head(iris)

# SUMMARY() ################################################

summary(iris$Species)       # Categorical variable
summary(iris$Sepal.Length)  # Quantitative variable
summary(iris)               # Entire data frame




# Use pacman to load add-on packages as desired - packages i load every time; to use "pacman"
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate,plotly, rio, rmarkdown, shiny, stringr, tidyr)

pacman::p_load(pacman, psych) 

# LOAD DATA ################################################


head(iris)

# PSYCH PACKAGE ############################################
p_load(psych)

# Get info on package
p_help(psych)           # Opens package PDF in browser
p_help(psych, web = F)  # Opens help in R Viewer

# DESCRIBE() ###############################################

# For quantitative variables only.

describe(iris$Sepal.Length)  # One quantitative variable
describe(iris)               # Entire data frame

#SELECTING CASES#####
head(iris)

# ALL DATA #################################################

hist(iris$Petal.Length)
summary(iris$Petal.Length)

summary(iris$Species)  # Get names and n for each species

# SELECT BY CATEGORY #######################################

# Versicolor
hist(iris$Petal.Length[iris$Species == "versicolor"],
     main = "Petal Length: Versicolor")

# Virginica
hist(iris$Petal.Length[iris$Species == "virginica"],
     main = "Petal Length: Virginica")

# Setosa
hist(iris$Petal.Length[iris$Species == "setosa"],
     main = "Petal Length: Setosa")

# SELECT BY VALUE ##########################################

# Short petals only (all Setosa)
hist(iris$Petal.Length[iris$Petal.Length < 2],
     main = "Petal Length < 2")

# MULTIPLE SELECTORS #######################################

# Short Virginica petals only
hist(iris$Petal.Length[iris$Species == "virginica" & 
                         iris$Petal.Length < 5.5],
     main = "Petal Length: Short Virginica")

# CREATE SUBSAMPLE #########################################

# Format: data[rows, columns]
# Leave rows or columns blank to select all LIKE(SETOSA)
i.setosa <- iris[iris$Species == "setosa", ]

# EXPLORE SUBSAMPLE ########################################

head(i.setosa)
summary(i.setosa$Petal.Length)
hist(i.setosa$Petal.Length)


# DATA FORMATS -  DATA TYPES ###############################################

# Numeric

n1 <- 15  # Double precision by default
n1
typeof(n1)

n2 <- 1.5
n2
typeof(n2)

# Character

c1 <- "c"
c1
typeof(c1)

c2 <- "a string of text"
c2
typeof(c2)

# Logical ;if we put cot it becomes a character####

l1 <- TRUE
l1
typeof(l1)

l2 <- F
l2
typeof(l2)

# DATA STRUCTURES ##########################################

## Vector c- stands for combine ##################################################

v1 <- c(1, 2, 3, 4, 5)
v1
is.vector(v1)

v2 <- c("a", "b", "c")
v2
is.vector(v2)

v3 <- c(TRUE, TRUE, FALSE, FALSE, TRUE)
v3
is.vector(v3)

## Matrix ##################################################

m1 <- matrix(c(T, T, F, F, T, F), nrow = 2)
m1

m2 <- matrix(c("a", "b", 
               "c", "d"), 
             nrow = 2,
             byrow = T)
m2

## Array ###################################################

# Give data, then dimemensions (rows, columns, tables)
a1 <- array(c( 1:24), c(4, 3, 2))
a1

## Data frame ##############################################

# Can combine vectors of the same length

vNumeric   <- c(1, 2, 3)
vCharacter <- c("a", "b", "c")
vLogical   <- c(T, F, T)

dfa <- cbind(vNumeric, vCharacter, vLogical)
dfa  # Matrix of one data type

df <- as.data.frame(cbind(vNumeric, vCharacter, vLogical))
df  # Makes a data frame with three different data types

## List ####################################################

o1 <- c(1, 2, 3)
o2 <- c("a", "b", "c", "d")
o3 <- c(T, F, T, T, F)

list1 <- list(o1, o2, o3)
list1

list2 <- list(o1, o2, o3, list1)  # Lists within lists!
list2

# COERCING TYPES ###########################################

## Automatic coercion ######################################

# Goes to "least restrictive" data type

(coerce1 <- c(1, "b", TRUE))
# coerce1  # Parenthese around command above make this moot
typeof(coerce1)

## Coerce numeric to integer ###############################

(coerce2 <- 5)
typeof(coerce2)

(coerce3 <- as.integer(5))
typeof(coerce3)

## Coerce character to numeric #############################

(coerce4 <- c("1", "2", "3"))
typeof(coerce4)

(coerce5 <- as.numeric(c("1", "2", "3")))
typeof(coerce5)

## Coerce matrix to data frame #############################

(coerce6 <- matrix(1:9, nrow= 3))
is.matrix(coerce6)

(coerce7 <- as.data.frame(matrix(1:9, nrow= 3)))
is.data.frame(coerce7)


# CREATE DATA ##############################################

(x1 <- 1:3)
(y  <- 1:9)

# Combine variables
(df1 <- cbind.data.frame(x1, y))
typeof(df1$x1)
str(df1)


# AS.FACTOR ################################################

(x2  <- as.factor(c(1:3)))
(df2 <- cbind.data.frame(x2, y))
typeof(df2$x2)
str(df2)

# DEFINE EXISTING VARIABLE AS FACTOR #######################

x3  <- c(1:3)
df3 <- cbind.data.frame(x3, y)
(df3$x3 <- factor(df3$x3,
                  levels = c(1, 2, 3)))
typeof(df3$x3)
str(df3)

# LABELS FOR FACTOR ########################################

x4  <- c(1:3)
df4 <- cbind.data.frame(x4, y)
df4$x4 <- factor(df4$x4,
                 levels = c(1, 2, 3),
                 labels = c("macOS", "Windows", "Linux"))
df4
typeof(df4$x4)
str(df4)

# ORDERED FACTORS AND LABELS ###############################

x5  <- c(1:3)
df5 <- cbind.data.frame(x5, y)
(df5$x5 <- ordered(df5$x5,
                   levels = c(3, 1, 2),
                   labels = c("No", "Maybe", "Yes")))
df5
typeof(df5$x5)
str(df5)


# ENTERING DATA - COLON OPERATOR ###########################################

# Assigns number 0 through 10 to x1
x1 <- 0:10
x1

# Descending order
x2 <- 10:0
x2

# SEQ ######################################################

?seq  # R help on seq

# Ascending values (duplicates 1:10)
(x3 <- seq(10))

# Specify change in values
(x4 <- seq(30, 0, by = -3))

# ENTER MULTIPLE VALUES WITH C #############################

# c = concatenate (or combine or collect)
?c  # R help on c

x5 <- c(5, 4, 1, 6, 7, 2, 2, 3, 2, 8)
x5

# SCAN #####################################################

?scan  # R help on scan

x6 <- scan()  # After running this command, go to console
# Hit return(ENTER) after each number
# Hit return(ENTER) twice to stop
x6

# REP ######################################################

?rep  # R help on rep
x7 <- rep(TRUE, 5)
x7

# Repeats set
x8 <- rep(c(TRUE, FALSE), 5)
x8

# Repeats items in set
x9 <- rep(c(TRUE, FALSE), each = 5)
x9


# IMPORTING DATA - INSTALL AND LOAD PACKAGES ################################

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate,plotly, rio, rmarkdown, shiny, stringr, tidyr)
pacman::p_load(pacman, rio) 

# ABOUT EXCEL FILES ########################################

# From the official R documentation
browseURL("http://j.mp/2aFZUrJ")

# You have been warned: ಠ_ಠ

# IMPORTING WITH RIO #######################################

# CSV

#does not work####
rio_csv <- import("~/Desktop/mbb.csv") 
head(rio_csv)

#try this ####
rio_csv <- import("/Users/user/Desktop/mbb.csv")
head(rio_csv)


# TXT
rio_txt <- import("/Users/user/Desktop/mbb.txt")
head(rio_txt)

# Excel XLSX
rio_xlsx <- import("/Users/user/Desktop/mbb.xlsx")
head(rio_xlsx)

# DATA VIEWER ##############################################

?View
View(rio_csv)
View(rio_txt)
View(rio_xlsx)

# READ.TABLE FOR TXT FILES #################################

# R's built-in function for text files (used by rio)

# TEXT FILES

# Load a spreadsheet that has been saved as tab-delimited
# text file. Need to give complete address to file. This
# command gives an error on missing data but works on
# complete data.
r_txt1 <- read.table("~/Desktop/mbb.txt", header = TRUE)

# This works with missing data by specifying the separator: 
# \t is for tabs, sep = "," for commas. R converts missing
# to "NA"
r_txt2 <- read.table("/Users/user/Desktop/mbb.txt", 
                     header = TRUE, 
                     sep = "\t")

# READ.CSV FOR CSV FILES ###################################

# R's built-in function for csv files (also used by rio)

# CSV FILES
# Don't have to specify delimiters for missing data
# because CSV means "comma separated values"
trends.csv <- read.csv("/Users/user/Desktop/mbb.csv", header = TRUE)



#HEIRARCHICAL CLUSTERING#######

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate,plotly, rio, rmarkdown, shiny, stringr, tidyr)
pacman::p_load(pacman, tidyverse) 

# LOAD DATA ################################################

?mtcars
head(mtcars)
cars <- mtcars[, c(1:4, 6:7, 9:11)]  # Select variables...THE COLUMNS AND SKIP WHAT YU DONT NEED##
cars <- mtcars[, c(1:4, 6:7, 9:11)]
head(cars)

# COMPUTE AND PLOT CLUSTERS ################################

# Save hierarchical clustering to "hc." This codes uses
# pipes from dplyr.
hc <- cars   %>%  # Get cars data
  dist       %>%  # Compute distance/dissimilarity matrix
  hclust      # Computer hierarchical clusters

plot(hc)          # Plot dendrogram

# ADD BOXES TO PLOT ########################################

rect.hclust(hc, k = 2, border = "gray")
rect.hclust(hc, k = 3, border = "blue")
rect.hclust(hc, k = 4, border = "green4")
rect.hclust(hc, k = 5, border = "darkred")


# PRINCIPLE COMPONENT- INSTALL AND LOAD PACKAGES ################################

# Packages I load every time; uses "pacman"
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr) 


# LOAD DATA ################################################

head(mtcars)
cars <- mtcars[, c(1:4, 6:7, 9:11)]  # Select variables
head(cars)

# COMPUTE PCA ##############################################

# For entire data frame ####################################
pc <- prcomp(cars,
             center = TRUE,  # Centers means to 0 (optional)
             scale = TRUE)   # Sets unit variance (helpful)

# To specify variables #####################################

pc <- prcomp(~ mpg + cyl + disp + hp + wt + qsec + am +
               gear + carb, 
             data = mtcars, 
             center = TRUE,
             scale = TRUE)

# EXAMINE RESULTS ##########################################

# Get summary stats
summary(pc)

# Screeplot for number of components
plot(pc)

# Get standard deviations and rotation
pc

# See how cases load on PCs
predict(pc) %>% round(2)

# Biplot of first two components- two dimensional plot
biplot(pc)


# REGRESSION - to predict if some judges should be retained - INSTALL AND LOAD PACKAGES ################################



# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, caret, lars, tidyverse)

# LOAD DATA ################################################

?USJudgeRatings
head(USJudgeRatings)
data <- USJudgeRatings

# Define variable groups
x <- as.matrix(data[, -12])
y <- data[, 12]

# REGRESSION WITH SIMULTANEOUS ENTRY #######################

# Using variable groups lm-linear model
reg1 <- lm(y ~ x)

# Or specify variables individually
reg1 <- lm(RTEN ~ CONT + INTG + DMNR + DILG + CFMG +
             DECI + PREP + FAMI + ORAL + WRIT + PHYS,
           data = USJudgeRatings)

# Results
reg1           # Coefficients only
summary(reg1)  # Inferential tests

# MORE SUMMARIES ###########################################

anova(reg1)            # Coefficients w/inferential tests
coef(reg1)             # Coefficients (same as reg1)
confint(reg1)          # CI for coefficients
resid(reg1)            # Residuals case-by-case
hist(residuals(reg1))  # Histogram of residuals

# ADDITIONAL MODELS ########################################

# Conventional stepwise regression
stepwise <- lars(x, y, type = "stepwise")

# Stagewise: Like stepwise but with better generalizability
forward <- lars(x, y, type = "forward.stagewise")

# LAR: Least Angle Regression
lar <- lars(x, y, type = "lar")

# LASSO: Least Absolute Shrinkage and Selection Operator
lasso <- lars(x, y, type = "lasso")

# Comparison of R^2 for new models
r2comp <- c(stepwise$R2[6], forward$R2[6], 
            lar$R2[6], lasso$R2[6]) %>% 
  round(2)
names(r2comp) <- c("stepwise", "forward", "lar", "lasso") 
r2comp  # Show values of R^2


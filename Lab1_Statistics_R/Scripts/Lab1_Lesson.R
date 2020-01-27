#BIO345 Spring 2020 - Lesson 1
#Author: Kevin Wong
#Reserouces: https://datacarpentry.org/R-ecology-lesson/01-intro-to-r.html, https://datacarpentry.org/R-ecology-lesson/02-starting-with-data.html

#### Introduction to R ####

#Simple math
3 + 5 #prints in console
12 / 7

#Assigning values
weight_kg <- 55 #See it appear in your environment

#Objects vs. Variables
weight_kg #Prints in console
2.2 * weight_kg #math with objects e.g. changing to pounds

#changing object's values
weight_kg <- 57.5
2.2 * weight_kg 

#creating a new object for pounds
weight_lbs <- 2.2 * weight_kg

#Functions and arguments 
b<-sqrt(a) #square root of a variable 
weight_kg_sqrt <- sqrt(weight_kg)

round(3.14159) #rounding this number, but what are the arguments to change how many decimal places? 
args(round) #determining arguments for this function
?round #obtaining help information about this function
round(x = 3.14159, digits = 2) #rounding to 2 digits
round(3.14159, 2) #rounding to 2 digits 

#Vectors and data types
weight_g <- c(50,60,65,82) #creating a vector with numbers
animals <- c("mouse", "rat", "dog") #creating a vector with characters - quotes are essential

length(weight_g) #length tells you how many elements there are in that vector
length(animals)

class(weight_g) #class tells you what type of data is in this vector
class(animals)

str(weight_g)
str(animals)

weight_g_2 <- c(weight_g, 90) #add to the end of a vector
weight_g_3 <- c(30, weight_g_2) #add to the beginning of a vector

#Subsetting vectors
animals <- c("mouse", "rat", "dog", "cat")
animals[2] #printing the second variable in the vector
animals[c(3,2)] #printing the third and second variables in the vector

more_animals <- animals[c(1,2,3,2,1,4)] #creating an object with repeated indices of another vector

#Conditional subsetting - using a logical vector to subset data
weight_g <- c(21,34,39,54,55)
weight_g[c(TRUE, FALSE, TRUE,TRUE,FALSE)] # Printing variables using a logical vector - typically these logical vectors are not written by hand 

weight_g > 50 # will return logicals with TRUE for indices that meet this condition
weight_g[weight_g > 50] #selecting values over 50

weight_g[weight_g < 30 | weight_g > 50] #combinging multiple conditions using "|" (atleast 1 condition is true; OR)
weight_g[weight_g >= 30 & weight_g == 21] #combinging multiple conditions using "&" (both conditions are true; AND)

animals <- c("mouse", "rat", "dog", "cat") 
animals[animals == "cat" | animals == "rat"] 
animals %in% c("rat", "cat", "dog", "duck", "goat") #finds all the indices in "animals" that match the list given 
animals[animals %in% c("rat", "cat", "dog", "duck", "goat")] #prints which indices are present in this list 

#### Starting with data ####

#Importing data
download.file(url="https://ndownloader.figshare.com/files/2292169", #downloading data from the internet
              destfile = "Lab1_Statistics_R/Data/portal_data_joined.csv") #putting the downloaded file into a specific folder in my working directory

surveys <- read.csv("Lab1_Statistics_R/Data/portal_data_joined.csv") #importing data into R and creating a dataframe 

#Exploring the dataframe
head(surveys) #showing the top 6 lines of the dataframe 
View(surveys) #shows the entire dataframe in a new tab
str(surveys) #shows the structure of the dataframe. Whats is the class of the object surveys? How many rows and columns? 

#simple plotting(https://rstudio-pubs-static.s3.amazonaws.com/7953_4e3efd5b9415444ca065b1167862c349.html)

plot(surveys$sex) #barplot
plot(surveys$hindfoot_length, surveys$weight) #scatterplot
boxplot(surveys$weight ~ surveys$sex) #boxplot

#Simple statistics

rodents <- subset(surveys, taxa == "Rodent") #creating a new dataframe by subsetting surveys with only rodent taxa
rodents.clean <- rodents[complete.cases(rodents), ] #removing rows with NAs in the entire data frame 

### Question 1: What is the weight difference between male and female rodents?

boxplot(rodents.clean$weight ~ rodents.clean$sex)# boxplot of data

#Removing the blank vlaues
levels(rodents.clean$sex)
levels(rodents.clean$sex)[1] <- NA
levels(rodents.clean$sex)

boxplot(rodents.clean$weight ~ rodents.clean$sex)# boxplot of data

#Hardcoding statistics
rodents.male <- subset(rodents.clean, sex == "M")
n.rodents.male <- length(rodents.male$weight) #sample size (n)
sum.rodents.male <- sum(rodents.male$weight) #Sum of weights
mean.rodents.male <- mean(rodents.male$weight) #mean of weights
as.rodents.male <- sum(rodents.male$weight^2) #added squares
ss.rodents.male <- as.rodents.male - (sum.rodents.male^2/n.rodents.male) #sum of squares
df.rodents.male <- n.rodents.male - 1 #degrees of freedom
var.rodents.male <- ss.rodents.male/df.rodents.male #variance
sd.rodents.male <- sqrt(var.rodents.male) #standard deviation
se.rodents.male <- sd.rodents.male/(sqrt(n.rodents.male)) #standard error

# n, standard deviation, standard error --> FASTER WAY
install.packages("Rmisc") #installing a new package
library(Rmisc) #loading package into environment

rodents.summary <- summarySE(rodents.clean, measurevar = "weight", groupvars = "sex") #obtaining summary statistics

# T-test
t.test(rodents.clean$weight ~ rodents.clean$sex) #t-test

#slope
plot(weight ~ hindfoot_length, data = rodents.male) #scatterplot
model <- lm(weight ~ hindfoot_length, data = rodents.male) #creating a variable with the linear model

abline(model) #draws slope from linear model on plot
summary(model) #summary of the model




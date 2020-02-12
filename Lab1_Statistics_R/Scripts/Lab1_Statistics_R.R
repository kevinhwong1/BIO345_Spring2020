#Lab 1: Statsitics - Answers
#Author: Kevin Wong 

#-------------------------------------------------------------------------------------------------------
# Question 1: 

## H0: There is no relationship between sea cucumber length and species
## HA: There is a relationship between sea cucumber length and species 

SeaCucumber <- read.csv('Lab1_Statistics_R/Data/SeaCucumber_Length.csv', header=T, sep=",")
# Individual Populations
# Purple Sea Cucumber 
PurpleSC <- subset(SeaCucumber, Species=="Purple.SC", select=c(Length, Species))
n.Purple <- length(PurpleSC$Length) #n 
Total.Purple <- sum(PurpleSC$Length) # total (sum X)
Mean.Purple <- mean(PurpleSC$Length) # mean 
CF.Purple <- Total.Purple^2/n.Purple #correction factor
AS.Purple <- sum(PurpleSC$Length^2) #added squares
SS.Purple <- AS.Purple-(Total.Purple^2/n.Purple) #sum of squares

# California Sea Cucumber
California.SC <- subset(SeaCucumber, Species=="CA.SC", select=c(Length, Species))
n.CA <- length(California.SC$Length) #n 
Total.CA <- sum(California.SC$Length) # total (sum X)
Mean.CA <- mean(California.SC$Length) # mean 
CF.CA <- Total.CA^2/n.CA #correction factor
AS.CA <- sum(California.SC$Length^2) #added squares
SS.CA <- AS.CA-(Total.CA^2/n.CA) #sum of squares

# Pooled Statistics
SS.SeaCucumber <- SS.CA + SS.Purple 
df.SeaCucumber <- (n.Purple-1)+(n.CA-1) #degrees of freedom
Var.SeaCucumber <- (SS.CA+SS.Purple)/df.SeaCucumber #pooled variance 
s.SeaCucumber <- sqrt(Var.SeaCucumber) #standard deviation
StEr.SeaCucumber <- s.SeaCucumber*sqrt((1/n.Purple)+(1/n.CA)) #standard error of the difference b/w means
t <- (Mean.Purple-Mean.CA)/StEr.SeaCucumber

#example of what to do in the future instead of hard coding
t.test(PurpleSC$Length,California.SC$Length)

# VISUALIZING DATA
# Boxplot
boxplot(Length~Species,data=SeaCucumber,main="Sea Cucumber Body Length",
        xlab="Type of Sea Cucumber", ylab="Body Length (mm)")
#Click Export > save as PDF or Image.

#-------------------------------------------------------------------------------------------------------
  
# Question 2: 
  
  ## H0: There is no relationship between dive time and birth status
  ## HA: There is a relationship between dive time and birth status
  
  
DiveTime <- read.csv('Lab1_Statistics_R/Data/DiveTime.csv', header=T, sep=",")
# Individual Datasets
# Before Birth 
Before.Birth <- subset(DiveTime, Birth.Status=="Before", select=c(Dive.Time))
n.Before <- length(Before.Birth$Dive.Time) #n 
Total.Before <- sum(Before.Birth$Dive.Time) # total (sum X)
Mean.Before <- mean(Before.Birth$Dive.Time) # mean 
CF.Before <- Total.Before^2/n.Before #correction factor
AS.Before <- sum(Before.Birth$Dive.Time^2) #added squares
SS.Before <- AS.Before-(Total.Before^2/n.Before) #sum of squares
# After Birth 
After.Birth <- subset(DiveTime, Birth.Status=="After", select=c(Dive.Time))
n.After <- length(After.Birth$Dive.Time) #n 
Total.After <- sum(After.Birth$Dive.Time) # total (sum X)
Mean.After <- mean(After.Birth$Dive.Time) # mean 
CF.After <- Total.After^2/n.After #correction factor
AS.After <- sum(After.Birth$Dive.Time^2) #added squares
SS.After <- AS.After-(Total.After^2/n.After) #sum of squares

# Pooled Statistics
SS.DiveTime <- SS.Before + SS.After 
df.DiveTime <- (n.Before-1)+(n.After-1) #degrees of freedom
Var.DiveTime <- (SS.Before+SS.After)/df.DiveTime #pooled variance 
s.DiveTime <- sqrt(Var.DiveTime) #standard deviation
StEr.DiveTime <- s.DiveTime*sqrt((1/n.Before)+(1/n.After)) #standard error of the difference b/w means
t <- (Mean.After-Mean.Before)/StEr.DiveTime
#example of what to do in the future instead of hard coding
t.test(Before.Birth$Dive.Time,After.Birth$Dive.Time)

# Boxplot
boxplot(Dive.Time~Birth.Status,data=DiveTime,main="Dive Time Per Dive",
        xlab="Birth Status", ylab="Dive Time (minutes)")
#Click Export > save as PDF or Image.

#-------------------------------------------------------------------------------------------------------

# Question 3: 

## H0: There is no relationship between ratio and body mass
## HA: There is a relationship between ratio and body mass


DeepSea_Squid <- read.csv('Lab1_Statistics_R/Data/DeepSea_Squid.csv', header=T, sep=",")
model <-lm(Body_Mass ~ Ratio, data=DeepSea_Squid) #runs a linear regression of Body_Mass as a function of Ratio
coe <- coef(model) #extracts the coeffecients
R2 <-summary(model)$r.squared
plot(Body_Mass ~ Ratio, data=DeepSea_Squid)
abline(lm(Body_Mass ~ Ratio, data=DeepSea_Squid))
summary(model)
## rounded coefficients for better output
cf <- round(coef(model), 2) 
cf # printing the intercept and ratio (slope) into console
## sign check to avoid having plus followed by minus for negative coefficients
eq <- paste0("eq = ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " x ")

## printing of the equation
mtext(eq, 3, line=-2)

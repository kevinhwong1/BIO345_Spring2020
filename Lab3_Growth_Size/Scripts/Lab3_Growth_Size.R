#BIO345: Lab 3 - Mussel Growth Baseline 

#BIO345 Spring 2020 - Lab 2
#Author: Kevin Wong

library(dplyr)
library(tidyverse)
library(readr)
library(stringr)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(Rmisc)

# Import Data

Growth.data <- read.csv("Lab3_Growth_Size/Data/Master_Mussel_Data_Spring2020.csv") #import data

#Dropping extra factor in tank

Growth.data$Tank <- factor(Growth.data$Tank)

# Boxplots
##Length
Box.Length <- ggplot(Growth.data, aes(x=Tank, y=Length_cm, fill = Tank)) +
  geom_boxplot(width=.3, outlier.colour=NA) +
  geom_jitter(position=position_jitter(width=.1, height=0)) +
  scale_fill_manual(values=c("dodgerblue3", "tomato3")) +
  xlab("Tank Treatment") + ylab("Shell Length (cm)") + #Axis titles
  theme_bw() + theme(panel.border = element_rect(color="black", fill=NA, size=0.75), panel.grid.major = element_blank(), #Makes background theme white
                     panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 15, color = "black")) +
  theme(legend.position = "none")

##Width
Box.Width <- ggplot(Growth.data, aes(x=Tank, y=Width_cm, fill = Tank)) +
  geom_boxplot(width=.3, outlier.colour=NA) +
  geom_jitter(position=position_jitter(width=.1, height=0)) +
  scale_fill_manual(values=c("dodgerblue3", "tomato3")) +
  xlab("Tank Treatment") + ylab("Shell Width (cm)") + #Axis titles
  theme_bw() + theme(panel.border = element_rect(color="black", fill=NA, size=0.75), panel.grid.major = element_blank(), #Makes background theme white
                     panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 15, color = "black")) +
  theme(legend.position = "none")

##Weight
Box.Weight <- ggplot(Growth.data, aes(x=Tank, y=Weight_g, fill = Tank)) +
  geom_boxplot(width=.3, outlier.colour=NA) +
  geom_jitter(position=position_jitter(width=.1, height=0)) +
  scale_fill_manual(values=c("dodgerblue3", "tomato3")) +
  xlab("Tank Treatment") + ylab("Weight (g)") + #Axis titles
  theme_bw() + theme(panel.border = element_rect(color="black", fill=NA, size=0.75), panel.grid.major = element_blank(), #Makes background theme white
                     panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 15, color = "black")) +
  theme(legend.position = "none")

#Combinging all figures together
Growth.Initial.Figs <- arrangeGrob(Box.Length, Box.Width, Box.Weight, ncol=3)

#Saving the figure
ggsave(file="Lab3_Growth_Size/Output/Growth_Inital_Figs.pdf", Growth.Initial.Figs, width = 11, height = 6, units = c("in"))


## Statistics 

#Creating linear model 
lm.length <- lm(Length_cm ~ Tank, data = Growth.data)
lm.width <- lm(Width_cm ~ Tank, data = Growth.data)
lm.weight <- lm(Weight_g ~ Tank, data = Growth.data)

#Normality 
qqnorm(resid(lm.length))
qqline(resid(lm.length)) 
shapiro.test(Growth.data$Length_cm) # Normal distribution for length (p-value > 0.05). Therefore perform a T-test

qqnorm(resid(lm.width))
qqline(resid(lm.width)) 
shapiro.test(Growth.data$Width_cm) # Normal distribution for length (p-value > 0.05). Therefore perform a T-test

qqnorm(resid(lm.weight))
qqline(resid(lm.weight)) 
shapiro.test(Growth.data$Weight_g) # NOT Normal distribution for length (p-value < 0.05). Therefore perform a Manwhitney U test (non-parametric)

#Homogeniety of Variances
boxplot(resid(lm.length)~Growth.data$Tank)

boxplot(resid(lm.width)~Growth.data$Tank)

boxplot(resid(lm.weight)~Growth.data$Tank)


# T-test
Length.t.test <- t.test(Length_cm ~ Tank, data = Growth.data)

Width.t.test <- t.test(Width_cm ~ Tank, data = Growth.data)

# Man Whitney U 
Weight.MWU.test <- wilcox.test(Weight_g ~ Tank, data = Growth.data)

#Saving outputs

capture.output(Length.t.test, file = "Lab3_Growth_Size/Output/Length.Statistics.csv")
capture.output(Width.t.test, file = "Lab3_Growth_Size/Output/Width.Statistics.csv")
capture.output(Weight.MWU.test, file = "Lab3_Growth_Size/Output/Weight.Statistics.csv")




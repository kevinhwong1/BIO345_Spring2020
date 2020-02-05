#BIO345 Spring 2020 - Lab 2
#Author: Kevin Wong

library(dbplyr)
library(tidyverse)
library(readr)
library(stringr)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(Rmisc)

# Goal - to learn how to make standard curve and extrapolate data 

TP_Data_all <- read.csv("Lab2_StandardCurves/Data/TP.1.data.csv") #import data

#Subset standard
standard <- TP_Data_all %>% 
  filter(Sample.Type == "Standard") 

plot.Standard<- ggplot(data = standard, aes(x=Concentration, y=abs.corr))+ #plotting standard curve
  ylab("Absorbance (nm)")+ xlab("Concentration") + 
  geom_point()+
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lmstandard <- lm (Concentration ~ abs.corr, data = standard) #creating linear model
lmsummary <- summary(lmstandard) #summarizing model

samples <- TP_Data_all %>% #subsetting Samples
  filter(Sample.Type == "Sample") 
samples$Concentration.calc <- predict(lmstandard, newdata = samples) #using model to get concentration

install.packages("LexisNexisTools")
install.packages("tm")
install.packages("tidytext")
install.packages("readtext")
install.packages("dplyr")
install.packages("officer")
install.packages("ggpubr")

library(readtext)
library(LexisNexisTools)
library(tm)
library(tidytext)
library(dplyr)
library(tibble)
library(SnowballC)
library(ggplot2)
library(stringr)
library(officer)
library(ggpubr)
library(ggpubr)

################################################################################
## DICTIONARY CREATION ##
## Create dictionary for high competence
highComp_dict = read.csv("C:/Users/Michelle/Desktop/test/highcomp.csv", header = FALSE)

# Convert dictionary from list to character
highComp_dict = unlist(highComp_dict)
# View(highComp_dict)

## Create dictionary for low competence
lowComp_dict = read.csv("C:/Users/Michelle/Desktop/test/lowcomp.csv", header = FALSE)

# Convert dictionary from list to character
lowComp_dict = unlist(lowComp_dict)
# View(lowComp_dict)

## Create dictionary for high morality
highMoral_dict = read.csv("C:/Users/Michelle/Desktop/test/highmoral.csv", header = FALSE)

# Convert dictionary from list to character
highMoral_dict = unlist(highMoral_dict)
# View(highMoral_dict)

## Create dictionary for low morality
lowMoral_dict = read.csv("C:/Users/Michelle/Desktop/test/lowmoral.csv", header = FALSE)

# Convert dictionary from list to character
lowMoral_dict = unlist(lowMoral_dict)
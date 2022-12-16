# load libraries
library(readxl)
library(tidyverse)

# load datasets
foodServiceEDA <- read_excel("C:/Users/Eduardo.Armenta/Desktop/Xignux/Proyectos/Tesoreria/Data/TodosJuntos.xlsx", sheet = 1)
qualtiaEDA <- read_excel("C:/Users/Eduardo.Armenta/Desktop/Xignux/Proyectos/Tesoreria/Data/TodosJuntos.xlsx", sheet = 2)
botanasEDA <- read_excel("C:/Users/Eduardo.Armenta/Desktop/Xignux/Proyectos/Tesoreria/Data/TodosJuntos.xlsx", sheet = 3)
xoEDA <- read_excel("C:/Users/Eduardo.Armenta/Desktop/Xignux/Proyectos/Tesoreria/Data/TodosJuntos.xlsx", sheet = 4)

# select date and amount from datasets
foodServiceEDA <- foodServiceEDA %>% select(Date, Amount)
qualtiaEDA <- qualtiaEDA %>% select(Date, Amount)
botanasEDA <- botanasEDA %>% select(Date, Amount)
xoEDA <- xoEDA %>% select(Date, Amount)

# min of data
minFS <- min(foodServiceEDA$Amount)
minQ <- min(qualtiaEDA$Amount)
minB <- min(botanasEDA$Amount)
minXO <- min(xoEDA$Amount)

# max of data
maxFS <- max(foodServiceEDA$Amount)
maxQ <- max(qualtiaEDA$Amount)
maxB <- max(botanasEDA$Amount)
maxXO <- max(xoEDA$Amount)

# average of data
avgFS <- mean(foodServiceEDA$Amount)
avgQ <- mean(qualtiaEDA$Amount)
avgB <- mean(botanasEDA$Amount)
avgXO <- mean(xoEDA$Amount)

# standard dev of data
stdevFS <- sd(foodServiceEDA$Amount)
stdevQ <- sd(qualtiaEDA$Amount)
stdevB <- sd(botanasEDA$Amount)
stdevXO <- sd(xoEDA$Amount)

# standardized data
standardFS <- scale(foodServiceEDA$Amount)
standardQ <- scale(qualtiaEDA$Amount)
standardB <- scale(botanasEDA$Amount)
standardXO <- scale(xoEDA$Amount)

# create histogram distributions
histFS<- ggplot(foodServiceEDA, aes(x=Amount)) +
  geom_histogram()
histQ <- ggplot(qualtiaEDA, aes(x=Amount)) +
  geom_histogram()
histB <- ggplot(botanasEDA, aes(x=Amount)) +
  geom_histogram()
histXO <- ggplot(xoEDA, aes(x=Amount)) +
  geom_histogram()

# view histograms
histFS
histQ
histB
histXO
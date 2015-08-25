rm(list = ls())
# Data can be downloaded from
# https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/parole.csv

setwd("~/Dropbox/EdX/The Analytics Edge/Unit 7")

parole = read.csv("parole.csv")
str(parole)

parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

table(parole$male, parole$violator)
14/(14+64)

table(parole$state == 2, parole$crime)

Fig1 = ggplot(data = parole, aes(x = age)) + geom_histogram()
Fig1 + geom_histogram(binwidth = 5)

Fig2 = ggplot(data = parole, aes(x = age)) + geom_histogram(color = "blue")
Fig2

Fig3 = ggplot(data = parole, aes(x = age)) + 
  geom_histogram(binwidth = 5, color = "blue") + 
  facet_grid(male ~ .)
Fig3

Fig4 = ggplot(data = parole, aes(x = age)) + 
  geom_histogram(binwidth = 5, color = "blue") + 
  facet_grid(. ~ male)
Fig4

Fig5 = ggplot(data = parole, aes(x = age, fill = male)) +
  geom_histogram(binwidth = 5, color = "white")
Fig5

colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Fig6 = ggplot(data = parole, aes(x = age, fill = male)) + 
  geom_histogram(binwidth = 5) + 
  scale_fill_manual(values=colorPalette)
Fig6

Fig7 = ggplot(data = parole, aes(x = age, fill = male)) + 
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.5) + 
  scale_fill_manual(values=colorPalette)
Fig7

Fig8 = ggplot(data = parole, aes(x = time.served)) +
  geom_histogram(color = "blue", binwidth = 0.1)
Fig8

Fig9 = ggplot(data = parole, aes(x = time.served)) +
  geom_histogram(binwidth = 1) +
  facet_grid(. ~ crime)
Fig9

Fig10 = ggplot(data = parole, aes(x = time.served, fill = crime)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.5)
Fig10

Fig5

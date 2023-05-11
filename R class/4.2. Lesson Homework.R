# 1. Solution ----

rm(list=ls())

library(wooldridge)
library(corrplot)

View(wage1)

wage1$seperation <- ifelse(wage1$female == 0 & wage1$married == 1, "married men", 
                           ifelse(wage1$female == 0 & wage1$married == 0, "single men",
                                  ifelse(wage1$female == 1 & wage1$married == 0, "single women", "married women")))
seperation <- c("single men","married men","single women", "married women")


a <- mean(wage1$wage[wage1$seperation == "married men"])
b <- mean(wage1$wage[wage1$seperation == "single men"])
c <- mean(wage1$wage[wage1$seperation == "single women"])
d <- mean(wage1$wage[wage1$seperation == "married women"])

A <- c(a,b,c,d)

barplot(A,
        names = c("Married men","Single men","Single women", "Married women"),
        col = c("lightblue","lightblue","pink","pink"),
        ylab = "Dollar per Hour",
        ylim = c(0,10),
        main = "Mean hourly wage by gender and marital status")


# 2.Solution ----
 # using the For Loop 
rm(list=ls()) 

library(wooldridge) 
library(corrplot) 

wage1$seperation <- ifelse(wage1$female == 0 & wage1$married == 1, "married men", 
                           ifelse(wage1$female == 0 & wage1$married == 0, "single men",
                                  ifelse(wage1$female == 1 & wage1$married == 0, "single women", "married women")))
seperation <- c("married men","single men","single women", "married women")

Aver <- vector("numeric",length(seperation))
for (i in 1: length(seperation)) {
 (Aver[i] <- mean(wage1$wage[wage1$seperation == seperation[i]]))
}

barplot(Aver,
        names = c("married men","single men","single women", "married women"),
        col = c("lightblue","lightblue","pink","pink"),
        ylab = "Dollar per Hour",
        ylim = c(0,10),
        main = "Mean hourly wage by gender and marital status")

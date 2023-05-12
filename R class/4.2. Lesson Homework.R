# 1. Solution ----

rm(list=ls())

library(wooldridge) 

View(wage1)  

wage1$categories <- ifelse(wage1$female == 0 & wage1$married == 1, "married men", 
                           ifelse(wage1$female == 0 & wage1$married == 0, "single men",
                                  ifelse(wage1$female == 1 & wage1$married == 0, "single women", "married women")))
categories <- c("married men","single men","single women", "married women")


a <- mean(wage1$wage[wage1$categories == "married men"])
b <- mean(wage1$wage[wage1$categories == "single men"])
c <- mean(wage1$wage[wage1$categories == "single women"])
d <- mean(wage1$wage[wage1$categories == "married women"])

A <- c(a,b,c,d)

barplot(A,
        names = c("married men","single men","single women", "married women"),
        col = c("lightblue","lightblue","pink","pink"),
        ylab = "Dollar per Hour",
        ylim = c(0,10),
        main = "Mean Hourly Wage by Gender and Marital Status")


# 2.Solution ----
 # using the For Loop 
rm(list=ls())  
 
library(wooldridge) 

wage1$categories <- ifelse(wage1$female == 0 & wage1$married == 1, "married men", 
                           ifelse(wage1$female == 0 & wage1$married == 0, "single men",
                                  ifelse(wage1$female == 1 & wage1$married == 0, "single women", "married women")))
categories <- c("married men","single men","single women", "married women")

Aver <- vector("numeric",length(categories))
for (i in 1: length(categories)) {
 (Aver[i] <- mean(wage1$wage[wage1$categories == categories[i]]))
}

barplot(Aver,
        names = c("married men","single men","single women", "married women"),
        col = c("lightblue","lightblue","pink","pink"),
        ylab = "Dollar per Hour",
        ylim = c(0,10), 
        main = "Mean Hourly Wage by Gender and Marital Status")


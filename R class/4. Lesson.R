rm(list=ls())

library(wooldridge)
library(corrplot)

# structure of the dataset ----
## (Sub) structure of the dataset ----
#for organising the script 
View(wage1)
nrow(wage1)
ncol(wage1)
dim(wage1)
str(wage1)
colnames(wage1) 

#Indexing ----
wage1$educ[5]

wage1$wage[12]

wage1$wage[c(5,6,7)]  #display datafrom line 5 6 7 

wage1$educ[c(5:7)]

wage1$educ[5] <- 10 
wage1$educ[-c(5:nrow(wage1))] #exclude, nrow = 526

wage1[-1,] #"-" means exclude, "1" denotes row, after "1" comes column 
wage1[,-1] 

wage1[,colnames(wage1) %in% c("wage", "educ", "exper")]

wage1$wage_EUR <- wage1$wage * 0.86

wage1[1:5,c("wage", "wage_EUR")]

#conditional selection ----
wage1_15plus <- wage1[wage1$educ > 15,]

#Question: How many workers are between 15 and 18 educ?
wage1_18minus <- wage1[ wage1$educ < 18,]
wage1_1518 <- wage1_18minus[wage1_18minus$educ > 15,]
nrow(wage1_1518)

length(wage1$educ[wage1$educ > 15 & wage1$educ < 18]) #use the vector function to check if its a vector
#or 
nrow(wage1[wage1$educ > 15 & wage1$educ < 18,]) #use the data frame function to ... is.data.frame

mean(wage1[wage1$educ > 15 & wage1$educ < 18, "wage"])
mean(wage1$educ[wage1$educ > 15 & wage1$educ < 18])

sum(wage1$educ > 15) #counting "true" , 99 means there are 99 workers with more 15 years of exp.

#percentage of workers with 15 years of experience 
#Nr people more 15/ Nr people total * 100
nrow(wage1_15plus) / nrow(wage1) *100
#or 
sum(wage1$educ > 15) / nrow(wage1) * 100

data_w <- wage1[,c("wage","educ","exper","tenure")]
data_w_cor <- cor(data_w)

corrplot(data_w_cor)
corrplot(data_w_cor, 
         method = "number", 
         type = "upper")






rm(list=ls())

wage1$gender <- ifelse(wage1$female == 0, "Male", "Female") #if value = 0 then male, otherwise (=1) female 
cols <- c("blue","red")
gender <- c("Male", "Female")

par(mfrow = c(1,2)) 
plot(NULL, 
     xlim = c(min(wage1$educ), max(wage1$educ)),
     ylim = c(min(wage1$wage), max(wage1$wage)),
     xlab = "Years of education",
     ylab = "Dollar per Hour" )
for (i in 1: length(gender)) {
  points(x= wage1$educ[wage1$gender == gender[i]],
         y=wage1$wage[wage1$gender == gender[i]],
         pch = i,
         col = cols[i])
}

boxplot(wage1$wage [wage1$female == 0],
        wage1$wage [wage1$female == 1],
        ylab = "Dollar per Hour",
        xlab = "Gender",
        names = c("Male", "Female"),
        col = c("cyan", "pink"),
        horizontal = TRUE)






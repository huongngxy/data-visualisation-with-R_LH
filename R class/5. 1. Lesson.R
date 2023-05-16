rm(list=ls())

library(wooldridge)

view(wage1)

northcen  <- sum(wage1$northcen[wage1$northcen == 1])
south <- sum(wage1$south[wage1$south == 1])
west <- sum(wage1$west[wage1$west == 1])
east <- nrow(wage1) - northcen - south - west 

#check 
northcen + south + west + east == nrow(wage1)

slices <- c(northcen, south, west, east)

slices_pct <- round(slices / sum(slices)*100)

labels <- c("North Center", "South", "West","East") #slices and labels should be in the same order if order change wanted 

pie(slices, 
    labels = paste0(slices_pct, "%"),
    main = paste0("Workers by Region (N = ", sum(slices) , ")"),
    col = rainbow(length(slices)))

legend("bottomright", 
       labels, 
       cex = 0.8, 
       bty = "n", 
       fill = rainbow(length(slices)))



rm(list=ls())
x <- c(68, 84, 75, 82, 68, 90, 62, 88, 76, 93,
       73, 79, 88, 73, 60, 93, 71, 59, 85, 75,
       61, 65, 75, 87, 74, 62, 95, 78, 63, 72,
       66, 78, 82, 75, 94, 77, 69, 74, 68, 60,
       96, 78, 89, 61, 75, 95, 60, 79, 83, 71,
       79, 62, 67, 97, 78, 85, 76, 65, 71, 75,
       65, 80, 73, 57, 88, 78, 62, 76, 53, 74,
       86, 67, 73, 81, 72, 63, 76, 75, 85, 77)
max(x)
min(x)
max(x) - min(x)

x_sorted <- sort(x)
x_sorted[length(x_sorted)]
x_sorted[(length(x_sorted)-4): length(x_sorted)]

x_sorted_des <- sort(x, decreasing = TRUE)
x_sorted_des[(length(x_sorted_des)-4):length(x_sorted_des)]

x_sorted_des[10]

studentabove75 <- x_sorted >= 75
sum(studentabove75)

sum(x<85)


studentabove65 <- x_sorted > 65 
studentabove85 <- x_sorted > 85
zufinden <- studentabove65 - studentabove85
sum(zufinden)/length(x_sorted)*100

sum(x > 65 & x < 85)/ length(x) * 100
hist(x)

rm(list=ls())

library(readxl)
X5_Lesson_HWData <- read_excel("R class/5. Lesson HWData.xlsx", 
                               sheet = "data", skip = 3)
View(X5_Lesson_HWData)
countries <- unique(X5_Lesson_HWData$country)
par(mfrow = c(3,3))

for(i in 1: length(countries)) {
  plot(x = X5_Lesson_HWData$year[X5_Lesson_HWData$country == countries[i]],
       y = X5_Lesson_HWData$value[X5_Lesson_HWData$country == countries[i]],
       xlab = "GDP",
       ylab = "Year",
       main = countries[i],
       type = "l")
} 


library(readxl)
X3_Lesson_Data_mpc <- read_excel("R class/3. Lesson Data_mpc.xlsx", 
                                 sheet = "Tabelle1", skip = 1)
View(X3_Lesson_Data_mpc)
countries <- unique(X3_Lesson_Data_mpc$country) 
cols <- c("orange","blue","pink","green","violet","cyan")
par(mfrow = c(2,3))
consumption_share <- X3_Lesson_Data_mpc$consumption/X3_Lesson_Data_mpc$income
 

for(i in 1:length(countries)) {
  plot(x = X3_Lesson_Data_mpc$year[X3_Lesson_Data_mpc$country == countries[i]],
       y = consumption_share[X3_Lesson_Data_mpc$country == countries[i]], 
       main = countries[i],
       xlab = "Year",
       ylab = "Consumption Share", 
       type = "l",
       col = cols[i])  
}
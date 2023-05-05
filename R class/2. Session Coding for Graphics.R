library(readxl)
X2_Session_Data_gdp <- read_excel("R class/2. Session Data_gdp.xlsx", 
                                  sheet = "data", skip = 2)
View(X2_Session_Data_gdp)  
countries <- c("France","Germany", "Italy", "Spain")
cols <- c("blue","red","green","orange") 

plot(NULL,
     xlim= c(1960, 2022),
     ylim=c(0,4000),
     main="Gross Domestic Product, 1960-2022",
     xlab="Year", 
     ylab="Mrd.Euro")
for(c in 1:length(countries)){
  lines(x=X2_Session_Data_gdp$year[X2_Session_Data_gdp$country==countries[c]],
        y=X2_Session_Data_gdp$value[X2_Session_Data_gdp$country==countries[c]],
        type="l",
        col= cols[c])
}
legend("topleft",
       legend=countries,
       col=cols,
       lty=1,
       lwd=1,
       bty="n",
       cex=0.8)



par(mfrow=c(2,2))
for(i in 1: length(countries)){
  plot(NULL, 
       xlim=c(1960,2022),
       ylim=c(min(X2_Session_Data_gdp$value[X2_Session_Data_gdp$country==countries[i]]),
              max(X2_Session_Data_gdp$value[X2_Session_Data_gdp$country== countries[i]])),
       main=countries[i],
       xlab="Year",
       ylab="Mrd.Euro")
  lines(x=X2_Session_Data_gdp$year[X2_Session_Data_gdp$country== countries[i]],
        y=X2_Session_Data_gdp$value[X2_Session_Data_gdp$country==countries[i]],
        type="l",
        col=cols[i])
  
}
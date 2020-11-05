library("dplyr")
library("ggplot2")
library(tidyr)
data<-read.csv("file =PS3_5.data.csv",sep="\t",header=T) 
data<-data[,-1]

#5.1 

fit <- lm(Distance ~ Velocity,data = data)
plot(Distance ~ Velocity,data=data,     
     xlab = "Velocity(km/s)",
     ylab = "Distance(megaparsecs)",
     main = "Distance vs Velocity",
     pch = 20,
     cex = 2,
     col = "grey")
#5.2 
abline(fit, lwd = 5, col = "blue")

#5.3 
summary(fit)$coefficients
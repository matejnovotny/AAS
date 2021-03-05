setwd("C:/DATA/DULEZITE/Ambica/zdrojaky/namety-master/josy_ambica")
library(stats)
library(anomalize) # STL dekompozice - v zÃ¡kladu Rka a Twiter metoda
library(tidyverse) # ObecnÃ© knihovny pro transformace dat v "tidy" podobÃ¡ch
library(tsbox) # Konvertor formÃ¡tÅ¯ (tÅÃ­d) ÄasovÃ½ch Åad: ts, xts, data.frame, data.table, tibble, zoo, tsibble, tibbletime, timeSeries
library(forecast) # Knihovna pro analÃ½zy ÄasovÃ½ch Åad a forecasting
library(timetk) # knihovna pro zÃ¡kladnÃ­ prÃ¡ce s ÄasovÃ½mi Åadami

# ------- Knihovny pro ukÃ¡zky ----
library(plotrix) #free kreslenÃ­ do plotu
library(gridExtra) #free kreslenÃ­ do plotu
library(grid) #free kreslenÃ­ do plotu
library(dygraphs) #interaktivnÃ­ graf

anomaly<-function(df){
  r<-sd(df)
  for (i in 1:length(df))
    if (abs(df[i])>3*r)
    {b[i]<-TRUE}
  else{b[i]<-FALSE}
return(b)
  }

c<-scan("nybirths.dat")
plot(c)
myts<-ts(c, frequency=12, start=c(1946,1))
plot(myts)
plot(stl(myts, 12))
rem<-stl(myts, 12)
rem.remainder<-rem$time.series[,'remainder']
rem.dataframe <- as.data.frame(rem.remainder)
names(rem.dataframe)[names(rem.dataframe) == "x"] <- "Remainder"
plot(rem.dataframe) 
View(rem.dataframe)

rem.dataframe$Anomaly<-anomaly(rem.dataframe$Remainder)
plot(rem.dataframe)
newts<-as.ts(rem.dataframe, frequency=12, start=c(1946,1))
View(newts)

library(MASS)
library(dplyr)
setwd("C:/Users/onkar.k/Desktop/Datasets")
getwd()
dta <- read.csv("covidIndia.csv")
newdta<-read.csv("CovidVaccineIndia.csv")
View(dta)
View(newdta)

str(dta$Date)
str(newdta$Updated.On)

dta$Date<-as.Date(dta$Date)
newdta$Updated.On<-as.Date(newdta$Updated.On)

#day wise positive rate
posday<-dta[,c(2,9)]
View(posday)
posday$Date<-as.Date(posday$Date)
posday$Date<-format(posday$Date,"%A")
pday<-aggregate(posday$Confirmed~posday$Date,FUN=sum)
View(pday)
summary(pday)
pdday<-barplot(pday$`posday$Confirmed`, las=2, names.arg = pday$`posday$Date`, 
               col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , 
                     rgb(0.3,0.9,0.4,0.6) , rgb(0.3,0.9,0.4,0.6),
                     rgb(0.3,0.9,0.4,0.6), rgb(0.3,0.1,0.4,0.6), 
                     rgb(0.3,0.9,0.4,0.6)),
               xlab = "Day", ylab="no. of positives", main = "Day wise positive rate")
text(pdday,pday$`posday$Confirmed`+0.4,paste(pday$`posday$Confirmed`,sep = ""),cex = 0.8)


dths<-aggregate(dta$Deaths~dta$State,FUN=sum)
View(dths)

barplot(dths$`dta$Deaths`,las=2, names.arg=dths$`dta$State`,xlab = "States",ylab ="No. of Deaths", main = "Highest Death State")
pstv<-aggregate(dta$Confirmed~dta$State,FUN=sum)
View(pstv)
barplot(pstv$`dta$Confirmed`,las=2, names.arg=pstv$`dta$State`,xlab = "States",ylab ="No. of Positive Case", main = "Highest No. Of Positive State")

ttls<-cbind(dths,pstv[,2])
colnames(ttls)[1]<-"State"
colnames(ttls)[2]<-"Deaths"
colnames(ttls)[3]<-"Positives"
View(ttls)

plot(ttls$Deaths~ttls$Positives,ttls, xlab="Positives", ylab = "Deaths", main = "Death vs positive")
with(ttls, text(ttls$Deaths~ttls$Positives, labels=ttls$State, pos=4,cex=0.3))

dths1<-sum(dta$Deaths)
dths1

pstv1<-sum(dta$Confirmed)
pstv1

prcnt<-dths1/pstv1
pp<-prcnt*100
pie(pp, main = "Death Rate")

str(dta$Date)
x<-as.Date(dta$Date)
head(x)
year<-as.numeric(format(x,"%y"))
head(year)
month<-as.numeric(format(x,"%m"))
month<-month.name[month]
head(month)
day<-as.numeric(format(x,"%d"))
head(day)
y<-cbind(dta,year,month,day)
View(y)


#Month Wise Comparison Confirmed cases
mnth<-aggregate(y$Confirmed~y$month+y$year,FUN=sum)
mnth
plot(mnth)

barplot(mnth$`y$Confirmed`,las=2, names.arg=mnth$`y$month`,xlab = "Month",ylab ="No. of Positive Case", main = "Month Wise Comparison")
ggplot(mnth,aes(x=mnth$`y$month`,y=mnth$`y$Confirmed`))+geom_bar(stat = "identity")

#Highest Confirmed Month
mnth1<-aggregate(y$Confirmed~y$month+y$year,FUN=sum)
mnth1

barplot(mnth1$`y$Confirmed`,las=2, names.arg=mnth1$`y$month`,xlab = "Month",ylab ="No. of Positive Case", main = "Month Wise positive rate")
ggplot(mnth,aes(mnth$`y$month`,mnth$`y$Confirmed`))+geom_point()+geom_line()

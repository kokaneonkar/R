library(MASS)
library(dplyr)
setwd("C:/Users/onkar.k/Desktop/Datasets")
getwd()
dta <- read.csv("covid19india.csv")
View(dta)
dths<-aggregate(dta$Deaths~dta$State,FUN=sum)
dths
barplot(dths$`dta$Deaths`,las=2, names.arg=dths$`dta$State`,xlab = "States",ylab ="No. of Deaths", main = "Highest Death State")
pstv<-aggregate(dta$Confirmed~dta$State,FUN=sum)
pstv
barplot(pstv$`dta$Confirmed`,las=2, names.arg=pstv$`dta$State`,xlab = "States",ylab ="No. of Positive Case", main = "Highest No. Of Positive State")

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


barplot(mnth$`y$Confirmed`,las=2, names.arg=mnth$`y$month`,xlab = "Month",ylab ="No. of Positive Case", main = "Month Wise Comparison")
ggplot(mnth,aes(x=mnth$`y$month`,y=mnth$`y$Confirmed`))+geom_bar(stat = "identity")

#Highest Confirmed Month
mnth1<-aggregate(y$Confirmed~y$month+y$year,FUN=sum)
mnth1

barplot(mnth1$`y$Confirmed`,las=2, names.arg=mnth1$`y$month`,xlab = "Month",ylab ="No. of Positive Case", main = "Month Wise positive rate")
ggplot(mnth,aes(mnth$`y$month`,mnth$`y$Confirmed`))+geom_point()+geom_line()

q()

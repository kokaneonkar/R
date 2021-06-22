library(MASS)

setwd("C:/Users/onkar.k/Desktop/Datasets")
dta <- read.csv("covid19india.csv")
View(dta)

str(dta$Date)
dta$Date<-as.Date(dta$Date)
str(dta$Date)

sum(is.na(dta))

data<-na.omit(dta)
sum(is.na(dta))

summary(dta)

View(dta)

x<-as.Date(dta$Date)
head(x)
year<-as.numeric(format(x,"%Y"))
head(year)
month<-as.numeric(format(x,"%m"))
month<-month.abb[month]
head(month)
day<-as.numeric(format(x,"%d"))
head(day)
dta<-cbind(dta,year,month)
View(dta)

#deaths aggregate by month and year

dths<-aggregate(dta$Deaths~dta$month+dta$year,FUN=sum)

colnames(dths)[1]<-"Month"
colnames(dths)[2]<-"Year"
colnames(dths)[3]<-"Deaths"
View(dths)

dths$Year<-paste(dths$Month,dths$Year)
dths$Month<-NULL
View(dths)
str(dths$Year)

dths$Year<-as.Date(dths$Year)

library(dplyr)


#Combine month & Year with Deaths



barplot(dths$Deaths,las=2, names.arg=dths$Month,xlab = "Month & Year",ylab ="No. of Deaths", main = "Highest No. Deaths")


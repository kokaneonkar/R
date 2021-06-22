getwd()
setwd("C:/Users/onkar.k/Desktop/Datasets")
data <- read.csv("covid_vaccine_statewise.csv")
View(data)

str(data)

#state wise combinations of Vaccinations

sp<-aggregate(data$Total.Sputnik.V.Administered~data$State,FUN=sum)
co<-aggregate(data$Total.Covaxin.Administered~data$State,FUN=sum)
cs<-aggregate(data$Total.CoviShield.Administered~data$State,FUN=sum)
to<-aggregate(data$Total.Individuals.Vaccinated~data$State,FUN=sum)

dtte<-cbind(sp,co[,2],cs[,2],to[,2])
colnames(dtte)[1]<-"State"
colnames(dtte)[2]<-"Sputnik"
colnames(dtte)[3]<-"Covaxin"
colnames(dtte)[4]<-"CoviShield"
colnames(dtte)[5]<-"Total"

View(dtte)

# Covaxin vs Total Vaccines
plot(dtte$Covaxin~dtte$Total,dtte)
with(dtte, text(dtte$Covaxin~dtte$Total, labels=dtte$State, pos=4,cex=0.3))

z<-dtte[,c(3,5)]
m<-apply(z,2,mean)
s<-apply(z,2,sd)
z<-scale(z,m,s)

vcn<-dist(z)
vcn

hc<-hclust(vcn)
plot(hc, labels = dtte$State, hang=-1, main = "Covaxin vs Total Vaccines")

hc1<-hclust(vcn, method = "average")
plot(hc1, labels = dtte$State, hang=-1, main = "Covaxin vs Total Vaccines")


# CoviShield vs Total Vaccines

plot(dtte$CoviShield~dtte$Total,dtte)
with(dtte, text(dtte$CoviShield~dtte$Total, labels=dtte$State, pos=4,cex=0.3))

z1<-dtte[,c(4,5)]
m1<-apply(z,2,mean)
s1<-apply(z,2,sd)
z1<-scale(z1,m,s)

vcn1<-dist(z1)
vcn1

hc1<-hclust(vcn1)
plot(hc1, labels = dtte$State, hang=-1, main = "CoviShield vs Total Vaccines")

hc2<-hclust(vcn1, method = "average")
plot(hc2, labels = dtte$State, hang=-1, main = "CoviShield vs Total Vaccines with avg")

# Sputnik vs Total Vaccines

plot(dtte$Sputnik~dtte$Total,dtte)
with(dtte, text(dtte$Sputnik~dtte$Total, labels=dtte$State, pos=4,cex=0.3))

z2<-dtte[,c(2,5)]
m2<-apply(z,2,mean)
s2<-apply(z,2,sd)
z2<-scale(z1,m,s)

vcn2<-dist(z1)
vcn2

hc3<-hclust(vcn2)
plot(hc3, labels = dtte$State, hang=-1, main = "Sputnik vs Total Vaccines")

hc4<-hclust(vcn2, method = "average")
plot(hc2, labels = dtte$State, hang=-1, main = "Sputnik vs Total Vaccines with avg")



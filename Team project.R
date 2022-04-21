Esport<- read.csv("GeneralEsportData.csv")

library(scales)
library(ggplot2)
library(dplyr)

Esport_Clean<-subset(Esport,Esport$TotalTournaments>0)

Esport_Clean$TotalEarningsPercentage<- Esport_Clean$TotalEarnings/sum(Esport_Clean$TotalEarnings)

Esport_Clean$TotalEarningsPercentage<- percent(Esport_Clean$TotalEarningsPercentage)

ggplot(Esport_Clean,aes(x=Genre,y=TotalEarnings)) +
  geom_bar(stat = "identity")

Esport_Clean%>%
  filter(ReleaseDate >= 2010)%>%
  ggplot(aes(x=Genre,y=TotalPlayers)) +
  geom_bar(stat = "identity")

plot(Esport_Clean$TotalTournaments,Esport_Clean$TotalEarnings)

fit1<-lm(TotalEarnings~ReleaseDate+TotalPlayers+TotalTournaments,data = Esport_Clean)
summary(fit1)

plot(Esport_Clean$TotalTournaments,Esport_Clean$OnlineEarnings)

fit2<-lm(OnlineEarnings~ReleaseDate+TotalPlayers+TotalTournaments,data = Esport_Clean)
summary(fit2)

AgeofEmpres<-filter(Esport_Clean,grepl('Age of Empires',Game))
ggplot(AgeofEmpres,aes(x=Game,y=TotalEarnings))+
  geom_bar(stat = "Identity")

CallofDuty<-filter(Esport_Clean,grepl('Call of Duty',Game))
ggplot(CallofDuty,aes(x=Game,y=TotalEarnings))+
  geom_bar(stat = "Identity")

Battlefield<-filter(Esport_Clean,grepl('Battlefield',Game))
ggplot(Battlefield,aes(x=Game,y=TotalEarnings))+
  geom_bar(stat = "Identity")

FIFA<-filter(Esport_Clean,grepl('FIFA',Game))
ggplot(FIFA,aes(x=Game,y=TotalEarnings))+
  geom_bar(stat = "Identity")

ggplot(Esport_Clean,aes(x=Genre,y=TotalTournaments)) +
  geom_bar(stat = "identity")


ReleaseDate<- filter(Esport_Clean,TotalPlayers >= 200)
ReleaseDate2<-ReleaseDate%>%
  group_by(ReleaseDate)%>%
  summarise(n())


  
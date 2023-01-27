library(readxl)
ebp05 <- read_excel("~/Desktop/ggplot2/ebp05.xlsx")
ebp05
head(ebp05)

co <- ebp05[ebp05$TEAM == "CONTROL", ]
t1 <- ebp05[ebp05$TEAM == "MG1000D14", ]
t2 <- ebp05[ebp05$TEAM == "MG3000D14", ]


library(ggplot2)


#GLU


#optikos elegxos kanonikotitas
hist(co$GLU, col="dark red", xlab="Control", breaks=5, ylab="GLU", main="GLU of control")
hist(t1$GLU, col="dark red", xlab="Control", breaks=5, ylab="GLU", main="GLU of team1")
hist(t2$GLU, col="dark red", xlab="Control", breaks=5, ylab="GLU", main="GLU of team2")

#statistikos elegxos kanonikotitas
shapiro.test(co$GLU)
shapiro.test(t1$GLU)
shapiro.test(t2$GLU)

qplot(TEAM, GLU, data=ebp05, geom="boxplot")


fit1<- aov(GLU~TEAM, data=ebp05)
fit1
summary(fit1)

Tuk1<-TukeyHSD(fit1,conf.level =0.99)

par(las=1)
par(mar=c(5,10,2,2))
plot(Tuk1)
for(i in 1:length(Tuk1$TEAM[,1])){text(x=Tuk1$TEAM[i,1],y=length(Tuk1$TEAM[,1])-i+1.3,labels=round(-log10(Tuk1$TEAM[i,4]),digits=3),cex=0.4)}

m_aggr<-aggregate(ebp05$GLU, by=list(ebp05$TEAM), FUN=mean)
str
group_means<-data.frame(TEAM=m_aggr[,1], Wt=m_aggr[,2])

myplot<-ggplot(ebp05, aes(TEAM,GLU), colour=Species)

myplot +
geom_jitter(aes(colour=TEAM)) +
facet_grid(~TEAM)+labs(x="") +
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
geom_hline(aes(yintercept = Wt, colour=TEAM), data=group_means)


#BUN


#optikos elegxos kanonikotitas
hist(co$BUN, col="dark red", xlab="Control", breaks=5, ylab="BUN", main="BUN of control")
hist(t1$BUN, col="dark red", xlab="Control", breaks=5, ylab="BUN", main="BUN of team1")
hist(t2$BUN, col="dark red", xlab="Control", breaks=5, ylab="BUN", main="BUN of team2")

#statistikos elegxos kanonikotitas
shapiro.test(co$BUN)
shapiro.test(t1$BUN)
shapiro.test(t2$BUN)

qplot(TEAM, BUN, data=ebp05, geom="boxplot")

fit2<- aov(BUN~TEAM, data=ebp05)
fit2
summary(fit2)

Tuk2<-TukeyHSD(fit2,conf.level =0.99)

par(las=1)
par(mar=c(5,10,2,2))
plot(Tuk2)
for(i in 1:length(Tuk2$TEAM[,1])){text(x=Tuk2$TEAM[i,1],y=length(Tuk2$TEAM[,1])-i+1.3,labels=round(-log10(Tuk2$TEAM[i,4]),digits=3),cex=0.4)}

m_aggr<-aggregate(ebp05$BUN, by=list(ebp05$TEAM), FUN=mean)
str
group_means<-data.frame(TEAM=m_aggr[,1], Wt=m_aggr[,2])

myplot<-ggplot(ebp05, aes(TEAM,BUN), colour=Species)

myplot +
geom_jitter(aes(colour=TEAM)) +
facet_grid(~TEAM)+labs(x="") +
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
geom_hline(aes(yintercept = Wt, colour=TEAM), data=group_means)


#CREA


#optikos elegxos kanonikotitas
hist(co$CREA, col="dark red", xlab="Control", breaks=5, ylab="CREA", main="CREA of control")
hist(t1$CREA, col="dark red", xlab="Control", breaks=5, ylab="CREA", main="CREA of team1")
hist(t2$CREA, col="dark red", xlab="Control", breaks=5, ylab="CREA", main="CREA of team2")

#statistikos elegxos kanonikotitas
shapiro.test(co$CREA)
shapiro.test(t1$CREA)
shapiro.test(t2$CREA)

qplot(TEAM, CREA, data=ebp05, geom="boxplot")

fit3<- aov(CREA~TEAM, data=ebp05)
fit3
summary(fit3)

Tuk3<-TukeyHSD(fit3,conf.level = 0.99)

par(las=1)
par(mar=c(5,10,2,2))
plot(Tuk3)
for(i in 1:length(Tuk3$TEAM[,1])){text(x=Tuk3$TEAM[i,1],y=length(Tuk3$TEAM[,1])-i+1.3,labels=round(-log10(Tuk3$TEAM[i,4]),digits=3),cex=0.4)}

m_aggr<-aggregate(ebp05$CREA, by=list(ebp05$TEAM), FUN=mean)
str
group_means<-data.frame(TEAM=m_aggr[,1], Wt=m_aggr[,2])

myplot<-ggplot(ebp05, aes(TEAM,CREA), colour=Species)

myplot +
geom_jitter(aes(colour=TEAM)) +
facet_grid(~TEAM)+labs(x="") +
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
geom_hline(aes(yintercept = Wt, colour=TEAM), data=group_means)


#TCHOL

#optikos elegxos kanonikotitas
hist(co$TCHOL, col="dark red", xlab="Control", breaks=5, ylab="TCHOL", main="TCHOL of control")
hist(t1$TCHOL, col="dark red", xlab="Control", breaks=5, ylab="TCHOL", main="TCHOL of team1")
hist(t2$TCHOL, col="dark red", xlab="Control", breaks=5, ylab="TCHOL", main="TCHOL of team2")

#statistikos elegxos kanonikotitas
shapiro.test(co$TCHOL)
shapiro.test(t1$TCHOL)
shapiro.test(t2$TCHOL)

qplot(TEAM, TCHOL, data=ebp05, geom="boxplot")

fit4<- aov(TCHOL~TEAM, data=ebp05)
fit4
summary(fit4)

Tuk4<-TukeyHSD(fit4,conf.level =0.99)

par(las=1)
par(mar=c(5,10,2,2))
plot(Tuk4)
for(i in 1:length(Tuk4$TEAM[,1])){text(x=Tuk4$TEAM[i,1],y=length(Tuk4$TEAM[,1])-i+1.3,labels=round(-log10(Tuk4$TEAM[i,4]),digits=3),cex=0.4)}

m_aggr<-aggregate(ebp05$TCHOL, by=list(ebp05$TEAM), FUN=mean)
str
group_means<-data.frame(TEAM=m_aggr[,1], Wt=m_aggr[,2])

myplot<-ggplot(ebp05, aes(TEAM,TCHOL), colour=Species)

myplot +
geom_jitter(aes(colour=TEAM)) +
facet_grid(~TEAM)+labs(x="") +
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
geom_hline(aes(yintercept = Wt, colour=TEAM), data=group_means)


#TGL

#optikos elegxos kanonikotitas
hist(co$TGL, col="dark red", xlab="Control", breaks=5, ylab="TGL", main="TGL of control")
hist(t1$TGL, col="dark red", xlab="Control", breaks=5, ylab="TGL", main="TGL of team1")
hist(t2$TGL, col="dark red", xlab="Control", breaks=5, ylab="TGL", main="TGL of team2")

#statistikos elegxos kanonikotitas
shapiro.test(co$TGL)
shapiro.test(t1$TGL)
shapiro.test(t2$TGL)

qplot(TEAM, TGL, data=ebp05, geom="boxplot")
fit5<- aov(TGL~TEAM, data=ebp05)
fit5
summary(fit5)

Tuk5<-TukeyHSD(fit5,conf.level =0.99 )

par(las=1)
par(mar=c(5,10,2,2))
plot(Tuk5)
for(i in 1:length(Tuk5$TEAM[,1])){text(x=Tuk5$TEAM[i,1],y=length(Tuk5$TEAM[,1])-i+1.3,labels=round(-log10(Tuk5$TEAM[i,4]),digits=3),cex=0.4)}

m_aggr<-aggregate(ebp05$TGL, by=list(ebp05$TEAM), FUN=mean)
str
group_means<-data.frame(TEAM=m_aggr[,1], Wt=m_aggr[,2])

myplot<-ggplot(ebp05, aes(TEAM,TGL), colour=Species)

myplot +
geom_jitter(aes(colour=TEAM)) +
facet_grid(~TEAM)+labs(x="") +
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
geom_hline(aes(yintercept = Wt, colour=TEAM), data=group_means)


#AST

#optikos elegxos kanonikotitas
hist(co$GLU, col="dark red", xlab="Control", breaks=5, ylab="AST", main="AST of control")
hist(t1$GLU, col="dark red", xlab="Control", breaks=5, ylab="AST", main="AST of team1")
hist(t2$GLU, col="dark red", xlab="Control", breaks=5, ylab="AST", main="AST of team2")

#statistikos elegxos kanonikotitas
shapiro.test(co$AST)
shapiro.test(t1$AST)
shapiro.test(t2$AST)

#Plot AST by group 
library("ggpubr")
ggboxplot(ebp05, x="TEAM", y= "AST", color = "TEAM", palette = c("#00AFBB", "#E7B800", "#FC4E07"), order = c("CONTROL","MG1000D14", "MG3000D14"), ylab="AST", xlab = "Teams")

# Mean plots
# Plot AST by TEAM
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)

ggline(ebp05, x = "TEAM", y = "AST", 
       add = c("mean_se", "jitter"), 
       order = c("CONTROL", "MG1000D14", "MG3000D14"),
       ylab = "AST", xlab = "Teams")

#Kruskal Wallis test

kruskal.test(AST ~ TEAM, data = ebp05)


#ALT

#optikos elegxos kanonikotitas
hist(co$ALT, col="dark red", xlab="Control", breaks=5, ylab="GLU", main="ALT of control")
hist(t1$ALT, col="dark red", xlab="Control", breaks=5, ylab="GLU", main="ALT of team1")
hist(t2$ALT, col="dark red", xlab="Control", breaks=5, ylab="GLU", main="ALT of team2")

#statistikos elegxos kanonikotitas
shapiro.test(co$ALT)
shapiro.test(t1$ALT)
shapiro.test(t2$ALT)

qplot(TEAM, ALT, data=ebp05, geom="boxplot")
fit7<- aov(ALT~TEAM, data=ebp05)
fit7
summary(fit7)

Tuk7<-TukeyHSD(fit7,conf.level =0.99)

par(las=1)
par(mar=c(5,10,2,2))
plot(Tuk7)
for(i in 1:length(Tuk7$TEAM[,1])){text(x=Tuk7$TEAM[i,1],y=length(Tuk7$TEAM[,1])-i+1.3,labels=round(-log10(Tuk7$TEAM[i,4]),digits=3),cex=0.4)}

m_aggr<-aggregate(ebp05$ALT, by=list(ebp05$TEAM), FUN=mean)
str
group_means<-data.frame(TEAM=m_aggr[,1], Wt=m_aggr[,2])

myplot<-ggplot(ebp05, aes(TEAM,ALT), colour=Species)

myplot +
geom_jitter(aes(colour=TEAM)) +
facet_grid(~TEAM)+labs(x="") +
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
geom_hline(aes(yintercept = Wt, colour=TEAM), data=group_means)


#TP

#optikos elegxos kanonikotitas
hist(co$TP, col="dark red", xlab="Control", breaks=5, ylab="GLU", main="TP of control")
hist(t1$TP, col="dark red", xlab="Control", breaks=5, ylab="GLU", main="TP of team1")
hist(t2$TP, col="dark red", xlab="Control", breaks=5, ylab="GLU", main="TP of team2")

#statistikos elegxos kanonikotitas
shapiro.test(co$TP)
shapiro.test(t1$TP)
shapiro.test(t2$TP)

qplot(TEAM, TP, data=ebp05, geom="boxplot")

fit8<- aov(TP~TEAM, data=ebp05)
fit8
summary(fit8)

Tuk8<-TukeyHSD(fit8,conf.level =0.99)

par(las=1)
par(mar=c(5,10,2,2))
plot(Tuk8)
for(i in 1:length(Tuk8$TEAM[,1])){text(x=Tuk8$TEAM[i,1],y=length(Tuk8$TEAM[,1])-i+1.3,labels=round(-log10(Tuk8$TEAM[i,4]),digits=3),cex=0.4)}

m_aggr<-aggregate(ebp05$TP, by=list(ebp05$TEAM), FUN=mean)
str
group_means<-data.frame(TEAM=m_aggr[,1], Wt=m_aggr[,2])

myplot<-ggplot(ebp05, aes(TEAM,TP), colour=Species)

myplot +
geom_jitter(aes(colour=TEAM)) +
facet_grid(~TEAM)+labs(x="") +
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
geom_hline(aes(yintercept = Wt, colour=TEAM), data=group_means)


#HCT

#optikos elegxos kanonikotitas
hist(co$GLU, col="dark red", xlab="Control", breaks=5, ylab="HCT", main="HCT of control")
hist(t1$GLU, col="dark red", xlab="Control", breaks=5, ylab="HCT", main="HCT of team1")
hist(t2$GLU, col="dark red", xlab="Control", breaks=5, ylab="HCT", main="HCT of team2")

#statistikos elegxos kanonikotitas
shapiro.test(co$HCT)
shapiro.test(t1$HCT)
shapiro.test(t2$HCT)

qplot(TEAM, HCT, data=ebp05, geom="boxplot")

fit9<- aov(HCT~TEAM, data=ebp05)
fit9
summary(fit9)

Tuk9<-TukeyHSD(fit9,conf.level =0.99)

par(las=1)
par(mar=c(5,10,2,2))
plot(Tuk9)
for(i in 1:length(Tuk9$TEAM[,1])){text(x=Tuk9$TEAM[i,1],y=length(Tuk9$TEAM[,1])-i+1.3,labels=round(-log10(Tuk8$TEAM[i,4]),digits=3),cex=0.4)}

m_aggr<-aggregate(ebp05$HCT, by=list(ebp05$TEAM), FUN=mean)
str
group_means<-data.frame(TEAM=m_aggr[,1], Wt=m_aggr[,2])

myplot<-ggplot(ebp05, aes(TEAM,HCT), colour=Species)

myplot +
geom_jitter(aes(colour=TEAM)) +
facet_grid(~TEAM)+labs(x="") +
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
geom_hline(aes(yintercept = Wt, colour=TEAM), data=group_means)


#WBC

#optikos elegxos kanonikotitas
hist(co$GLU, col="dark red", xlab="Control", breaks=5, ylab="WBC", main="WBC of control")
hist(t1$GLU, col="dark red", xlab="Control", breaks=5, ylab="WBC", main="WBC of team1")
hist(t2$GLU, col="dark red", xlab="Control", breaks=5, ylab="WBC", main="WBC of team2")

#statistikos elegxos kanonikotitas
shapiro.test(co$WBC)
shapiro.test(t1$WBC)
shapiro.test(t2$WBC)

qplot(TEAM, WBC, data=ebp05, geom="boxplot")
fit10<- aov(WBC~TEAM, data=ebp05)
fit10
summary(fit10)

Tuk10<-TukeyHSD(fit10,conf.level =0.99)

par(las=1)
par(mar=c(5,10,2,2))
plot(Tuk10)
for(i in 1:length(Tuk10$TEAM[,1])){text(x=Tuk10$TEAM[i,1],y=length(Tuk10$TEAM[,1])-i+1.3,labels=round(-log10(Tuk10$TEAM[i,4]),digits=3),cex=0.4)}

m_aggr<-aggregate(ebp05$WBC, by=list(ebp05$TEAM), FUN=mean)
str
group_means<-data.frame(TEAM=m_aggr[,1], Wt=m_aggr[,2])

myplot<-ggplot(ebp05, aes(TEAM,WBC), colour=Species)

myplot +
geom_jitter(aes(colour=TEAM)) +
facet_grid(~TEAM)+labs(x="") +
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
geom_hline(aes(yintercept = Wt, colour=TEAM), data=group_means)


#PLI

#optikos elegxos kanonikotitas
hist(co$GLU, col="dark red", xlab="Control", breaks=5, ylab="GLU", main="GLU of control")
hist(t1$GLU, col="dark red", xlab="Control", breaks=5, ylab="GLU", main="GLU of team1")
hist(t2$GLU, col="dark red", xlab="Control", breaks=5, ylab="GLU", main="GLU of team2")

#statistikos elegxos kanonikotitas
shapiro.test(co$PLI)
shapiro.test(t1$PLI)
shapiro.test(t2$PLI)

qplot(TEAM, PLI, data=ebp05, geom="boxplot")

fit11<- aov(PLI~TEAM, data=ebp05)
fit11
summary(fit11)

Tuk11<-TukeyHSD(fit11,conf.level =0.99)

par(las=1)
par(mar=c(5,10,2,2))
plot(Tuk11)
for(i in 1:length(Tuk11$TEAM[,1])){text(x=Tuk11$TEAM[i,1],y=length(Tuk11$TEAM[,1])-i+1.3,labels=round(-log10(Tuk11$TEAM[i,4]),digits=3),cex=0.4)}

m_aggr<-aggregate(ebp05$PLI, by=list(ebp05$TEAM), FUN=mean)
str
group_means<-data.frame(TEAM=m_aggr[,1], Wt=m_aggr[,2])

myplot<-ggplot(ebp05, aes(TEAM,PLI), colour=TEAM)

myplot +
geom_jitter(aes(colour=TEAM)) +
facet_grid(~TEAM)+labs(x="") +
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
geom_hline(aes(yintercept = Wt, colour=TEAM), data=group_means)


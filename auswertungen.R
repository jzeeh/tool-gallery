cccddd

Mittelwert_Studienmitte<-rowSums(mitte_neu[10:13],na.rm = TRUE)/ rowSums(is.na(mitte_neu[,10:13])==FALSE)

ectsunderwerb<- cbind(Mittelwert_Studienmitte,mitte_neu$X25a)
ectsunderwerb <- as.data.frame(ectsunderwerb)

ECTS<-ectsunderwerb[,1] 
Erwerbsstunden<-ectsunderwerb[,2]
grenzen <- c(0,10,20,30,60)
Erwerbsstunden <- cut(Erwerbsstunden, breaks=grenzen)
Erwerbsstunden <- factor(Erwerbsstunden, labels=c("1-10","11-20","21-30", "31-40"))
colSums(table(Mittelwert_Studienmitte,Erwerbsstunden))

mean(Mittelwert_Studienmitte[Erwerbsstunden>=31 & Erwerbsstunden<=60])


stdabw <- function(x) {n=length(x) ; sqrt(var(x) * (n-1) / n)}
stdabw(na.omit(Mittelwert_Studienmitte[Erwerbsstunden>=31 & Erwerbsstunden<=60]))
###

Mittelwert_Studienbeginn<-rowSums(beginner[10:13],na.rm = TRUE)/ rowSums(is.na(beginner[,10:13])==FALSE)
ectsunderwerb<- cbind(Mittelwert_Studienbeginn,beginner$X41a)
ectsunderwerb <- as.data.frame(ectsunderwerb)

ECTS<-ectsunderwerb[,1] 
Erwerbsstunden<-ectsunderwerb[,2]
grenzen <- c(0,10,20,30,60)
Erwerbsstunden <- cut(Erwerbsstunden, breaks=grenzen)
Erwerbsstunden <- factor(Erwerbsstunden, labels=c("1-10","11-20","21-30", "31-40"))
colSums(table(Mittelwert_Studienbeginn,Erwerbsstunden))

mean(na.omit(Mittelwert_Studienbeginn[Erwerbsstunden>0 & Erwerbsstunden<=10]))

###

stdabw(na.omit(Mittelwert_Studienbeginn[Erwerbsstunden>0 & Erwerbsstunden<=10]))

## Vereinbarkeit
colSums(table(Mittelwert_Studienbeginn,beginner$X41b))

mean(na.omit(Mittelwert_Studienbeginn[beginner$X41b==1]))
mean(na.omit(Mittelwert_Studienbeginn[beginner$X41b==2]))
mean(na.omit(Mittelwert_Studienbeginn[beginner$X41b==3]))
mean(na.omit(Mittelwert_Studienbeginn[beginner$X41b==4]))
mean(na.omit(Mittelwert_Studienbeginn[beginner$X41b==5]))

stdabw(na.omit(Mittelwert_Studienbeginn[beginner$X41b==1]))


###

mitte_neu$X25c

colSums(table(Mittelwert_Studienmitte,mitte_neu$X25c))

mean(na.omit(Mittelwert_Studienmitte[mitte_neu$X25c==1]))
mean(na.omit(Mittelwert_Studienmitte[mitte_neu$X25c==2]))
mean(na.omit(Mittelwert_Studienmitte[mitte_neu$X25c==3]))
mean(na.omit(Mittelwert_Studienmitte[mitte_neu$X25c==4]))
mean(na.omit(Mittelwert_Studienmitte[mitte_neu$X25c==5]))

stdabw(na.omit(Mittelwert_Studienmitte[mitte_neu$X25c==1]))

####

F1gr <- cbind(beginner$X21b,beginner$X21c,beginner$X21d,beginner$X21e,beginner$X21g)
F1gr <-rowSums(F1gr[,1:5],na.rm = TRUE)/ rowSums(is.na(F1gr[,1:5])==FALSE)

colSums(table(Mittelwert_Studienbeginn,F1gr))

#############
# Entscheidung studieren
Mittelwert_Studienbeginn<-rowSums(beginner[10:13],na.rm = TRUE)/ rowSums(is.na(beginner[,10:13])==FALSE)
beginner_entsch <- subset(beginner,beginner$X14!=5)
Entscheidung_Studieren <- beginner_entsch$X14
Mittelwert_Studienbeginn<-rowSums(beginner_entsch[10:13],na.rm = TRUE)/ rowSums(is.na(beginner_entsch[,10:13])==FALSE)
summary(lm(Mittelwert_Studienbeginn~Entscheidung_Studieren))

### entsch studium
beginner_neu <- subset(beginner,  beginner$X15!=5)
beginner_neu$MW.ECTS<-rowSums(beginner_neu[10:13],na.rm = TRUE)/ rowSums(is.na(beginner_neu[,10:13])==FALSE)
Entscheidung_Studium <- beginner_neu$X15

summary(lm(beginner_neu$MW.ECTS~Entscheidung_Studium ))

### studienberechtigung
Berecht <- 0 ### Dummy Studienberechtigungspr?fung, f?r alle anderen auch bilden?
Berecht[beginner$X5==7] <- 1 
Berecht[beginner$X5!=7] <- 0
t.test(Mittelwert_Studienbeginn~Berecht)

## stunden
ectsunderwerb<- cbind(Mittelwert_Studienbeginn,beginner$X41a)
ectsunderwerb <- as.data.frame(ectsunderwerb)

ECTS<-ectsunderwerb[,1] 
Erwerbsstunden<-ectsunderwerb[,2]

summary(lm(Mittelwert_Studienbeginn~Erwerbsstunden ))

## berufliche gr?nde
F1gr <- cbind(beginner$X21b,beginner$X21c,beginner$X21d,beginner$X21e,beginner$X21g)
F1gr <-rowSums(F1gr[,1:5],na.rm = TRUE)/ rowSums(is.na(F1gr[,1:5])==FALSE)
summary(lm(Mittelwert_Studienbeginn~F1gr ))

## neigungen
F2gr <- cbind(beginner$X21t,beginner$X21u)
F2gr <-rowSums(F2gr[,1:2],na.rm = TRUE)/ rowSums(is.na(F2gr[,1:2])==FALSE)
summary(lm(Mittelwert_Studienbeginn~F2gr ))

## unterst?tzung
F1 <- cbind(beginner$X32c ,beginner$X32f,beginner$X32g)
F1 <-rowSums(F1[,1:3],na.rm = TRUE)/ rowSums(is.na(F1[,1:3])==FALSE)
summary(lm(Mittelwert_Studienbeginn~F1 ))

##
F3 <- cbind(beginner$X32a ,beginner$X32b)
F3 <-rowSums(F3[,1:2],na.rm = TRUE)/ rowSums(is.na(F3[,1:2])==FALSE)
summary(lm(Mittelwert_Studienbeginn~F3 ))

### F4 <- cbind(beginner$X32d_neu ,beginner$X32e_neu)
beginner$X32d_neu <- 6-beginner$X32d
beginner$X32e_neu <- 6-beginner$X32e
F4 <- cbind(beginner$X32d_neu ,beginner$X32e_neu)
F4 <-rowSums(F4[,1:2],na.rm = TRUE)/ rowSums(is.na(F4[,1:2])==FALSE)
summary(lm(Mittelwert_Studienbeginn~F4 ))

##
Vereinbarkeit <- beginner$X41b
summary(lm(Mittelwert_Studienbeginn~Vereinbarkeit  ))

### alter und erwerbsstunden 
Alter1 <- recode(beginner$Alter, recodes='"15"="NA";"902"="NA"; "1919"="19"; "1920"="20"; "1921"="21"; "1922"="22";"1923"="23";"1924"="24"; "1925"="25";"1926"="26";"1927"="27"')
Alter1 <- as.numeric(Alter1)


summary(lm(Erwerbsstunden ~ Alter1))
summary(lm(Vereinbarkeit~Erwerbsstunden ))

mitte_neu <- subset(mitte, beginn_num >= 9)
F1 <- cbind(mitte_neu$X17a ,mitte_neu$X17b,mitte_neu$X17k)
F1 <-rowSums(F1[,1:3],na.rm = TRUE)/ rowSums(is.na(F1[,1:3])==FALSE)

Mittelwert_Studienmitte<-rowSums(mitte_neu[10:13],na.rm = TRUE)/ rowSums(is.na(mitte_neu[,10:13])==FALSE)
summary(lm(Mittelwert_Studienmitte~F1))

F2 <- cbind(mitte_neu$X17i ,mitte_neu$X17j)
F2 <-rowSums(F2[,1:2],na.rm = TRUE)/ rowSums(is.na(F2[,1:2])==FALSE)
summary(lm(Mittelwert_Studienmitte~F2))


F3 <- cbind(mitte_neu$X17d ,mitte_neu$X17h)
F3 <-rowSums(F3[,1:2],na.rm = TRUE)/ rowSums(is.na(F3[,1:2])==FALSE)
summary(lm(Mittelwert_Studienmitte~F3))

F5 <- cbind(mitte_neu$X17c ,mitte_neu$X17e)
F5 <-rowSums(F5[,1:2],na.rm = TRUE)/ rowSums(is.na(F5[,1:2])==FALSE)
summary(lm(Mittelwert_Studienmitte~F5))


angs1 <- mitte_neu$X17c
angs2 <- mitte_neu$X17e


Pflichtschule <- 0 ### Dummy Pflichtschle, f?r alle anderen auch bilden?
Pflichtschule[mitte_neu$X31b==1] <- 1 
Pflichtschule[mitte_neu$X31b!=1] <- 0
t.test(Mittelwert_Studienbeginn~Berecht)


Erwerbsstunden <- mitte_neu$X25a
summary(lm(Mittelwert_Studienmitte~Erwerbsstunden))

Vereinbarkeit <- mitte_neu$X25c
summary(lm(Mittelwert_Studienmitte~Vereinbarkeit))

Studium <- 1
Studium[mitte_neu$Studium==1 & mitte_neu$X1==1] <- 0
Studium[mitte_neu$Studium==2 & mitte_neu$X1==2] <- 1

t.test(Mittelwert_Studienmitte~Studium)

Hauptstudium <- mitte_neu$X3-1
t.test(Mittelwert_Studienmitte~Hauptstudium)


summary(lm(Vereinbarkeit~Erwerbsstunden))

summary(lm(Hauptstudium~Studium))

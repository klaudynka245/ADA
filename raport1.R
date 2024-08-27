library('tidyverse')
personel <- read.csv2('personel.csv',header=FALSE)
names(personel) <- c('D','S','A1','A2','W1','W2','P','Wiek','Wyk')
names(personel)
glimpse(personel)
personel$A2[personel$A2 == '11'] = '1'
personel <- personel %>% mutate(across(D:Wyk,as.factor))
glimpse(personel)
library(kable)
library(binom)
library('kableExtra')
personel

#ZADANIE 1

#Sposób 1
table(personel$A1)

# Sposób 2
personel %>% count(A1) %>% mutate(prop=n/sum(n))

personel %>% filter(Wyk=='1') %>% count(A1)

tab_A1_all <- count(A1)
tab_A1_DZ <- personel %>% filter(D=='Z') %>% count(A1)
tab_A1_DP <- personel %>% filter(D=='P') %>% count(A1)
tab_A1_DS <- personel %>% filter(D=='S') %>% count(A1)
tab_A1_DO <- personel %>% filter(D=='O') %>% count(A1)
tab_A1_PK <- personel %>% filter(P=='K') %>% count(A1)
tab_A1_PM <- personel %>% filter(P=='M') %>% count(A1)
tab_A1_Wyk1 <- personel %>% filter(Wyk=='1') %>% count(A1)
tab_A1_Wyk2 <- personel %>% filter(Wyk=='2') %>% count(A1)
tab_A1_Wyk3 <- personel %>% filter(Wyk=='3') %>% count(A1)

tab_W1_all <- count(W1)
tab_W1_DZ <- personel %>% filter(D=='Z') %>% count(W1)
tab_W1_DP <- personel %>% filter(D=='P') %>% count(W1)
tab_W1_DS <- personel %>% filter(D=='S') %>% count(W1)
tab_W1_DO <- personel %>% filter(D=='O') %>% count(W1)
tab_W1_PK <- personel %>% filter(P=='K') %>% count(W1)
tab_W1_PM <- personel %>% filter(P=='M') %>% count(W1)
tab_W1_Wyk1 <- personel %>% filter(Wyk=='1') %>% count(W1)
tab_W1_Wyk2 <- personel %>% filter(Wyk=='2') %>% count(W1)
tab_W1_Wyk3 <- personel %>% filter(Wyk=='3') %>% count(W1)

# ZADANIE 2

#Sposób 1
table(personel$W1,personel$D)

# Sposób 2
ftable(personel,col.vars='W1',row.vars='D') %>% kable()

#Sposób 3
library(vcd)
structable(W1~P,personel) %>% addmargins()

structable(W1~P,personel) %>% addmargins()
structable(W1~S,personel) %>% addmargins()
structable(A1~D,personel) %>% addmargins()

# ZADANIE 3

#Sposób 1
barplot(table(personel$W1))
pie(table(personel$W1))

df_pie <- personel %>% count(A1) %>% data.frame()
df_pie

ggplot(df_pie, aes(x="", y=A1, fill=n)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


# ZADANIE 4

mosaic(~D+A1, personel)
mosaic(~D+W1, personel)
mosaic(~S+P, personel)
mosaic(~P+W1, personel)

1 wszystkie tablice z 1 (20 tablic) bez komentarza i kodzik ale nie do całości, przykładowa linijka, no i fajnie
2 3 tablice + kodzik przykładowy jeden a nie cały skrypt wiadomo
3 kołowy słupkowy 4 wykresy 2*2=4 umiemy liczyć, jedno pie, jedno bara bara dosłownie, DOSŁOWNIE 2 zdania komentarza
4 4 mozaikowe, jedno wywołanie bo może oszukamy i do każdego 
komentarz (1,1.5,2 zdania mozna wybrać kochana) taki jak nawijała że baby do garów a nie kierowniczki

##### CZĘŚĆ 2

# ZADANIE 5
sample(1:10,5)
sample(1:10,5,replace=TRUE)
sample(c('Tak','Nie'),5,replace=TRUE)
sample(c('Tak','Nie'),5,replace=TRUE,prob=c(0.9,0.1))

d <- data(mtcars)
f <- function(x){
  if (x==1){
    s <- sample(1:nrow(mtcars),3,replace=TRUE)
  } else{
    s <- sample(1:nrow(mtcars),3)
  }
  mtcars[s, ]
}

f(1)

# ZADANIE 6
library(likert)

# Za pomocą funkcji likert, podajemy dane czyli odp na pytania i ta funkcja zwraca na wyjściu obiekt typu likert i ten obiekt dajemy do plota/summary i mamy podsumowanko 
# Jako 1 argument dajemy dataframe ze zmiennymi do których odpowiedzi mamy w tej skali, grouping- dodatkowy wektor po którym grupujemy (opcjonalnie)

#Summary to informacja o procentach odpowiedzi, zwraca tabelke, też średnia i std

# Plot - dajemy wynik funkcji likert, można sprecyzować jaki plot chcemy widzieć, (argument type: bar, heat, density)

# Podsumowanie dla A1 i A2, 3 wersje bo 3 ploty
# A1 i A2 pogrupowane po zmiennej D

# Jak się grupuje to nie można heata

# A1 pogrupowane po zmiennej P

#W danych jest błąd X DDDDD, w pytaniu A2 mamy debila, zmienić 11 na 1!! 

#Jak robimy wykresy to typiara nie chce nazw zmiennych które są mało informacyjne (np. A1 na jakąś fajniejszą opisową żeby lepiej wyglądało)

likt <- data.frame((personel$A1,personel$A2))

#LUB

likt <- likert(select(personel,A1,A2))

#WERSJA Z GRUPOWANKIEM
likt <- likert(data.frame(personel$A1,personel$A2),grouping=personel$D)
summary(likt)
plot(likt,type='bar')

#-----------------------#
personel <- read.csv2('personel.csv',header=FALSE)
names(personel) <- c('D','S','A1','A2','W1','W2','P','Wiek','Wyk')
personel$A2[personel$A2 == '11'] = '1'


personel$D[personel$D == 'O'] = 'Obsługa kadrowo-płacowa'
personel$D[personel$D == 'P'] = 'Produkcja'
personel$D[personel$D == 'Z'] = 'Zaopatrzenia'
personel$D[personel$D == 'S'] = 'Sprzedaż'

personel$S[personel$S == 1] = 'Tak'
personel$S[personel$S == 0] = 'Nie'

personel$A1[personel$A1 == -2] = 'Zdecydowanie się nie zgadzam'
personel$A1[personel$A1 == -1] = 'Nie zgadzam się'
personel$A1[personel$A1 == 0] = 'Trudno powiedzieć'
personel$A1[personel$A1 == 1] = 'Zgadzam się'
personel$A1[personel$A1 == 2] = 'Zdecydowanie się zgadzam'

personel$A2[personel$A2 == -2] = 'Zdecydowanie się nie zgadzam'
personel$A2[personel$A2 == -1] = 'Nie zgadzam się'
personel$A2[personel$A2 == 0] = 'Trudno powiedzieć'
personel$A2[personel$A2 == 1] = 'Zgadzam się'
personel$A2[personel$A2 == 2] = 'Zdecydowanie się zgadzam'

personel$W1[personel$W1 == -2] = 'Zdecydowanie się nie zgadzam'
personel$W1[personel$W1 == -1] = 'Nie zgadzam się'
personel$W1[personel$W1 == 0] = 'Trudno powiedzieć'
personel$W1[personel$W1 == 1] = 'Zgadzam się'
personel$W1[personel$W1 == 2] = 'Zdecydowanie się zgadzam'

personel$W2[personel$W2 == -2] = 'Zdecydowanie się nie zgadzam'
personel$W2[personel$W2 == -1] = 'Nie zgadzam się'
personel$W2[personel$W2 == 0] = 'Trudno powiedzieć'
personel$W2[personel$W2 == 1] = 'Zgadzam się'
personel$W2[personel$W2 == 2] = 'Zdecydowanie się zgadzam'

personel$P[personel$P == 'K'] = 'Kobieta'
personel$P[personel$P == 'M'] = 'Mężczyzna'

personel


df <- data.frame(personel$,personel$A2)
colnames(df) <- c('Atmosfera przed wyjazdem', 'Atmosfera po wyjeździe')
likt_atmo <- likert(df)
summary(likt_atmo)
likert.bar.plot(likt_atmo) + scale_fill_discrete(labels=c('Zdecydowanie się nie zgadzam', 'Nie zgadzam się','Trudno powiedzieć','Zgadzam się','Zdecydowanie się zgadzam')) + guides(fill=guide_legend(nrow=2,byrow=TRUE,title='Odpowiedź')) 
plot(likt_atmo,type='heat')
plot(likt_atmo,type='density')

likert.density.plot(likt_atmo)+ scale_x_discrete(
  labels = c('Zdecydowanie się nie zgadzam', 'Nie zgadzam się','Trudno powiedzieć','Zgadzam się','Zdecydowanie się zgadzam'))

likt_atmo_gr_D <- likert(data.frame(personel$A1,personel$A2),grouping=personel$D)
likt_atmo_gr_D

summary(likt_atmo_gr_D)
plot(likt_atmo_gr_D,type='bar')
likert.bar.plot(likt_atmo_gr_D)
plot(likt_atmo_gr_D,type='density')

likt_atmo_gr_P <- likert(data.frame(personel$A1,personel$A2),grouping=personel$P)
summary(likt_atmo_gr_P)
plot(likt_atmo_gr_P,type='bar')
plot(likt_atmo_gr_P,type='density')


likt_atmo_gr_D <- likert(data.frame(personel$,personel$A2),grouping=personel$D)
likt_atmo_gr_D
f(likt_atmo_gr_D$group)

class(likt_atmo_gr_D)


likt_atmo_gr_D <- likert(data.frame(personel$A1,personel$A2),grouping=personel$D)
likt_atmo_gr_D
summary(likt_atmo_gr_D)
likert.bar.plot(likt_atmo_gr_D) + scale_fill_discrete(labels=c('Zdecydowanie się nie zgadzam', 'Nie zgadzam się','Trudno powiedzieć','Zgadzam się','Zdecydowanie się zgadzam')) + guides(fill=guide_legend(nrow=2,byrow=TRUE,title='Odpowiedź'))+ 
  ylab("Procent") + xlab('Dział')

likert.density.plot(likt_atmo_gr_D,legend='Dział')

df <- data.frame(personel$A1,personel$A2)
colnames(df) <- c('Atmosfera przed wyjazdem', 'Atmosfera po wyjeździe')
likt_atmo <- likert(df)
summary(likt_atmo)
likert.bar.plot(likt_atmo) + scale_fill_discrete(labels=c('Zdecydowanie się nie zgadzam', 'Nie zgadzam się','Trudno powiedzieć','Zgadzam się','Zdecydowanie się zgadzam')) + guides(fill=guide_legend(nrow=2,byrow=TRUE,title='Odpowiedź')) + theme(axis.text.y=element_text(angle=45,hjust=1)) + 
  ylab("Procent") 


# ZADANIE 7

#Funkcja która przyjmuje n-liczbę prób, poziom ufności, i p, wzór na przedział z wykładu
#Użytkownik podaje albo wektor x (z 1 i 0 jako porażkami i sukcesami), albo i x i n, (x to realizacja z rozkładu dwumianowego)
#funkcja qbeta to kwantyle 
#Implementacja własnej funkcji
#Sprawdzanko z wbudowaną funkcją
#Dla naszych danych policzyć przedziały ufności




personel <- read.csv2('personel.csv',header=FALSE)
names(personel) <- c('D','S','A1','A2','W1','W2','P','Wiek','Wyk')
personel$A2[personel$A2 == '11'] = '1'


personel$D[personel$D == 'O'] = 'Obsługa kadrowo-płacowa'
personel$D[personel$D == 'P'] = 'Produkcja'
personel$D[personel$D == 'Z'] = 'Zaopatrzenia'
personel$D[personel$D == 'S'] = 'Sprzedaż'

personel$S[personel$S == 1] = 'Tak'
personel$S[personel$S == 0] = 'Nie'

personel$A1[personel$A1 == -2] = 'Zdecydowanie się nie zgadzam'
personel$A1[personel$A1 == -1] = 'Nie zgadzam się'
personel$A1[personel$A1 == 0] = 'Trudno powiedzieć'
personel$A1[personel$A1 == 1] = 'Zgadzam się'
personel$A1[personel$A1 == 2] = 'Zdecydowanie się zgadzam'

personel$A2[personel$A2 == -2] = 'Zdecydowanie się nie zgadzam'
personel$A2[personel$A2 == -1] = 'Nie zgadzam się'
personel$A2[personel$A2 == 0] = 'Trudno powiedzieć'
personel$A2[personel$A2 == 1] = 'Zgadzam się'
personel$A2[personel$A2 == 2] = 'Zdecydowanie się zgadzam'

personel$W1[personel$W1 == -2] = 'Zdecydowanie się nie zgadzam'
personel$W1[personel$W1 == -1] = 'Nie zgadzam się'
personel$W1[personel$W1 == 0] = 'Trudno powiedzieć'
personel$W1[personel$W1 == 1] = 'Zgadzam się'
personel$W1[personel$W1 == 2] = 'Zdecydowanie się zgadzam'

personel$W2[personel$W2 == -2] = 'Zdecydowanie się nie zgadzam'
personel$W2[personel$W2 == -1] = 'Nie zgadzam się'
personel$W2[personel$W2 == 0] = 'Trudno powiedzieć'
personel$W2[personel$W2 == 1] = 'Zgadzam się'
personel$W2[personel$W2 == 2] = 'Zdecydowanie się zgadzam'

personel$P[personel$P == 'K'] = 'Kobieta'
personel$P[personel$P == 'M'] = 'Mężczyzna'
names(personel) <- c('Dział','S','Atmosfera przed wyjazdem','Atmosfera po wyjeździe','Ocena wynagrodzenia przed wyjazdem','Ocena wynagrodzenia po wyjeżdzie','Płeć','Wiek','Wykształcenie')

personel <- personel %>% mutate(across(Dział:Wyk,as.factor))
personel
glimpse(personel)

daneW1 <- personel %>% count(`Ocena wynagrodzenia przed wyjazdem`) %>% data.frame()

daneW1

ggplot(daneW1, aes(x=Ocena.wynagrodzenia.przed.wyjazdem, y=n)) + 
  geom_bar(stat = "identity", fill="hotpink") + 
  ggtitle("Wykres słupkowy dla oceny wykształcenia przed wyjazdem")

df <- data.frame(personel$`Atmosfera przed wyjazdem`,personel$`Atmosfera po wyjeździe`)
colnames(df) <- c('Atmosfera przed wyjazdem', 'Atmosfera po wyjeździe') 
likt_atmo <- likert(df)
summary(likt_atmo)
likert.bar.plot(likt_atmo) + guides(fill=guide_legend(nrow=2,byrow=TRUE,title='Odpowiedź')) + theme(axis.text.y=element_text(angle=45,hjust=1)) + 
  ylab("Procent") 

plot(likt_atmo,type='heat') + theme(axis.text.x=element_text(angle=90,hjust=1),axis.text.y=element_text(angle=45,hjust=1)) + guides(fill=guide_legend(title='Odpowiedź'))

likert.density.plot(likt_atmo)


df <- data.frame(personel$`Atmosfera przed wyjazdem`,personel$`Atmosfera po wyjeździe`)
likt_atmo_gr_D <- likert(,grouping=personel$Dział)
likt_atmo_gr_D
summary(likt_atmo_gr_D)
likert.bar.plot(likt_atmo_gr_D) + guides(fill=guide_legend(nrow=2,byrow=TRUE,title='Odpowiedź'))+ 
  ylab("Procent") + xlab('Dział')

likert.density.plot(likt_atmo_gr_D,legend='Dział')



likt_atmo_gr_P <- likert(data.frame(personel$`Atmosfera przed wyjazdem`,personel$`Atmosfera po wyjeździe`),grouping=personel$Płeć)
summary(likt_atmo_gr_P)

likert.bar.plot(likt_atmo_gr_P) + guides(fill=guide_legend(nrow=2,byrow=TRUE,title='Odpowiedź'))+ 
  ylab("Procent") + xlab('Płeć')

likert.density.plot(likt_atmo_gr_P,legend='Płeć')

# ZADANIE 8

# Udowodnić że jest p i 1-p najłatwiej z funkcji charakterystycznej, najpierw dla y potem dla sumy y, korzystając z tego napisać funcję generuj_dwumianowy(n,N,p) N-długość próby losowej
# DODATKOWE: Znaleźć metodę generowania tego co niżej tylko inną metodą, porównanie szybkości działania zaproponowanej przez gwiazdę i tej mojej

generuj_dwumianowy <- function(n,p,N){
  vec <- rep(NA,size=N)
  for (i in 1:N){
    vec2 <- rep(NA,size=n)
    for (j in 1:n){
      y <- sample(c(0,1),size=1,prob=c(1-p,p))
      vec2[j] <- y
    }
    x <- sum(vec2)
    vec[i] <- x
  }
  return(vec)
}

gd <- generuj_dwumianowy(20,0.6,1000)

rb <- rbinom(1000,20,0.6)

mean(gd)
mean(rb)

var(gd)
var(rb)

x<- 1:20
hist(gd,prob=TRUE,col='hotpink',xlim=c(4,20), ylim=c(0,.2))
par(new = TRUE)
plot(x,dbinom(x,20,0.6), col='black',xlim=c(4,20), ylim=c(0,.2))


# ZADANIE 9

#asymptotic to ta na W, exact to clopper i jakaś dodatkowa

x <- rbinom(1,20,0.6)
x
binom.confint(x,20,conf.level=0.95,methods='all')


y <- 1:30
x <- rbinom(y,30,0.6)
binom.confint(x,30,conf.level=0.95,methods='exact')

library("dplyr") 

f <- function(met,n,N,p){
  counter <- 0
  len <- rep(NA,size=N)
  for (i in 1:N){
    x <- rbinom(1,30,0.6)
    m <- binom.confint(x,30,conf.level=0.95,methods=met)
    if (between(p,m$lower,m$upper)){
      counter <- counter + 1
    }
    len[i] <- m$upper-m$lower
  }
  per_pokr <- counter/N
  sr <- mean(len)
  return (list(per_pokr,sr))
}


pr <- seq(from = 0, to = 1, by= 0.05)
exact_pr <- rep(NA,length(pr))
exact_pr2 <- rep(NA,length(pr))
for (i in 1:length(pr)){
  exact_pr[i] <- f('exact',30,1000,pr[i])[1] 
  exact_pr2[i] <- f('exact',30,1000,pr[i])[2]
}
exact_pr
plot(pr,exact_pr)
plot(pr,exact_pr2)

# ZADANIE 10
x <- rbinom(1,100,0.5)
binom.test(x,100,p=0.5,alternative='g')

prop.test(x,100,0.5,'g',0.95,FALSE)
prop.test(x,100,0.5,'g',0.95,TRUE)

x <- rbinom(1,100,0.5)
y <- rbinom(1,60,0.2)
prop.test(c(x,y),c(100,60),alternative='t',correct=TRUE)

prop.test(c(x,y),c(100,60),c(0.5,0.2))
#NIE TRZEBA 10 W RAPORCIE ♥♥♥♥

#ZADANIE 11
count(personel,P,S)

count(personel,W2)

kob_kier <- 8
m_kier <- 19
l_kobiet <- 71
l_mezcz <- 129
prop.test(c(kob_kier,m_kier),c(l_kobiet,l_mezcz))

kob_kier/l_kobiet
m_kier/l_mezcz

count(personel,P,W1)

prop.test(c(36,70),c(71,129))

count(personel,P,D)
prop.test(c(23,3),c(71,129),alternative='l')


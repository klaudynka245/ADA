library('tidyverse')
personel <- read.csv2('personel.csv',header=FALSE)
names(personel) <- c('D','S','A1','A2','W1','W2','P','Wiek','Wyk')
names(personel)
glimpse(personel)
personel$A2[personel$A2 == '11'] = '1'
personel <- personel %>% mutate(across(D:Wyk,as.factor))
glimpse(personel)

library(binom)
library(vcd)
library(pracma)
library(ca)
library(BSDA)

#ZADANIE 4
personel_new <- personel
personel_new$W1[personel_new$W1 == '-2'] = '-1'
personel_new$W1[personel_new$W1 == '2'] = '1'
personel_new$W2[personel_new$W2 == '-2'] = '-1'
personel_new$W2[personel_new$W2 == '2'] = '1'
personel_new <- personel_new %>% mutate(across(D:Wyk,as.factor))
personel_new$W2
tabela <- ftable(personel_new$W1, personel_new$W2)
tabela
n <- sum(rowSums(tabela))
n
P <- tabela/n
P
r <- rowSums(P)
r
c <- colSums(P)


#TEST Z

test_z <- function(tabela){
  n <- sum(rowSums(tabela))
  P <- tabela/n
  r <- rowSums(P)
  c <- colSums(P)
  D <- r[1] - c[1]
  sigma2_D <- (r[1]*(1-r[1])+c[1]*(1-c[1])-2*(P[1,1]*P[2,2]-P[1,2]*P[2,1]))/n
  Z <- D/sqrt(sigma2_D)
  p = 2*(1 - pnorm(abs(Z)))
  return(p)
}

D = r[1] - c[1]
sigma2_D = (r[1]*(1-r[1])+c[1]*(1-c[1])-2*(P[1,1]*P[2,2]-P[1,2]*P[2,1]))/n
Z = D/sqrt(sigma2_D)
Z
p = 2*(1-dnorm(abs(Z)))
p
p = 1-2*dnorm(abs(Z))
p

#TEST Z0
test_z0 <- function(tabela){
  n <- sum(rowSums(tabela))
  P <- tabela/n
  r <- rowSums(P)
  c <- colSums(P)
  D <- r[1] - c[1]
  sigma2_D <- (tabela[1,2]+tabela[2,1])/n^2
  Z_0 <- D/sqrt(sigma2_D0)
  p <- 1-2*dnorm(abs(Z_0))
  return(p)
}


sigma2_D0 <- (tabela[1,2]+tabela[2,1])/n^2
Z_0 <- D/sqrt(sigma2_D0)
p <- 1-2*dnorm(abs(Z_0))
p


#mcnemary
mcnemar.test(tabela)
mcnemar.test(tabela, correct=FALSE)$p.value


# ZADANIE 4
test_z <- function(tabela){
  n <- sum(tabela)
  P <- tabela/n
  r <- rowSums(P)
  c <- colSums(P)
  D <- r[1] - c[1]
  sigma2_D <- (r[1]*(1-r[1])+c[1]*(1-c[1])-2*(P[1,1]*P[2,2]-P[1,2]*P[2,1]))/n
  Z <- D/sqrt(sigma2_D)
  p = 2*(1 - pnorm(abs(Z)))
  return(p)
}

moc <- function(n){
  MC <- 1000
  p2 <- seq(0.01,0.99,0.01)
  p1 <- 0.5
  m <- length(p2)
  res <- rep(NA, m)
  for (i in 1:m){
    counter <- 0
    for (j in 1:MC){
    X <- factor(sample(c("1","0"), n, replace=TRUE, prob = c(p1,1-p1)), levels = 0:1)
    Y <- factor(sample(c("1","0"), n, replace=TRUE, prob = c(p2[i],1-p2[i])), levels=0:1)
    tab <- ftable(X,Y)
    if (test_z(tab) < 0.05){
      counter <- counter+1
      }
    }
    res[i] <- counter/MC
  }
return(data.frame( 'prob' = p2, 'results' = res))
}

m_20 <- moc(20)
plot(m_20)


# ZADANIE 5

library(data.table)

dane <- read.csv2('personel.csv',header=FALSE, )
names(personel) <- c('D','S','A1','A2','W1','W2','P','Wiek','Wyk')
dane <- fread("personel.csv", select = c(2,5,9))
names(dane) <- c('S', 'W1', 'Wyk')
dane <- mutate(dane, across(c('S','W1','Wyk'), as.factor))
dane <- ftable(dane)
dane <- as.table(dane)
df <- as.data.frame(dane)
df

model_a <- glm(Freq ~ S + Wyk, 
               data = df, family = poisson)
summary(model_a)

1-pchisq(deviance(model_a), df = df.residual(model_a))
cbind(model_a$data, fitted(model_a))

exp(2.1823-1.8575+1.2281-0.7691)
exp(-1.8575+1.2281-0.7691)

model_b <- glm(Freq ~ S + Wyk + S*Wyk, 
               data = df, family = poisson)
summary(model_b)
exp(2.3026-3.6889+1.1233-1.3863+1.7099+3.5835)
cbind(model_b$data,fitted(model_b))

p_b <- 1-pchisq(deviance(model_b), df = df.residual(model_b))
p_b


model_a <- glm(Freq ~ S + Wyk, 
               data = df, family = poisson)
p_a <- 1-pchisq(deviance(model_a), df = df.residual(model_a))
p_a



# Zadanie 9 
[1 2 3] model w a np., poszukać na podstawie tego co napisane, 
1 wersja przeciwko modelowi pełnemu
a 2 przeciwko innemu z H_0 który nie jest pełnym 

a) [1 2 3] przeciwko [123] i przeciwko [12 3]
można przy pomocy glma i to będzie ten z zerowej, przeciwko pełnemu,
przeciwko innemu to anova w przykładzie

dla każdego podpunktu zidentyfikować model, przeprowadzić testy z modelem pełnym i nadmodel modelu z zerowej, 
kroki z przykładu 
hipotezy + p-wartości i wnioski 
fragmenty kodu z dopasowaniem modelu 

#Zadanie 10
a) musimy wybrać model początkowy, np. ten [1 2 3], potem chcemy przeprowadzić testy istotności interakcji, czy jeśli dodamu 
interakcję polepszy się dopasowanie, to te testy z poprzedniego zadania

wychodzimy od modelu, H_0 np [1 2 3] a w alternatywnej nadmodel np [1 23], sprawdzamy czy zerowa odrzucona
jeśli tak to ten model z interakcją jest lepszy i wtedy on się staje modelem z zerowej i znowu dołączamy interakcję np. [12 23]
i znowu sprawdzamy, jak odrzucona zerowa to kolejna interakcja

bc) kryterium informacyjne, np koncentrujemy się na uporządkowanych hierarchiczne modele i wybieramy ten dla którego
kryterium informacyjne przyjmuje najmniejszą wartość, 19 modeli dopasować i dla każdego kryterium informacyjne AIC BIC 

2 sposób wychodzimy od pewnego modelu usuwamy dodajemy interakcję tak długo jak to powoduje zmniejszenie wartości kryterium 
funkcja step




# Zadanie 7
model <- glm(Freq ~ S+W1+Wyk+S*Wyk+W1*Wyk, data = df, family = poisson)
result <- cbind(model$data, fitted(model))
result$`fitted(model)` <- result$`fitted(model)`/sum(result$`fitted(model)`)
result$Freq <- result$Freq/sum(result$Freq)


sum(result$`fitted(model)`[result$S == 1 & result$W1 == 2])/(sum(result$`fitted(model)`[result$S == 1]))
sum(result$Freq[result$S == 1 & result$W1 == 2])/(sum(result$Freq[result$S == 1]))

sum(result$`fitted(model)`[result$Wyk == 1 & result$S == 1])/(sum(result$`fitted(model)`[result$Wyk == 1]))
sum(result$Freq[result$S == 1 & result$Wyk == 1])/(sum(result$Freq[result$Wyk == 1]))

sum(result$`fitted(model)`[result$Wyk == 3 & result$S == 0])/(sum(result$`fitted(model)`[result$Wyk == 3]))
sum(result$Freq[result$S == 0 & result$Wyk == 3])/(sum(result$Freq[result$Wyk == 3]))


df <- as.data.frame(ftable(personel$S,personel$P,personel$Wyk))
names(df) <- c('S', 'P', 'Wyk', 'Freq')

model <- glm(Freq ~ S+P+Wyk+S*Wyk+P*Wyk, data = df, family = poisson)
result <- cbind(model$data, fitted(model))
result$`fitted(model)` <- result$`fitted(model)`/sum(result$`fitted(model)`)
result$Freq <- result$Freq/sum(result$Freq)


sum(result$`fitted(model)`[result$S == 1 & result$P == 'K'])/(sum(result$`fitted(model)`[result$S == 1]))
sum(result$Freq[result$S == 1 & result$P == 'K'])/(sum(result$Freq[result$S == 1]))

sum(result$`fitted(model)`[result$S == 1 & result$Wyk == 1])/(sum(result$`fitted(model)`[result$Wyk == 1]))
sum(result$Freq[result$S == 1 & result$Wyk == 1])/(sum(result$Freq[result$Wyk == 1]))

sum(result$`fitted(model)`[result$P == 'M' & result$Wyk == 3])/(sum(result$`fitted(model)`[result$Wyk == 3]))
sum(result$Freq[result$P == 'M' & result$Wyk == 3])/(sum(result$Freq[result$Wyk == 3]))

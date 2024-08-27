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
library(psych)


## ZADANIE 1
#tablica dwudzielcza jako x, albo 2 wektory, conf.int - przedział ufności, 


#ZADANIE 2
#jaka jest zerowa, jaka alternatywna, przedstawić dane na raporcie
#użyć prop.test, badanie niezalezności jest równoważne badaniu hipotezy o heterogeniczności
fisher.test(ftable(personel$S,personel$P))

#ZADANIE 3
fisher.test(ftable(personel$S, personel$Wiek))

fisher.test(ftable(personel$S, personel$Wyk))

# ZADANIE 4
fisher.test(ftable(personel$W1, personel$S))

fisher.test(ftable(personel$W1, personel$Wyk))

fisher.test(ftable(personel$W1, personel$P))

fisher.test(ftable(personel$W1, personel$Wiek), workspace=271020)


# ZADANIE 6
#W1 i S, tabela w raporcie, oba testy, p-value, napisać jakie p-value, sformułować wniosek
# i odnieść się do 4a

alpha <- 0.01
chisq.test(ftable(personel$W1,personel$S))$p.value
assocstats(ftable(personel$W1,personel$S))$chisq_tests[,3][1]


# ZADANIE 7
# rozmiar testu to błąd pierwszego rodzaju
#
rmultinom(1,10,c(0.1,0.4,0.5))


# ZADANIE 8
#test na niezależność zmiennych, tau lub gamma, gamma jak są porządkowe, DescTools( tablica ) - 1. to tau, druga to gamma



# ZADANIE 9
# %*% mnożenie macierzowe, funkcja svd do dekompozycji 
f <- ftable(personel$Wyk, personel$W1)
f
f <- as.matrix(f)
f
l1 <- sum(f[1,])
length(f[,1])
l1

analiza.korespondencji <- function(tabela){
  n <- length(tabela[,1])
  m <- length(tabela[1,])
  n <- sum(rowSums(tabela))
  P <- tabela/n
  r <- rowSums(P)
  c <- colSums(P)
  d_r <- diag(r)
  d_c <- diag(c)
  R <- inv(d_r)
  C <- inv(d_c)
  A <- inv(d_r ^ (1/2)) %*% (P - r %*% t(c)) %*% inv(d_c ^ (1/2))
  total_inertia <- tr(t(A) %*% A)
  A <- svd(A)
  Gamma <- diag(A$d)
  U <- A$u
  V <- A$v
  F_ <- inv(d_r^(1/2)) %*% U %*% Gamma
  G <- inv(d_c^(1/2)) %*% V %*% Gamma
  F_ <- F_[,1:2]
  G <- G[,1:2]
  xs_row <- F_[,1] #współrzędne x dla wierszy
  ys_row <- F_[,2] #współrzędne y dla wierszy
  xs_col <- G[,1] #współrzędne x dla kolumn
  ys_col <- G[,2] #współrzędne y dla kolumn
  gam <- A$d ^ 2
  dim1 <- round(sum(gam[1])/sum(gam), 3) * 100
  dim2 <- round(sum(gam[2])/sum(gam), 3) * 100
  df_row <- data.frame('Dim.1' = xs_row, 'Dim.2' = ys_row, row.names = rownames(tabela))
  df_col <- data.frame('Dim.1' = xs_col, 'Dim.2' = ys_col, row.names = colnames(tabela))
  
  ggplot() + geom_point(aes(x=df_row$Dim.1, y=df_row$Dim.2), color='blue', shape = 16) +
    geom_text(aes(x=df_row$Dim.1, y=df_row$Dim.2),label=rownames(df_row), 
              nudge_x = 0, nudge_y = 0.02, size=2.5, color='blue') +
    geom_point(aes(x=df_col$Dim.1, y=df_col$Dim.2), color='red', shape=17) +
    geom_text(aes(x=df_col$Dim.1, y=df_col$Dim.2),label=rownames(df_col), 
              nudge_x = 0, nudge_y = -0.02, size=2.5, color='red') +
    xlab(paste('Dim 1 (', as.character(round(dim1,2)), '%)')) +
    ylab(paste('Dim 2 (', as.character(round(dim2,2)), '%)'))
}

p(f)

plot(ca(f))


# ZADANIE 10
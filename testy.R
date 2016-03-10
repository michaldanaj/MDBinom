# TODO: Add comment
# 
# Author: Piotr
###############################################################################

n <- 1000
x <- rnorm(n)
y <- rbinom(n, size=1, prob=0.3)
reg_nieparam(x,y)

#czy b³¹d dla y=NA
reg_nieparam(c(x,1),c(y,NA))

#czy b³¹d dla x=NA
reg_nieparam(score=c(NA, x,NA),default=c(3,y,1), buckets=2, na.omit=FALSE)

buckety_br(c(NA, x,NA),c(3,y,1), 20, method = "eq_count")




setwd('G:\\Michal\\eclipse - java\\MDbinom')
library(lineprof)
source("testy_prof.R")
l <- lineprof(f(),interval = 0.000001)
shine(l)



wynik<-univariate_loop(x_df=dane_tr, y=dane_tr$churn2_8)

genRaport(wynik, dir="c:/temp/raport")



univariate_anal_stats1(x=c(2,2,2,2), y=c(1,0,0,0))

univariate_anal_stats(x=c(2,2,2,2), y=c(1,0,0,0))

univariate_anal_stats(x=dane_tr$Nazwisko_Nazwa, y=dane_tr$churn2_8)

wyniki<-univariate_loop(dane_tr, dane_tr$churn2_8)
genRaport(wyniki, dir="c:/temp/raport")


wynik<-univariate_anal_stats(dane_tr$Wiek, dane_tr$churn2_8, czas=dane_tr$miesiacZamowienia)
genRaport(wynik)

windows()
reg_nieparam(score=dane_tr$Wiek, default=dane_tr$churn2_8)


model<-glm(churn2_8~Wiek, data=dane_tr, family='binomial')
pred<-predict(model, type='response')

dopasowanie_do_zmiennej(dane_tr$Wiek, pred, wynik[[1]])


make_model_formula("daa",c("a","b"))

step_bez_kor(data=dane_tr[,c("AboCenaPodstawowa","ModemCenaPodstawowa")], model=model, target_var_name='churn2_8')


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

univariate_anal_stats1<-function(x,y, 
		locfit=FALSE, 
		discrete_threshold=numeric_var_treatment.params$discrete_threshold,
		NA_substit = numeric_var_treatment.params$NA_substit,
		special_val=numeric_var_treatment.params$spcial_val, 
		no_stats_threshold=numeric_var_treatment.params$spcial_val,
		max_gleb=3, 
		min_bucket=200, 
		interactive=FALSE,
		breaks=NULL, 
		mapping=NULL, 
		forceContinous=FALSE,
		span=0.9,...){
	

univariate_anal_stats1(x=c(2,2,2,2), y=c(1,0,0,0))

univariate_anal_stats(x=c(2,2,2,2), y=c(1,0,0,0))

univariate_anal_stats(x=dane_tr$Nazwisko_Nazwa, y=dane_tr$churn2_8)

wyniki<-univariate_loop(dane_tr, dane_tr$churn2_8)
genRaport(wyniki, dir="c:/temp/raport")



library(MDBinom)
library(testthat)

context("Testy funkcji rg_nieparam")

#bckt_br z powyższym

set.seed(1)

x <- c(rep(1,100),1:100)
y <- rbinom(200, 1, 0.1)
weights <- abs(rnorm(200))
breaks=c(1,50,100)

bckt <- rg_nieparam(score=x, default=y, plot=FALSE, buckets = 3)

test_that("rg_nieparam czy liczba obserwacji z bucketów daje liczbę w Totalu, bez wag", {
  expect_equal(sum(bckt$n_obs), length(x))
})

test_that("rg_nieparam czy średnia z bucketów daje średnią w Totalu, bez wag", {
  expect_equal(sum(bckt$br * bckt$n_obs) / sum(bckt$n_obs), mean(y))
})


bckt <- rg_nieparam(score=x, default=y, plot=FALSE, buckets = 3, weights = weights)

test_that("rg_nieparam czy liczba obserwacji z bucketów daje liczbę w Totalu, przy wykorzystaniu wag", {
  expect_equal(sum(bckt$n_obs), sum(weights))
})

test_that("rg_nieparam czy średnia z bucketów daje średnią w Totalu, przy wykorzystaniu wag", {
  expect_equal(sum(bckt$br * bckt$n_obs) / sum(bckt$n_obs), sum(y * weights) / sum(weights))
})


### Uwzględnianie wag w regresji nieparametrycznej ###

grid <- -50:50/10
grid <- -5:5
x<-vector()
y<-vector()
for (point in grid){
  print(point)
  exp_y <- 1/(1+exp(-point))
  y_temp <- rep(0,100);
  y_temp[1:100/100<exp_y] <- 1
  x <- c(x, rep(point,100))
  y <- c(y, y_temp)
}


weight <- rep(1,length(x))
weight[y==0] <- 10
rg_nieparam(x,y, glm = TRUE, weights = weight)


test_that("rg_nieparam czy uwzględnione są wagi w regresji nieparametrycznej", {
  #expect_equal(sum(bckt$br * bckt$n_obs) / sum(bckt$n_obs), sum(y * weights) / sum(weights))
})

#TODO
#Dokończyć test sprawdzający czy wagi są poprawnie uwzględniane dla 
#regresji logistycznej i nieparametrycznej

#dodać test na rysunek inny niż 'br'



model <- glm(y~x, family=binomial)
pred <- predict(model, type='response')
  
rg_nieparam(score = x, default = y, pred = pred, col_pred = 'red')
  
  ("Czy poprawnie rysuje logit dla pred")

rg_nieparam(score = x, default = y, pred = pred, col_pred = 'red',
            plt_type = 'logit')

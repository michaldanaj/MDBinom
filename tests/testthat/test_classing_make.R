# TODO: Add comment
# 
# Author: Piotr
###############################################################################

library(MDBinom)
library(testthat)

context("Testy dyskretyzacji z wagami")

set.seed(1)

x <- c(rep(1,100),1:100)
y <- rbinom(200, 1, 0.1)
w <- rnorm(200)


x <- c(1)
y <- c(1)
buckety_br(x,y,10)
#bckt_br(x,y,10)




x <- c(1,1,1,1,1)
y <- c(0,0,0,0,0) 
bckt <- MDBinom::bckt_stat(x,y,1)

test_that("bckt_stat czy Total jest ostatnim wierszem", {
  expect_equal(bckt$label[2], 'TOTAL')
})

bckt <- bckt_stat2(breaks = c(0,1), x = x, y = y, weights = 1)
test_that("bckt_stat2 czy Total jest ostatnim wierszem", {
  expect_equal(bckt$label[nrow(bckt)], 'TOTAL')
})

test_that("bckt_stat test typu kolumny discrete", {
  expect_equal(typeof(bckt_stat(x,y,1)$discret), 'double')
  expect_equal(typeof(bckt_stat(as.character(x),y,1)$discret), 'character')
})


#bckt_br z powyższym

set.seed(1)

x <- c(rep(1,100),1:100)
y <- rbinom(200, 1, 0.1)
w <- abs(rnorm(200))
breaks=c(1,50,100)

bckt <- bckt_stat2(breaks,x, y, weights = w)
test_that("bckt_stat2 czy Total jest ostatnim wierszem", {
  expect_equal(bckt$label[nrow(bckt)], 'TOTAL')
})

bckt <- bckt_br(x,y,4,w)
test_that("bckt_br czy Total jest ostatnim wierszem", {
  expect_equal(bckt$label[nrow(bckt)], 'TOTAL')
})

test_that("bckt_br czy poprawnie liczona średnia br na Totalu", {
  expect_equal(bckt$br[nrow(bckt)], sum(y*w)/sum(w))
})

breaks=c(1,1.1,1.2, 1.3,50,100)
bckt <- bckt_stat2(breaks,x, y)
bucket <- buckety_stat2(breaks,x, y)
bckt_bez_total <- bckt_stat2(breaks,x, y, total=FALSE)
test_that("bckt_stat2 czy puste zakresy przedziałów nie wywalą błędu", {
  expect_equal(nrow(bckt), 4)
  expect_equal(nrow(bckt_bez_total), 3)
  expect_equal(bckt$br[-nrow(bckt)], bucket$br)
  expect_equivalent(as.data.frame(bckt_bez_total[,c('br','label')]), bucket[,c('br','label')])
})




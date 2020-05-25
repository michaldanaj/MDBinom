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


### Porównanie wyników starej i nowej funkcji

bck_old <- buckety_stat(x,y)
bck_new <- as.data.frame(bckt_stat(x,y))

names_old <- names(bck_old)
names_new <- names(bck_new)
names_com <- intersect(names_old, names_new)

bck_old2 <- bck_old[, names_com]
bck_new2 <- bck_new[, names_com]

rownames(bck_old2)<-NULL
rownames(bck_new2)<-NULL

test_that("bckt_stat Porównanie starych i nowych wyliczeń", {
  expect_equal(bck_old2[,-c(9, 10)], bck_new2[,-c(9, 10)])
  expect_equal(-bck_old2[-nrow(bck_old), 9], bck_new2[-nrow(bck_new), 9])
  expect_equal(bck_old2[-nrow(bck_old), 10], bck_new2[-nrow(bck_new), 10])
})


### Testy avg



n <- 10000
#x <- rnorm(n)
x <- 6*(1:n/n-0.5)
x <- sample(x,n)
x_range <- round(x)
y_exp <- 1/(1+exp(-x))
y <- rbinom(length(x), 1, prob = y_exp)
#rg_nieparam(x,y)
weights <- rep(1, n)
weights[y==0] <- 4
  
#TODO : dokończyć pisać testy

# 1. bckt_stat nie sortuje poprawnie wartości wymienionych w avg
model <- glm(y~ x, family='binomial')
pred <- predict(model, type='response')
bckt <- bckt_stat(x=x_range,y=y, avg=pred)
bckt[,c('pred','br')]

# 2. bckt_stat nie uwzględnia wag gdy są dodatkowe kolumny avg
model <- glm(y~ x, family='binomial', weights = weights)
pred <- predict(model, type='response')
bckt <- bckt_stat(x=x_range,y=y, avg=pred, weights=weights)
#bckt[,c('discret', 'pred','br', 'n_obs', 'n_bad')]

zostaw <- x_range == -3
avg_y <- sum(y[zostaw]*weights[zostaw])/sum(weights[zostaw])
avg_pre <- sum(pred[zostaw]*weights[zostaw])/sum(weights[zostaw])

test_that("bckt_stat czy poprawnie liczy z wagami dla kolumn avg", {
  expect_equal(bckt$br[1], avg_y)
  expect_equal(bckt$pred[1], avg_pre)
})


# n. dwie kolumny w avg

bckt <- bckt_stat(x=x_range,y=y, avg=data.frame(avg1=pred, avg2=y), weights=weights)

test_that("bckt_stat czy poprawnie liczy gdy w avg jest data.frame z dwoma kolumnami", {
  expect_equal(bckt$avg2, bckt$br)
  expect_equal(length(bckt$avg1), 8)
})


#TODO: wprowadzić popwarność liczenia rg_nieparam z pred na jakimś deterministycznym
#przykładzie
model <- glm(y~ x, family='binomial', weights = weights)
pred <- predict(model, type='response')
rg_nieparam(score=x, default = y, weights = weights, pred=pred, buckets=10)
# 3. bckt_stat z dodatkowymi dwoma kolumnami w avg







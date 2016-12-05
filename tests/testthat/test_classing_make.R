# TODO: Add comment
# 
# Author: Piotr
###############################################################################


x <- c(rep(1,100),1:100)
y <- rbinom(200, 1, 0.1)

x <- c(1)
y <- c(1)
buckety_br(x,y,10)


expect_equal(2,2)

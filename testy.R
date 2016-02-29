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




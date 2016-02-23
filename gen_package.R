# TODO: Add comment
# 
# Author: Piotr
###############################################################################


library(devtools)
library(roxygen2)

setwd('G:\\Michal\\eclipse - java\\MDbinom')
#create('MDBinom')


document()
install('MDBinom')
build()
build(pkg = 'MDBinom', path='.', binary = TRUE)

# TODO: Add comment
# 
# Author: Piotr
###############################################################################


library(devtools)
library(roxygen2)

setwd('G:\\Michal\\eclipse - java\\MDbinom')
#create('MDBinom')	

document()

#check(check_dir =".")

setwd('G:\\Michal\\eclipse - java')
shell('R CMD check  MDBinom', intern=TRUE)

install('MDBinom')
build(pkg = 'MDBinom', path='.', binary = TRUE)

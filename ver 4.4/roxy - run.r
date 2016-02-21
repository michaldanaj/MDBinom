setwd('D:\\Michal\\R pakiety\\MDBinom\\ver 4.4');

library(roxygen2)

shell('copy  MDBinom_2012-08-01.r MDBinom.r', intern=TRUE);
package.skeleton('MDBinom', code_files='MDBinom.R', force=TRUE)

 # `R CMD roxygen -d helloRoxygen' works, too.
#roxygenize('MDBinom', roxygen.dir='MDBinom', copy.package=FALSE, unlink.target=FALSE)
roxygenize('MDBinom')

shell('rmdir MDBinom/inst/*', intern=TRUE);
shell('rmdir MDBinom/inst', intern=TRUE);
shell('copy DESCRIPTION MDBinom/DESCRIPTION', intern=TRUE);
shell('copy NAMESPACE MDBinom/NAMESPACE', intern=TRUE);
shell('R CMD check  MDBinom', intern=TRUE)
#system('R CMD build --binary  MDBinom')
shell('Rcmd build --force --binary   MDBinom', intern=TRUE)

shell('Rcmd INSTALL --build MDBinom', intern=TRUE)
#"C:\Michal\Program Files\R\R-2.15.1patched\bin\i386\Rcmd" INSTALL --build MDBinom

remove.packages('MDBinom');
install.packages(repos=NULL, lib.loc=getwd(), pkgs="MDBinom_4.4.zip")
library(MDBinom)
help(AR_boot)


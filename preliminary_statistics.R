# TODO: Add comment
# 
# Author: Piotr
###############################################################################




Zrobiæ wstêpn¹ analizê, badaj¹c¹ miêdzy innymi

czy jest tylko jedna wartoœæ zmiennej (sd=0)
czy za du¿o wartoœci zmiennej kategoryczne
jedna wartoœæ zmiennej stanowi wiêcej ni¿ x% próby
		zastanowiæ siê nad faktorami
zastanowiæ siê nad zmiennymi numerycznymi

w celu usuniêcia zmiennych, dla których nie ma sensu/nie chcemy robiæ raportów.

Poni¿ej przyk³ad



###############################################################################
######## analiza faktorów ########
###############################################################################


faktory<-sapply(dane, is.factor)
faktory_data<-dane[,faktory]
lapply(faktory_data, summary)

do wyrzucenia

do_wyrzucenia<-c('dzien0', 'pesel6', 'id_plan_taryfowy', 'data_umowy_konw', 'data_kk_konw');

###############################################################################
######## analiza zmiennych ci¹g³ych ########
###############################################################################

ciagle<-sapply(dane, function(x)is.numeric(x) & !is.factor(x))
ciagle_names<-names(dane)[ciagle]

sd_ciagle<-lapply(dane[,ciagle], sd, na.rm=TRUE)
sd_ciagle<-nvl(sd_ciagle,0)

do_wyrzucenia_ciagle<-names(dane)[ciagle][sd_ciagle==0]

ciagle<-ciagle[-match(do_wyrzucenia_ciagle, ciagle_names)]
ciagle_names<-ciagle_names[-match(do_wyrzucenia_ciagle, ciagle_names)]

do_wyrzucenia<-c(do_wyrzucenia, do_wyrzucenia_ciagle)

###############################################################################
######## za du¿y agregat ########
###############################################################################

ciagle_table<-lapply(dane[,ciagle_names], function(x){
			z<-nvl(x,-9999999)
			max(table(z)/length(z))
		}
)
ciagle_table<-unlist(ciagle_table)

do_wyrzucenia<-c(do_wyrzucenia,ciagle_names[ciagle_table>=0.98])
ciagle<- ciagle[ciagle_table<0.98]
ciagle_names<- ciagle_names[ciagle_table<0.98]

###############################################################################
######## logiczne ########
###############################################################################

logiczne_nazwy<-names(dane_tr)[(sapply(dane_tr,function(x)class(x)=='logical'))]
do_wyrzucenia<-c(do_wyrzucenia, logiczne_nazwy)

###############################################################################
######## usuwam daty ########
###############################################################################

daty_names<-ciagle_names[grep('data', ciagle_names)]
do_wyrzucenia<-unique(c(do_wyrzucenia, daty_names))

###############################################################################
######## usuwam zbêdne zmienne ########
###############################################################################

do_wyrzucenia<-c(do_wyrzucenia, 'scorepwa')

do_wyrzucenia_b<-names(dane) %in% do_wyrzucenia

dane2<-dane[,!do_wyrzucenia_b]


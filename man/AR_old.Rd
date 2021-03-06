% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MDBinom.r
\name{AR_old}
\alias{AR_old}
\title{Liczy Accuracy Ratio (GINI)}
\usage{
AR_old(
  score,
  def,
  plot = FALSE,
  return.table = FALSE,
  sort.order = NULL,
  label = "",
  lgd_adjusted = FALSE,
  ...
)
}
\arguments{
\item{score}{wektor    wartości, według który należy posortować wartości \code{def}}

\item{def}{Wektor wartości \code{\{0,1\}}}

\item{plot}{Czy narysować krzywą CAP. Domyślnie \code{FALSE}.}

\item{return.table}{czy zwracać tabelę z agregatami na poziomie pojedynczej 
wartości score}

\item{sort.order}{dla wartości równej "br", zamiast po \code{score}, 
sortuje po wartości \code{br}. Może to być przydatne przy zmiennych dyskretnych.}

\item{label}{opis wyświetlony w legendzie.}

\item{lgd_adjusted}{W przypadku modeli LGD pole pod krzywymi CAP i ROC dla modelu idealnego jest
            mniejsze niż dla modelu PD i nie da się go wyliczyć analogicznym wzorem. Dlatego
dla LGD wyliczana jest osobno powierzchnia dla modelu idealnego i jest ona użyta jako mianownik do
wyliczenia AR.}

\item{...}{dodatkowe parametry.}
}
\value{
Zwraca listę obiektów \code{data.frame}. Elementy tej listy odpowiadają kolejnym
kolumnom ze \code{score_wiele} i mają takie same nazwy, jak kolumny ze \code{score_wiele}.
Każdy element listy ma postać:
 \item{do }{Górna granica przedziału, dla którego podane są statystyki.}
 \item{bad}{Liczba bad-ów w przedziale.}
 \item{obs}{Liczba obserwacji w przedziale.}
 \item{pct_all}{Procentowy udział obserwacji w danym przedziale.}
 \item{pct_bad}{Procentowy udział bad-ów w danym przedziale w stosunku do 
								wszystkich bad-ów.}
 \item{pct_good}{Procentowy udział good-ów w danym przedziale w stosunku do 
								wszystkich good-ów.}
 \item{br}{Stosunek bad-ów do good-ów w danym przedziale (bad rate).}
 \item{AR}{Wyliczone Accuracy Ratio.}
}
\description{
Wylicza Accuracy Ratio (GINI). Wynikiem jest \code{data.frame} przechowujący
informacje pozwalające na wyrysowanie krzywej CAP. W ostatniej kolumnie zapiasna
jest wartość AR.
}
\examples{
n<-1000;
x<-rnorm(n);
def<- as.numeric(x+rnorm(n)<0);
AR(x,def,plot=TRUE);
}
\author{
Michał Danaj
}

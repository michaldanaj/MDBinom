% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MDBinom.r
\name{rg_nieparam}
\alias{rg_nieparam}
\title{Rysuje lokalnie wygładzoną funckję (dev)
 
 Rysuje i zwraca statystyki dla bucketów.}
\usage{
rg_nieparam(
  score,
  default,
  buckets = 100,
  pred = NULL,
  weights = rep(1, length(score)),
  wytnij = 0,
  span = 0.7,
  degree = 2,
  plot = TRUE,
  plt_type = "br",
  new = TRUE,
  col_points = "black",
  col_line = "darkblue",
  col_pred = "green",
  index = FALSE,
  glm = FALSE,
  col_glm = "green",
  ...
)
}
\arguments{
\item{score}{Wektor zmiennych numerycznych.}

\item{default}{Wektor zmiennej dwumianowej.}

\item{buckets}{Liczba bucketów, na ile neleży podzielić \code{score}.}

\item{pred}{predykcja modelu.}

\item{wytnij}{Ile krańcowych obserwacji wyciąć.}

\item{span}{Współczynnik wygładzania. Szegóły w funkcji \code{\link[locfit]{locfit}}}

\item{degree}{Stopień wielomianu do lokalnego wygładzania. Szegóły w funkcji \code{\link[locfit]{locfit}}}

\item{plot}{Czy rysować wykres.}

\item{plt_type}{jeśli \code{br}, to na osi OY będzie BR. W przeciwnym razie będzie logit(BR)}

\item{new}{Czy rysować wykres od nowa.}

\item{col_points}{Kolor punktów.}

\item{col_line}{Kolor lini.}

\item{index}{jeśli \code{TRUE}, na osi OX będą numery kolejnych bucketów.
W przeciwnym razie na osi OX będą wartości \code{score}.}

\item{glm}{czy rysować dopasowanie modelu logistycznego do zmiennej.}

\item{col_glm}{kolor wykresu z modelu logistycznego.}

\item{...}{dodatkowe parametry.}
}
\description{
Rysuje lokalnie wygładzoną funckję (dev)
 
 Rysuje i zwraca statystyki dla bucketów.
}
\examples{
	n<-1000;
	x1<-rnorm(n);
	x2<-x1+rnorm(n);
	y<-(x1+rnorm(n))<0;
	
	reg_nieparam(x1,y, buckets=20)
	reg_nieparam(x2,y, buckets=20, new=FALSE, col_line="green",col_points="green")
}
\author{
Michał Danaj
}

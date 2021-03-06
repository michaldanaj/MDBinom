% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raport.R
\name{genRaport}
\alias{genRaport}
\title{Generuje raport}
\usage{
genRaport(
  wyniki,
  dir = tempdir(),
  kolejnosc = 1:length(wyniki),
  show = TRUE,
  scale = c(0, 0.2)
)
}
\arguments{
\item{wyniki}{lista z wynikami dyskretyzacji itp, z funkcji \code{\link{univariate_anal_stats}}}

\item{dir}{katalog z raportem, jako pełna bezwzględna ścieżka! Katalog musi być stworzony. Domyślnie \code{\link{tempdir()}}.}

\item{kolejnosc}{kolejność wg której zmienne mają być wyświetlone.}

\item{show}{czy na koniec wyświetlić raport w przeglądarce. Domyślnie \code{TRUE}.}

\item{scale}{Skala osi OY.}
}
\description{
Generuje raport
}
\author{
Michał Danaj
}

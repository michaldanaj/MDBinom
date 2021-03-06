% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MDBinom.r
\name{dopasowanie_do_zmiennej}
\alias{dopasowanie_do_zmiennej}
\title{Sprawdzenie, czy estymacja modelu dobrze tłumaczy zależność targetu od zmiennej}
\usage{
dopasowanie_do_zmiennej(x, y_pred, bucket, subset = NULL, ...)
}
\arguments{
\item{x}{wektor z wartościami zmiennej}

\item{y_pred}{wektor z predykcją modelu}

\item{bucket}{grupowanie zmiennej}

\item{subset}{podzbiór}

\item{...}{dodatkowe parametry graficzne}
}
\value{
bucket z dodanymi wyestymowanymi wartościami
}
\description{
Sprawdzenie, czy estymacja modelu dobrze tłumaczy zależność targetu od zmiennej
}
\author{
Michał Danaj
}

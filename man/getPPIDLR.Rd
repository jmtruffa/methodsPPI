\name{getPPIDLR}
\alias{getPPIDLR}
\title{Devuelve el histórico de precios del dólar.}
\usage{
getPPIDLR()
}
\description{
Devuelve el histórico del precio del dolar MEP y CCL y el gráfico.

Lo calcula con GD30 y AL30 desde septiembre 2020. Si se pide fecha previa, lo construye con AY24.

La fecha más atrás que posee es 2014-05-27




Devuelve una lista con dos elementos:

1.- El dataframe con los precios
2.- El gráfico

}
\arguments{
  \item{from}{Fecha desde la cuál se quiere el precio del MEP y CCL}
  \item{to}{Fecha hasta la cuál se quiere el valor}

\examples{
getPPIDLR(from = "2014-05-27", to = Sys.Date())
}







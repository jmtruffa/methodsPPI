\name{getPPIDLR2}
\alias{getPPIDLR2}
\title{Devuelve BID / OFFER de un ticker para Pesos, MEP y CCL}
\usage{
getPPIDLR2()
}
\description{
Devuelve un dataframe con los tickers en pesos, MEP y CCL, con un campo que se llama SIDE donde está el mejor BID y el mejor OFFER encontrado al momento de la consulta.

Se le provee el ticker y completa la C y D al final y devuelve un dataframe con los mejores BID y OFFER.

No hay seguridad que los mismos sean devueltos siempre en el mismo orden, por lo que conviene parsearlos segun los campos SIDE y ticker (si contiene C, D o no contiene porque sería en pesos.)

Esta función puede servir también para calcular el MEP y CCL con CEDEARs
}
\arguments{
  \item{ticker}{De qué ticker se van a traer los mejores BID y OFFER en pesos, MEP y CCL}
  \item{type}{Qué tipo de activo. "BONOS", "CEDEARS". Default "BONOS"}

\examples{
getPPIDLR2(ticker = "GD30", type = "BONOS")
}







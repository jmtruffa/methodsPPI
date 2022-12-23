\name{getPPICurrentRofex}
\alias{getPPICurrentRofex}
\title{Obtener la curva de tasas implícitas y precios de futuros}
\usage{
getPPICurrentRofex()
}
\description{
Devuelve los precios de los futuros con sus TNA Implícitas y un gráfico de la curva.

Toma el spot de la página de MAE: https://www.mae.com.ar/mercado/datos-del-mercado/mae-today a partir de un scrip en Go que está ubicado en:

/analisis financieros/functions/data/bin/maeScraper

Ese script devuelve el último operado que es tomado por la función.

Luego obtiene los precios desde PPI de los futuros y arma el dataset y el gráfico

No posee argumentos ya que devuelve el actual.

}

\examples{
getPPICurrentRofex()
getPPICurrentRofex()[[1]] #solo tabla de futuros
getPPICurrentRofex()[[2]] #solo gráfico
}







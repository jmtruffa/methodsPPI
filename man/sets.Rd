\name{sets}
\alias{sets}
\title{Devuelve sets pre armados de activos}
\usage{
  sets(set)
}
\description{
  Devuelve sets prearmados con su correspondiente "type" para luego pasarlo como parámetro a alguna de las funciones de obtención de precios.

  Posee sets:

    - merval: acciones líders
    - curvaAR: Soberanos (AL, GD)
    - letras: Ledes y Lecer
    - bonosPesos: Bonos pesos
    - cedears
}
\arguments{
  \item{set}{Uno de la lista disponible}

}
\examples{
  sets(set = "merval")
}

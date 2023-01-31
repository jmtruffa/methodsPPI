\name{getPPIFullBook}
\alias{getPPIFullBook}
\title{Devuelve el book de BIDs y OFFERs de los activos pasados en la lista según el uso de la funcion sets}
\usage{
getPPIFullBook(token, ticker, type, settlement)
}
\description{
Devuelve el book de BIDs y OFFERs (máximo 5) de los activos.
Se le pasan listas para que puedan traer datos de distintos tipos de instrumentos dado que está implementado con purrr::pmap_dfr().

Cada parámetro puede ser una lista de varios instrumentos. Sólo tienen que coincidir los tipos de instrumentos con los nombres.

No necesita pedir el Login a PPI ya que la función lo implementa sola, usando un token por cada llamada a la función.

}
\arguments{
  \item{instrumentos}{Los instrumentos, según la descripción que se les da en methodsPPI::sets. ("BONOS", "LETRAS", "CEDEARS", etc).}
  \item{settlement}{"INMEDIATA" o "A-48HS"}
  \item{endpoint}{Cuando use getYields, localmente, si debe pegarle a "yields" o a "apr"}
  \item{fee}{Lista con los fees a aplicar. Ver ejemplo.}


\examples{
fee = tibble(
  letras = 0.00,
  bonos = 0.00
) # esto define las comisiones para los distintos tipos de instrumentos. Asigna letras a LETRAS (de la API de PPI) y el resto la comisión de bonos.
getBook(c("letras", "bonosPesos", "lecer", "bonosCER"), settlement = "INMEDIATA", fee = fee)

Devuelve un df con esta forma:

# A tibble: 186 × 10
# Groups:   ticker, SIDE [38]
   date       ticker SIDE  position price quantity type     fee yield mduration
   <date>     <chr>  <chr>    <dbl> <dbl>    <dbl> <chr>  <dbl> <dbl>     <dbl>
 1 2023-01-31 S28F3  BID          1  95.0   799833 LETRAS     0 1.04     0.0349
 2 2023-01-31 S28F3  BID          2  95     261156 LETRAS     0 1.05     0.0347
 3 2023-01-31 S28F3  BID          3  94.9  1046935 LETRAS     0 1.09     0.0342
 4 2023-01-31 S28F3  BID          4  94.8    10000 LETRAS     0 1.10     0.0339
}







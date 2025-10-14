#' Chequeo con logging para `methodsPPI::getPPIPrices`
#'
#' Envuelve `methodsPPI::getPPIPrices()` para manejar errores y registrar mensajes en el log.
#'
#' @inheritDotParams getPPIPrices
#' @param ... Argumentos pasados a `methodsPPI::getPPIPrices()`.
#'
#' @return
#' Una lista con:
#' \itemize{
#'   \item{`ok`}{ Lógico: `TRUE` si la llamada fue exitosa; `FALSE` si hubo fallos.}
#'   \item{`data`}{ `data.frame` con los datos obtenidos si `ok` es `TRUE`; `NULL` si `ok` es `FALSE`.}
#'   \item{`fail`}{ `data.frame` con los tickers que fallaron si `ok` es `FALSE`; `NULL` si `ok` es `TRUE`.}
#'   \item{`msg`}{ Mensaje descriptivo del resultado.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- check_getPPIPrices(
#'   token$token,
#'   ticker     = lecaps_consultar$ticker,
#'   type       = lecaps_consultar$type,
#'   from       = min(from, max_fecha_lecap + 1),  # cubre huecos si la DB está atrasada
#'   to         = to,
#'   settlement = settlement,
#'   server     = server,
#'   port       = port
#' )
#' if (!res$ok) {
#'   print(res$msg)
#'   print(res$fail)
#' }
#' datos <- res$data
#' }
#'
#' @seealso
#' [methodsPPI::getPPIPrices()], [functions::log_msg()]
#'
#' @export
check_getPPIPrices <- function(...) {
  res <- try(methodsPPI::getPPIPrices(...), silent = TRUE)

  # error duro o estructura inesperada
  if (inherits(res, "try-error") || !is.list(res) || length(res) < 2) {
    msg <- "getPPIPrices falló y no devolvió la lista esperada (result, fail)."
    functions::log_msg(msg, "ERROR")
    return(list(ok = FALSE, data = NULL, fail = NULL, msg = msg))
  }

  result_df <- res[[1]]
  fail_df   <- res[[2]]

  # normalizo por si viniera NULL
  if (is.null(fail_df)) {
    fail_df <- tibble::tibble(ticker = character())
  }

  if (nrow(fail_df) > 0) {
    msg <- sprintf(
      "La función no consiguió datos para %d ticker(s): %s",
      nrow(fail_df),
      paste0(unique(fail_df$ticker), collapse = ", ")
    )
    functions::log_msg(msg, "WARN")
    return(list(ok = FALSE, data = result_df, fail = fail_df, msg = msg))
  }

  functions::log_msg("getPPIPrices ejecutada correctamente (sin fallidos).", "INFO")
  list(ok = TRUE, data = result_df, fail = fail_df, msg = "OK")
}


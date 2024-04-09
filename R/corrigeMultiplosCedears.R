#' corrigeMultiplosCedears
#'
#' Función que corrige los múltiplos de los cedears hacia atrás
#' No tiene todos los CEDEARs. A medida que salen nuevos hay que ir
#' incluyéndolos
#' Hay que pasarle el df que vuelve de PPI
#' Lo devuelve con los precios (columna "price") ajustados
#'
#' @param df el dataframe que devuelve getPPIPriceHistoryMultiple3
#' @return Devuelve el mismo df pero con el campo price de los cedears ajustados.

corrigeMultiplosCedears = function(df) {
  df = df %>%
    mutate(
      price = case_when(
        ticker == "AMZN" & date <= "2022-06-06" ~ price / 19,
        ticker == "GOOGL" & date <= "2022-07-15" ~ price / 19,
        ticker == "AMD" & date <= "2022-11-16" ~ price / 20,
        ticker == "DIS" & date <= "2022-11-16" ~ price / 3,
        ticker == "VIST" & date <= "2022-11-16" ~ price / 5,
        ticker == "V" & date <= "2022-11-16" ~ price / 3,
        ticker == "JNJ" & date <= "2022-11-16" ~ price / 3,
        ticker == "META" & date <= "2022-11-16" ~ price / 3,
        ticker == "MSFT" & date <= "2022-11-16" ~ price / 3,
        ticker == "MCD" & date <= "2022-11-16" ~ price / 3,
        ticker == "AXP" & date <= "2022-11-16" ~ price / 3,
        ticker == "CAR" & date <= "2022-11-16" ~ price / 26,
        ticker == "ASR" & date <= "2022-11-16" ~ price / 10,
        ticker == "NUE" & date <= "2022-11-16" ~ price / 8,
        ticker == "PAC" & date <= "2022-11-16" ~ price / 8,
        ticker == "HSY" & date <= "2022-11-16" ~ price / (21/3),
        ticker == "IFF" & date <= "2022-11-16" ~ price / 6,
        ticker == "TSM" & date <= "2022-11-16" ~ price / 27,
        ticker == "AVY" & date <= "2022-11-16" ~ price / 6,
        ticker == "USB" & date <= "2022-11-16" ~ price / 5,
        ticker == "FCX" & date <= "2022-11-16" ~ price / 3,
        ticker == "CAT" & date <= "2022-11-16" ~ price / 4,
        ticker == "MMC" & date <= "2022-11-16" ~ price / 4,
        ticker == "FMX" & date <= "2022-11-16" ~ price / 3,
        ticker == "SYY" & date <= "2022-11-16" ~ price / 4,
        ticker == "GE" & date <= "2022-11-16" ~ price / 8,
        ticker == "TGT" & date <= "2022-11-16" ~ price / 6,
        ticker == "ADI" & date <= "2022-11-16" ~ price / 5,
        ticker == "MSI" & date <= "2022-11-16" ~ price / 4,
        ticker == "COST" & date <= "2022-11-16" ~ price / 4,
        ticker == "GLOB" & date <= "2022-11-16" ~ price / 3,
        ticker == "NKE" & date <= "2022-11-16" ~ price / 4,
        ticker == "DE" & date <= "2022-11-16" ~ price / 4,
        ticker == "HD" & date <= "2022-11-16" ~ price / 4,
        ticker == "CRM" & date <= "2022-11-16" ~ price / 3,

        TRUE ~ price
      )
    ) %>%
    mutate(

      price = case_when(

        ticker == "MMM" & date <= "2024-01-24" ~ price / 2,
        ticker == "ADGO" & date <= "2024-01-24" ~ price / 2,
        ticker == "ADBE" & date <= "2024-01-24" ~ price / 2,
        ticker == "AEM" & date <= "2024-01-24" ~ price / 2,
        ticker == "AMGN" & date <= "2024-01-24" ~ price / 3,
        ticker == "AAPL" & date <= "2024-01-24" ~ price / 2,
        ticker == "BAC" & date <= "2024-01-24" ~ price / 2,
        ticker == "GOLD" & date <= "2024-01-24" ~ price / 2,
        ticker == "BIOX" & date <= "2024-01-24" ~ price / 2,
        ticker == "CVX" & date <= "2024-01-24" ~ price / 7,
        ticker == "LLY" & date <= "2024-01-24" ~ price / 7,
        ticker == "XOM" & date <= "2024-01-24" ~ price / 2,
        ticker == "FSLR" & date <= "2024-01-24" ~ price / 6,
        ticker == "JD" & date <= "2024-01-24" ~ price / 2,
        ticker == "JPM" & date <= "2024-01-24" ~ price / 3,
        ticker == "MELI" & date <= "2024-01-24" ~ price / 2,
        ticker == "PEP" & date <= "2024-01-24" ~ price / 3,
        ticker == "PFE" & date <= "2024-01-24" ~ price / 2,
        ticker == "PG" & date <= "2024-01-24" ~ price / 3,
        ticker == "RIO" & date <= "2024-01-24" ~ price / 2,
        ticker == "SONY" & date <= "2024-01-24" ~ price / 2,
        ticker == "SBUX" & date <= "2024-01-24" ~ price / 3,
        ticker == "TXR" & date <= "2024-01-24" ~ price / 2,
        ticker == "BA" & date <= "2024-01-24" ~ price / 4,
        ticker == "TM" & date <= "2024-01-24" ~ price / 3,
        ticker == "VZ" & date <= "2024-01-24" ~ price / 2,
        ticker == "VIST" & date <= "2024-01-24" ~ price / 3,
        ticker == "WMT" & date <= "2024-01-24" ~ price / 3,
        ticker == "NFLX" & date <= "2024-01-24" ~ price / 3,


        TRUE ~ price
      )
    )
  df
}

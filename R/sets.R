sets = function(set) {
  require(DBI)
  require(dplyr)
  require(dbplyr)
  if (set != '') {

    con = dbConnect(RSQLite::SQLite(), "~/Google Drive/Mi unidad/data/test.sqlite3")
    res = as_tibble(tbl(con, "sets") %>%
                      filter(nombre == set) %>%
                      select(ticker, type)
    )

  } else {
    stop("Set no puede ser NULL")
  }
  dbDisconnect(con)
  res
}

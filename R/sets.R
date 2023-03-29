sets = function(set, db = "") {
  require(dplyr)
  require(dbplyr)

  if (db == "") {
    if (stringr::str_detect(Sys.info()['nodename'], "Air")) {
      db = "~/data/test.sqlite3"
    } else {
      db = '~/data/test.sqlite3'
    }
  }


  if (set != '') {

    con = DBI::dbConnect(RSQLite::SQLite(), dbname = db)
    res = as_tibble(tbl(con, "sets") %>%
                      filter(nombre == set) %>%
                      select(ticker, type)
    )

  } else {
    stop("Set no puede ser NULL")
  }
  DBI::dbDisconnect(con)
  res
}


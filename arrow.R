
library(tidyverse)

thepkgs <-  installed.packages() %>% as_tibble() %>% pull(Package)

if(!("arrow" %in% thepkgs)){
  
  if (!(Sys.info()[["effective_user"]] %in% c("fabio", "favstats"))) {
    remove.packages("arrow")
  }
  
  Sys.setenv(LIBARROW_MINIMAL = "false")
  Sys.setenv("NOT_CRAN" = "true")
  
  print("##### please install arrow #####")
  
  options(
    HTTPUserAgent =
      sprintf(
        "R/%s R (%s)",
        getRversion(),
        paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
      )
  )
  if (!(Sys.info()[["effective_user"]] %in% c("fabio", "favstats"))) {
    suppressMessages(
      suppressWarnings(
        install.packages(
          "arrow",
          repos = "https://packagemanager.rstudio.com/all/__linux__/focal/latest",
          quiet = TRUE
        )
      )
    )
    arrow::install_arrow(verbose = F) # verbose output to debug install errors
  }
  
}

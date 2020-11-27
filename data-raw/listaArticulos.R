## code to prepare `listaArticulos` dataset goes here
listaArticulos <- readr::read_csv("data-raw/Lista_Ejemplo.csv")
usethis::use_data(listaArticulos, overwrite = TRUE)

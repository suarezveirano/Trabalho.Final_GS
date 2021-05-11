## aplico una funcion para lectura de los 4 archivos y unificarlos

read_multiple <- function(flnm) {
  read_csv(flnm, skip = 4, col_types = cols(.default = "c")) %>%
    mutate(filename = flnm)
}

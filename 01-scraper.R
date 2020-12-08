
library(tidyverse)
library(rvest)
library(readxl)

cc_buscador_territorial <- function(id, year = c("2015", "2019")) {
  
  year <- match.arg(year)
  
  url <- paste0(
    url <- "https://www5.registraduria.gov.co/CuentasClarasPublicoTer",
    year,
    "/Consultas/Candidato/Reporte/",
    id
  )
  
  message(url)
  
  obj <- httr::RETRY("GET", url)
  stopifnot(httr::status_code(obj) == 200)
  website <- httr::content(obj)
  
  info <- website %>% 
    rvest::html_nodes("#form #centro .fuente") %>% 
    rvest::html_text()
  
  href <- website %>% 
    rvest::html_nodes(".enlacexls") %>% 
    rvest::html_attr("href") %>% 
    paste0("https://www5.registraduria.gov.co", .)
  
  formulario <- website %>% 
    rvest::html_nodes(".rounded-cornerform strong span") %>% 
    rvest::html_text()
  
  tibble::tibble(nombre = info[[1]], corporacion = info[[2]], year, formulario, href)
  
}

cc_buscador_legislativo <- function(id, year = c("2014", "2018")) {
  
  year <- match.arg(year)
  
  url <- paste0(
    url <- "https://www5.registraduria.gov.co/CuentasClarasPublicoCon",
    year,
    "/Consultas/Candidato/Reporte/",
    id
  )
  
  message(url)
  
  obj <- httr::RETRY("GET", url)
  stopifnot(httr::status_code(obj) == 200)
  website <- httr::content(obj)
  
  info <- website %>% 
    rvest::html_nodes("#form #centro .fuente") %>% 
    rvest::html_text()
  
  href <- website %>% 
    rvest::html_nodes(".enlacexls") %>% 
    rvest::html_attr("href") %>% 
    paste0("https://www5.registraduria.gov.co", .)
  
  formulario <- website %>% 
    rvest::html_nodes(".rounded-cornerform strong span") %>% 
    rvest::html_text()
  
  tibble::tibble(nombre = info[[1]], corporacion = info[[2]], year, formulario, href)
  
  
}


download_excel <- function(href) {
  
  temp <- tempfile()
  download.file(href, temp, quiet = TRUE)
  
  return(temp)
}


# Formulario 5.1B ---------------------------------------------------------

ingresos_familia <- function(x) {
  
  suppressMessages({
    data <- read_excel(x, skip = 11) 
    candidato <- read_excel(x, range = "F10", col_names = "") %>% pull()
  })
  
  output <- data %>% 
    rename(de = matches("Nombre de"), de_id = matches("Cédula"), valor = Valor, parentesco = Parentesco) %>% 
    mutate(para = candidato, formulario = "5.1B") %>% 
    select(de, para, valor, formulario, parentesco, de_id) %>% 
    drop_na(de) %>%
    filter(de != "TOTAL")
  
  if (is.logical(output$de_id)) {
    output$de_id <- as.character(output$de_id)
  }
  
  if (nrow(output) == 0) {
    message(candidato, ": formulario 5.1B", " vacío!")
    output <- mutate_if(output, is.logical, as.character) %>% 
      mutate(valor = numeric())
  }
  
  return(output)
  
}


# Formulario 5.2B ---------------------------------------------------------

ingresos_particulares <- function(x) {
  
  suppressMessages({
    data <- read_excel(x, skip = 11) 
    candidato <- read_excel(x, range = "F10", col_names = "") %>% pull()
  })
  
  output <- data %>% 
    rename(de = matches("Nombre de"), valor = Valor, de_id = matches("Cédula")) %>% 
    mutate(para = candidato, parentesco = "otro", formulario = "5.2B") %>% 
    select(de, para, valor, formulario, parentesco, de_id) %>% 
    drop_na(de) %>%
    filter(de != "TOTAL")
  
  if (is.logical(output$de_id)) {
    output$de_id <- as.character(output$de_id)
  }
  
  if (nrow(output) == 0) {
    message(candidato, ": formulario 5.2B", " vacío!")
    output <- mutate_if(output, is.logical, as.character) %>% 
      mutate(valor = numeric())
  }
  return(output)
  
}

descargar <- function(datos_buscador) {
  
  if (any(class(datos_buscador) == "try-error")) stop("Los meta-datos no existen", call. = FALSE)
  
  out1 <- download_excel(datos_buscador$href[[2]]) %>% 
    ingresos_familia()
  
  out2 <- download_excel(datos_buscador$href[[3]]) %>% 
    ingresos_particulares()
  
  el <- bind_rows(out1, out2)
  
  el$year <- unique(datos_buscador$year)
  
  return(el) # edge list
  
}
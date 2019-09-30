library(tidyverse)
library(readxl)

# *******************************
# Formulario 5.B (General)
# *******************************

fb <- function(x) {
  data <- read_excel(x, skip = 17) 
  candidato <- read_excel(x, range = "N15", col_names = "") %>% pull()
  
  data %>% 
    rename(codigo = Código, concepto = Concepto, valor = Valor) %>% 
    mutate(formulario = "5B", candidato = candidato) %>% 
    select(candidato, codigo, concepto, valor, formulario) %>% 
    mutate(tipo = case_when(
      codigo %in% 101:106 ~ "ingresos",
      codigo %in% 201:211 ~ "gastos"
    )) %>% 
    drop_na() 
}


# *******************************
# Formulario 5.1B
# *******************************

f1b <- function(x) {
  data <- read_excel(x, skip = 11) 
  candidato <- read_excel(x, range = "F10", col_names = "") %>% pull()
  
  output <- data %>% 
    rename(de = `Nombre de la Persona Natural`, valor = Valor, parentesco = Parentesco) %>% 
    mutate(para = candidato, formulario = "5.1B") %>% 
    select(de, para, valor, formulario, parentesco) %>% 
    drop_na() %>%
    filter(de != "TOTAL")
  
  if (nrow(output) == 0) message(candidato, ": formulario 5.1B", " vacío!")
  return(output)
  
}

# *******************************
# Formulario 5.2B
# *******************************

f2b <- function(x) {
  data <- read_excel(x, skip = 11) 
  
  candidato <- read_excel(x, range = "F10", col_names = "") %>% pull()
  
  output <- data %>% 
    rename(de = `Nombre de la Persona Natural o Jurídica`, valor = Valor) %>% 
    mutate(para = candidato, parentesco = "otro", formulario = "5.2B") %>% 
    select(de, para, valor, formulario, parentesco) %>% 
    drop_na() %>%
    filter(de != "TOTAL")
  
  if (nrow(output) == 0) message(candidato, ": formulario 5.2B", " vacío!")
  return(output)
  
}


# *******************************
# Formulario 5.3B
# *******************************

f3b <- function(x) {
  data <- read_excel(x, skip = 11) 
  candidato <- read_excel(x, range = "E10", col_names = "") %>% pull()
  
  output <- data %>% 
    rename(de = `Nombre de la Persona Jurídica`, valor = Valor) %>% 
    mutate(para = candidato, formulario = "5.1B", parentesco = "ninguno") %>% 
    select(de, para, valor, formulario, parentesco) %>% 
    drop_na() %>%
    filter(de != "TOTAL")
  
  if (nrow(output) == 0) message(candidato, ": formulario 5.3B", " vacío!")
  return(output)
  
}

# *******************************
# Información General
# *******************************

archivos_de_excel <- list.files("datos/total", full.names = TRUE)
info_general <- map(archivos_de_excel, fb) %>% bind_rows()

# *******************************
# Red de aportantes
# *******************************

info_1B <- list.files("datos/1", full.names = TRUE) %>% 
  map(f1b) %>% 
  bind_rows()

info_2B <- list.files("datos/2", full.names = TRUE) %>% 
  map(f2b) %>% 
  bind_rows()

info_3B <- list.files("datos/3", full.names = TRUE) %>% 
  map(f3b) %>% 
  bind_rows()

red <- bind_rows(info_1B, info_2B)

write_csv(red, "red_de_aportantes.csv")

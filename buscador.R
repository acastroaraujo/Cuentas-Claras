
# Borrador, funciones + análisis de algunos candidatos


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

ingresos_familia <- function(x) {
  
  suppressMessages({
    data <- read_excel(x, skip = 11) 
    candidato <- read_excel(x, range = "F10", col_names = "") %>% pull()
  })
  
  output <- data %>% 
    rename(de = `Nombre de la Persona Natural`, valor = Valor, parentesco = Parentesco) %>% 
    mutate(para = candidato, formulario = "5.1B") %>% 
    select(de, para, valor, formulario, parentesco) %>% 
    drop_na() %>%
    filter(de != "TOTAL")
  
  if (nrow(output) == 0) {
    message(candidato, ": formulario 5.1B", " vacío!")
    output <- mutate_if(output, is.logical, as.character) %>% 
      mutate(valor = numeric())
  }
  
  return(output)
  
}


ingresos_particulares <- function(x) {
  
  suppressMessages({
    data <- read_excel(x, skip = 11) 
    candidato <- read_excel(x, range = "F10", col_names = "") %>% pull()
  })
  
  output <- data %>% 
    rename(de = `Nombre de la Persona Natural o Jurídica`, valor = Valor) %>% 
    mutate(para = candidato, parentesco = "otro", formulario = "5.2B") %>% 
    select(de, para, valor, formulario, parentesco) %>% 
    drop_na() %>%
    filter(de != "TOTAL")
  
  if (nrow(output) == 0) {
    message(candidato, ": formulario 5.2B", " vacío!")
    output <- mutate_if(output, is.logical, as.character) %>% 
      mutate(valor = numeric())
  }
  return(output)
  
}


miguel <- cc_buscador_territorial(5360, "2019")

df_miguel <- bind_rows(
  ingresos_familia(download_excel(miguel$href[[2]])),
  ingresos_particulares(download_excel(miguel$href[[3]]))
) %>% 
  group_by(de, para, formulario, parentesco) %>% 
  summarize(valor = sum(valor))


claudia <- cc_buscador_territorial(5359, "2019")

df_claudia <- bind_rows(
  ingresos_familia(download_excel(claudia$href[[2]])),
  ingresos_particulares(download_excel(claudia$href[[3]]))
) %>% 
  group_by(de, para, formulario, parentesco) %>% 
  summarize(valor = sum(valor))

carlos <- cc_buscador_territorial(5358, "2019")

df_carlos <- bind_rows(
  ingresos_familia(download_excel(carlos$href[[2]])),
  ingresos_particulares(download_excel(carlos$href[[3]]))
) %>% 
  group_by(de, para, formulario, parentesco) %>% 
  summarize(valor = sum(valor))

hollman <- cc_buscador_territorial(5361, "2019")

df_hollman <- bind_rows(
  ingresos_familia(download_excel(hollman$href[[2]])),
  ingresos_particulares(download_excel(hollman$href[[3]]))
) %>% 
  group_by(de, para, formulario, parentesco) %>% 
  summarize(valor = sum(valor))

angelica <- cc_buscador_legislativo(2070, "2018")

df_angelica <- bind_rows(
  ingresos_familia(download_excel(angelica$href[[2]])),
  ingresos_particulares(download_excel(angelica$href[[3]]))
) %>% 
  group_by(de, para, formulario, parentesco) %>% 
  summarize(valor = sum(valor))

juanita <- cc_buscador_legislativo(374, "2018")

df_juanita <- bind_rows(
  ingresos_familia(download_excel(juanita$href[[2]])),
  ingresos_particulares(download_excel(juanita$href[[3]]))
) %>% 
  group_by(de, para, formulario, parentesco) %>% 
  summarize(valor = sum(valor)) 

df_carlos %>% ungroup() %>% 
  top_n(10, valor) %>% 
  arrange(desc(valor))


library(igraph)
library(tidygraph)
library(ggraph)

edge_list <- bind_rows(
  df_angelica,
  df_carlos,
  df_claudia,
  df_juanita,
  df_miguel
) %>% 
  mutate(de = str_replace(de, "VALOREM.+", "VALOREM")) %>% 
  mutate(de = stringi::stri_trans_general(de, "Latin-ASCII"),
         para = stringi::stri_trans_general(para, "Latin-ASCII")) %>% 
  group_by(de, para) %>% 
  summarize(valor = sum(valor))

money <- edge_list %>% 
  graph_from_data_frame() %>% 
  tidygraph::as_tbl_graph() 

v <- bind_rows(
  df_angelica,
  df_carlos,
  df_claudia,
  df_juanita,
  df_miguel
) %>% 
  mutate(de = str_replace(de, "VALOREM.+", "VALOREM")) %>% 
  group_by(de, para) %>% 
  summarize(valor = sum(valor)) %>% 
  group_by(de) %>% 
  summarize(valor = sum(valor), n = n()) %>%
  rename(name = de) %>% 
  mutate(label_name = ifelse(
    test =  n > 1 | name %in% unique(edge_list$para),
    yes = name,
    no = NA_character_
  )) %>% 
  mutate(candidato = ifelse(name %in% unique(edge_list$para), TRUE, FALSE))


money %>% 
  left_join(v) %>% 
  ggraph("fr") + 
  geom_node_point(aes(size = valor), show.legend = FALSE) +
  geom_edge_fan(aes(width = valor), alpha = 1/2, show.legend = FALSE) + 
  geom_node_label(aes(label = str_wrap(label_name, 10), fill = candidato), 
                  size = 2, repel = TRUE, alpha = 3/4,
                  family = "Avenir Next Condensed",
                  show.legend = FALSE) + 
  geom_node_point(aes(filter = candidato), color = "#00bfc4", size = 3)
  

edge_list %>% 
  mutate(edge = paste(de, "→", para)) %>% 
  ungroup() %>% 
  top_n(20, valor) %>% 
  mutate(de = fct_reorder(de, valor, sum)) %>% 
  ggplot(aes(valor, de, fill = para)) + 
  geom_col() +
  labs(y = NULL, x = NULL) +
  acathemes::theme_custom() +
  scale_x_continuous(labels = scales::dollar_format(1)) + 
  scale_fill_brewer(type = "qual") + 
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))



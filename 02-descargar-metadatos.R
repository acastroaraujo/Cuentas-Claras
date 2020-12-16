
source("01-scraper.R")

# Meta Datos 

retrieve_n <- function(outfolder, y) {
  
  files <- dir(outfolder)
  
  index <- which(str_detect(files, paste0("^", y)))

  out <- files[index]
  
  if (is_empty(out)) return(1)
  
  out <- out %>% 
    str_replace("\\d{4}-reporte-(\\d+)\\.rds", "\\1") %>% 
    as.numeric() %>% 
    max()
  
  return(out + 1)
  
}


# Legislativo -------------------------------------------------------------

outfolder <- "metadatos-legislativo/"
y <- "2014"

if (!dir.exists(outfolder)) dir.create(outfolder)

if (!is_empty(dir(outfolder))) {
  
  n <- retrieve_n(outfolder, y)
  
} else {
  
  n <- 1
  
}

errores <- 0

repeat {
  
  out <- try(cc_buscador_legislativo(n, y))
  filename <- str_glue("{outfolder}{y}-reporte-{n}.rds")
  write_rds(out, filename, compress = "gz")
  n <- n + 1
  
  errores <- if (any(class(out) == "try-error")) errores + 1 else 0
  if (errores >= 5) break
  
  Sys.sleep(runif(1, 1, 3))     ## be kind
  
}

# Debug

error_index <- dir(outfolder, full.names = TRUE) %>% 
  map(read_rds) %>% 
  map_lgl(~ any(class(.x) == "try-error")) %>% 
  which()

length(error_index)


# Territorial -------------------------------------------------------------

outfolder <- "metadatos-territorial/"
y <- "2019"

if (!dir.exists(outfolder)) dir.create(outfolder)

if (!is_empty(dir(outfolder))) {
 
  n <- retrieve_n(outfolder, y)
  
} else {
  
  n <- 1
  
}

errores <- 0

repeat {
  
  out <- try(cc_buscador_territorial(n, y))
  filename <- str_glue("{outfolder}{y}-reporte-{n}.rds")
  write_rds(out, filename, compress = "gz")
  n <- n + 1
  
  errores <- if (any(class(out) == "try-error")) errores + 1 else 0
  if (errores >= 10) break
  
  Sys.sleep(runif(1, 1, 3))     ## be kind
  
}

# Debug

error_index <- dir(outfolder, full.names = TRUE) %>% 
  map(read_rds) %>% 
  map_lgl(~ any(class(.x) == "try-error")) %>% 
  which()

length(error_index)


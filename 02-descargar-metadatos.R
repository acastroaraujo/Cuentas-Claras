
source("01-scraper.R")

# Meta Datos --------------------------------------------------------------

outfolder <- "metadatos-territorial/"

if (!dir.exists(outfolder)) dir.create(outfolder)
n <- 1

if (!is_empty(dir(outfolder))) {
 
  n <- str_replace(dir(outfolder), "\\d{4}-reporte-(\\d+)\\.rds", "\\1") %>% 
    as.numeric() %>% 
    max()
  
  n <- n + 1
}

repeat {
  
  out <- try(cc_buscador_territorial(n, "2019"))
  filename <- str_glue("{outfolder}2019-reporte-{n}.rds")
  write_rds(out, filename, compress = "gz")
  n <- n + 1
  
  errores <- if (any(class(out) == "try-error")) errores + 1 else 0
  if (errores >= 3) break
  
  Sys.sleep(runif(1, 1, 3))     ## be kind
  
}

error_index <- dir(outfolder, full.names = TRUE) %>% 
  map(read_rds) %>% 
  map_lgl(~ any(class(.x) == "try-error")) %>% 
  which()

length(error_index)
  
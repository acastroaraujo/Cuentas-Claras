
source("01-scraper.R")
library(progress)

infolder <- "metadatos-territorial/"
outfolder <- "cuentas-territorial/"

# infolder <- "metadatos-legislativo/"
# outfolder <- "cuentas-legislativo/"

if (!dir.exists(outfolder)) dir.create(outfolder)

cuentas_done <- str_replace(dir(outfolder), ".rds", "")
cuentas_left <- setdiff(str_replace(dir(infolder), ".rds", ""), cuentas_done)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(cuentas_left))

while(length(cuentas_left > 0)) {
  
  x <- sample(cuentas_left, 1)
  
  meta <- read_rds(str_glue("{infolder}{x}.rds"))
  out <- try(descargar(meta))
  write_rds(out, str_glue("{outfolder}{x}.rds"))
  cuentas_left <- cuentas_left[-which(cuentas_left == x)] ## int. subset
  
  pb$tick()
  Sys.sleep(runif(1, 0.1, 2))     ## be kind
  
}





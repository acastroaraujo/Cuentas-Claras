library(tidyverse)
library(tabulizer)

candidatos <- list.files("datos")
info <- list.files(paste0("datos/", candidatos[[1]]))

i <- str_detect(info, "Anexo_5.1B")

f <- file(paste0("datos/", candidatos, "/", info[i]))
close(f)


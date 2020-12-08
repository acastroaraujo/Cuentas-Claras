
library(tidyverse)

out1 <- "cuentas-territorial/"
out2 <- "cuentas-legislativo/"

error_index_1 <- dir(out1, full.names = TRUE) %>% 
  map(read_rds) %>% 
  map_lgl(~ any(class(.x) == "try-error")) %>% 
  which()

error_index_2 <- dir(out2, full.names = TRUE) %>% 
  map(read_rds) %>% 
  map_lgl(~ any(class(.x) == "try-error")) %>% 
  which()

df_territorial <- dir(out1, full.names = TRUE) %>% 
  map(read_rds) %>% 
  `[`(-error_index_1)

df_legislativo <- dir(out2, full.names = TRUE) %>% 
  map(read_rds) %>% 
  `[`(-error_index_2)

df <- bind_rows(
  bind_rows(df_territorial),
  bind_rows(df_legislativo)
) 

df %>% 
  rename(source = de, target = para) %>% 
  write_csv(str_glue("datos-{Sys.Date()}.csv"))

# index <- df %>% 
#   map_lgl(~ !is.character(.x$valor))
# 
# sum(!index)


# Merge -------------------------------------------------------------------

# Some names are spelled similarily, we can quantify how similar using "edit distance"

bind_rows(df_territorial[index]) %>% 
  filter(de != para) %>% 
  mutate(edit_distance = map2_dbl(de, para, ~ adist(.x, .y)[[1]]))

mat <- adist(c(bind_rows(df_territorial[index])$de, bind_rows(df_territorial[index])$para))
colnames(mat) <- rownames(mat) <- c(bind_rows(df_territorial[index])$de, bind_rows(df_territorial[index])$para)


name_df <- as_tibble(which(lower.tri(mat, diag = FALSE), arr.ind = TRUE)) %>% 
  mutate(
    row_name = colnames(mat)[.data$row], 
    col_name = colnames(mat)[.data$col],
    dist = mat[lower.tri(mat, diag = FALSE)]
    )

name_df %>% arrange(dist) 

# Having this, we can probably go back and change the names using the indices or something




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
  rename(source = de, target = para, source_id = de_id) %>% 
  filter(source != "0") %>% 
  write_csv(str_glue("datos-{Sys.Date()}.csv"))



df %>% 
  rename(source = de, target = para, source_id = de_id) %>% 
  filter(source != "0", year != 2014) %>% 
  count(source, sort = TRUE) %>% View

# out <- dir(out2, full.names = TRUE) %>% 
#   map(read_rds) 
# 
# new_index <- out[-error_index_2] %>% 
#   map_lgl(~ is.character(.x$valor))
# 
# 
# dir(out2, full.names = TRUE)[-error_index_2][new_index] %>% file.remove()
# df_legislativo[!index][[1]] 


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








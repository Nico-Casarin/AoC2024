library(tidyverse)
library(stringr)

multiply_elements <- function(x) {
  split_vals <- strsplit(x, ",")[[1]]
  as.numeric(split_vals[1]) * as.numeric(split_vals[2])
}

df <- readLines('files/03.txt')

sum <- 0

df <- str_extract_all(df, "mul\\(\\d+(,\\d+)*\\)")

for (i in seq_along(df_clean)) {
  for (e in seq_along(df_clean)) {
    df_clean[[e]] <- gsub("mul\\(", "", df_clean[[e]])
    df_clean[[e]] <- gsub("\\)", "", df_clean[[e]])
  }
  mult_vector <- sapply(df_clean[[i]], function(x) {
    multiply_elements(x)
  })
  sum <- sum(mult_vector)+sum
}
print(sum)


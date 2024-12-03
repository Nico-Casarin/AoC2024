library(tidyverse)
library(stringr)

multiply_elements <- function(x) {
  split_vals <- strsplit(x, ",")[[1]]
  as.numeric(split_vals[1]) * as.numeric(split_vals[2])
}

df <- readLines('files/03.txt')


df_clean <- str_extract_all(df, "mul\\(\\d+(,\\d+)*\\)")

for (e in seq_along(abc)) {
  abc[[e]] <- gsub("mul\\(", "", abc[[e]])
  abc[[e]] <- gsub("\\)", "", abc[[e]])
}
test <- sapply(abc[[1]], function(x) {
  multiply_elements(x)
})

sum(test)

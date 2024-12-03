library(tidyverse)
library(stringr)

multiply_elements <- function(x) {
  split_vals <- strsplit(x, ",")[[1]]
  as.numeric(split_vals[1]) * as.numeric(split_vals[2])
}

df <- readLines('files/03.txt')

sum <- 0

df_clean <- str_extract_all(df, "mul\\(\\d+(,\\d+)*\\)")

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

df_clean <- str_extract_all(df, "mul\\(\\d+(,\\d+)*\\)|do\\(\\)|don't\\(\\)")

sum <- 0

do <- T

for (i in seq_along(df_clean)) {
  data <- df_clean[[i]]
  print(data)
  for (e in seq_along(data)) {
    if (data[e] == "don't()") {
      data[e] <- "0,0"
      do <- F
    } else if (data[e] == "do()"){
      data[e] <- "0,0"
      do <- T
    }
    if (do == T) {
      data[e] <- gsub("mul\\(", "", data[e])
      data[e] <- gsub("\\)", "", data[e])
    } else if (do == F) {
      data[e] <- "0,0"
    }
  }
  df_clean[[i]] <- data
}

for (i in seq_along(df_clean)) {
  mult_vector <- sapply(df_clean[[i]], function(x) {
    multiply_elements(x)
  })
  sum <- sum(mult_vector)+sum
}
sum
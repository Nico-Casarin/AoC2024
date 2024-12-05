library(tidyverse)
library(stringr)

rules <- read_file("rules.txt")
rules <- as.list(strsplit(rules, "\r\n")[[1]])
rules <- lapply(rules, function(x) strsplit(x, split="\\|"))
rules <- lapply(rules, function(x) unlist(x))


input <- read_file("input05.txt")
input <- unlist(strsplit(input, "\n"))

split_input <- lapply(input, function(x) strsplit(x, ',')[[1]])

input <- lapply(split_input, as.numeric)

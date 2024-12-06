grid <- readLines("files/04.txt")

grid_matrix <- do.call(rbind, strsplit(grid, ""))
grid_matrix

count_word_in_vector <- function(vec, word) {
  count <- 0
  n <- nchar(word)
  if (length(vec) >= n) {
    for (i in 1:(length(vec) - n + 1)) {
      if (paste(vec[i:(i + n - 1)], collapse = "") == word) {
        count <- count + 1
      }
      if (paste(rev(vec[i:(i + n - 1)]), collapse = "") == word) {
        count <- count + 1
      }
    }
  }
  return(count)
}

n <- nchar('xmas')


extract_diagonals <- function(mat) {
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  diagonals <- list()
  
  for (offset in -(nrow - 1):(ncol - 1)) {
    diag_values <- mat[row(mat) - col(mat) == offset]
    if (length(diag_values) > 0) {
      diagonals <- c(diagonals, list(diag_values))
    }
  }
  
  for (offset in -(nrow - 1):(ncol - 1)) {
    diag_values <- mat[row(mat) + col(mat) == offset + ncol]
    if (length(diag_values) > 0) {
      diagonals <- c(diagonals, list(diag_values))
    }
  }
  
  return(diagonals)
}

word <- "XMAS"

horizontal_count <- sum(apply(grid_matrix, 1, count_word_in_vector, word))

vertical_count <- sum(apply(grid_matrix, 2, count_word_in_vector, word))

diagonals <- extract_diagonals(grid_matrix)
diagonal_count <- sum(sapply(diagonals, count_word_in_vector, word))

total_count <- horizontal_count + vertical_count + diagonal_count

total_count


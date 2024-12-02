library(tidyverse)

##First Star

df <- as.tibble(readLines("02.txt")) %>%  
  separate_wider_delim("value", delim = " ",
                       names = c("x1", "x2", 'x3', 'x4', 'x5', 'x6', 'x7','x8'),
                       too_few = "debug",too_many = c("debug")) %>% 
  mutate(across(x1:x8, as.integer)) %>%
  rowwise() %>%
  mutate(sum_diff_list = list(map2_int(c_across(1:7), 
                             c_across(2:8), 
                             ~ if_else((0 < abs(.x - .y) & abs(.x - .y) <= 3),1,0) %>% sum(na.rm = T))),
         no_changes_list = list(map2_int(c_across(1:7), 
                                   c_across(2:8), 
                                   ~ if_else(.x < .y,1,0) %>% sum(na.rm = T))),
         sum_diff = sum(sum_diff_list),
         all_same = length(unique(no_changes_list[1:value_pieces-1])),
         ok = ifelse((sum_diff==(value_pieces-1) & all_same ==1),1,0)) %>% 
  ungroup() 

sum(df$ok)


##### Better solution:

  
is_safe <- function(levels) {
  diffs <- diff(levels)
  all(diffs >= -3 & diffs <= -1) || all(diffs >= 1 & diffs <= 3)
}

is_safe_with_dampener <- function(levels) {
  if (is_safe(levels)) {
    return(TRUE)
  }
  for (i in seq_along(levels)) {
    if (is_safe(levels[-i])) {
      return(TRUE)
    }
  }
  return(FALSE)
}

data <- readLines("C:\\Users\\casarni002\\Downloads\\02.txt") %>%
  strsplit(" ") %>%
  map(as.numeric)

safe_reports <- sum(map_lgl(data, is_safe))

print(safe_reports)

safe_reports <- sum(map_lgl(data, is_safe_with_dampener))

print(safe_reports)


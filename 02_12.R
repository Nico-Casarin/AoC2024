library(tidyverse)

##First Star

df <- as.tibble(readLines("C:\\Users\\casarni002\\Downloads\\02.txt")) %>%  
  separate_wider_delim("value", delim = " ",
                       names = c("x1", "x2", 'x3', 'x4', 'x5', 'x6', 'x7','x8'),
                       too_few = "debug",too_many = c("debug")) %>% 
  mutate(across(x1:x8, as.integer)) %>%
  rowwise() %>%
  mutate(sum_diff = list(map2_int(c_across(1:7), 
                             c_across(2:8), 
                             ~ if_else((0 < abs(.x - .y) & abs(.x - .y) <= 3),1,0) %>% sum(na.rm = T))),
         sum_diff2 = list(map2_int(c_across(1:7), 
                                   c_across(2:8), 
                                   ~ if_else(.x < .y,1,0) %>% sum(na.rm = T))),
         sum_diff = sum(sum_diff),
         all_same = length(unique(sum_diff2[1:value_pieces-1])),
         ok = ifelse((sum_diff==(value_pieces-1) & all_same ==1),1,0)) %>% 
  ungroup()
sum(df$ok)

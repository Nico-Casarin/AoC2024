library(tidyverse)

df <- read_table(
  'files/01.txt',
  col_names = F
) %>% 
  mutate(X1 = sort(X1),
         X2 = sort(X2),
         X3 = abs(X1-X2)) %>% 
  summarise(sum = sum(X3))

df <- read_table(
  'files/01.txt',
  col_names = F
) %>% 
  mutate(X1 = sort(X1),
         X2 = sort(X2))
left <- df %>% select(X1)
rigt <- df %>% select(X2)

left %>% 
  group_by(X1) %>% 
  summarise(n = sum(X1 == rigt$X2), .groups = "drop") %>% 
  mutate(simi = X1*n) %>% 
  summarise(n = sum(simi))

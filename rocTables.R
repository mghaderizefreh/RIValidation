rm(list = ls())
source('allfunctions.R')
library(tidyverse)
#### known ####
newgrp <- c(1:3,9,NA,NA,5,NA,6,7)
newgrp2 <- c(6,4,8,7,2,3,1,9,5)
trs <- c("LV", "MS", "S", "AC1", "Int")
trsTxt <- c("LV","LMS","S","AC1","AUC")
read_table('results/known/pair.txt') %>% 
  mutate(newgroup = newgrp[grp]) %>% select(-method, -grp) %>% drop_na() %>% 
  select(-newgroup) %>% 
  pivot_wider(names_from = c(grp1,grp2), values_from = aurocc) %>% 
  mutate(full_partial=(resilient_steady+resilient_compensatory+resilient_relapsed)/3.,
         non_partial=(steady_unrecovered+compensatory_unrecovered+relapsed_unrecovered)/3.) %>% 
  pivot_longer(resilient_steady:non_partial, names_to = 'comparison', values_to = 'aurocc') %>% 
  arrange(rep,transformation,comparison) %>% 
  mutate(grp = rep(newgrp2, dim(.)[1]/9)) %>% 
  arrange(rep,transformation,grp) %>% 
  group_by(transformation, comparison, grp) %>% 
  summarise(avg = mean(aurocc), sd = sd(aurocc)) %>% ungroup() %>% 
  select(-comparison) %>% arrange(grp) %>% 
  mutate(val = sprintf("%3.2f (%6.1E)", avg, sd)) %>% select(-avg, -sd) %>% 
  pivot_wider(names_from = grp, values_from = val) %>% 
  mutate(transformation = case_match(transformation,'Int'~'AUC', "MS"~"LMS", .default = transformation)) %>% 
  mutate(transformation = factor(transformation, levels = trsTxt)) %>% 
  arrange(transformation) %>% mutate(method=NA)%>%select(method,everything())%>% 
  write.table('finalFigures/pair.csv', quote = F, append = F, sep = ',',
    col.names = c("method","transformation","resilient_steady",
    "resilient_compensatory","resilient_relapsed","full_partial",
    "steady_unrecovered","compensatory_unrecovered","relapsed_unrecovered",
    "non_partial","resilient_unrecovered"), row.names = F)
#### unknown ####
read_table('results/unknown/pair.txt') %>% 
  mutate(newgroup = newgrp[grp]) %>% select(-grp) %>% drop_na() %>% 
  select(-newgroup) %>% 
  pivot_wider(names_from = c(grp1,grp2), values_from = aurocc) %>% 
  mutate(full_partial=(resilient_steady+resilient_compensatory+resilient_relapsed)/3.,
         non_partial=(steady_unrecovered+compensatory_unrecovered+relapsed_unrecovered)/3.) %>% 
  pivot_longer(resilient_steady:non_partial, names_to = 'comparison', values_to = 'aurocc') %>% 
  arrange(method, rep,transformation,comparison) %>% 
  mutate(grp = rep(newgrp2, dim(.)[1]/9)) %>% 
  arrange(method, rep,transformation,grp) %>% 
  group_by(method, transformation, comparison, grp) %>% 
  summarise(avg = mean(aurocc), sd = sd(aurocc)) %>% ungroup() %>% 
  select(-comparison) %>% arrange(grp, transformation) %>% 
  mutate(val = sprintf("%3.2f (%6.1E)", avg, sd)) %>% select(-avg, -sd) %>% 
  pivot_wider(names_from = grp, values_from = val) %>% 
  mutate(transformation = case_match(transformation,'Int'~'AUC', "MS"~"LMS", .default = transformation)) %>% 
  mutate(transformation = factor(transformation, levels = trsTxt)) %>% 
  arrange(transformation) %>% 
  write.table('finalFigures/pair.csv', quote = F, append = T, sep = ',',
    col.names = F, row.names = F)

linearGrowth = F # data needs to be simulated. Not included in this repository
if (linearGrowth) {
#### linear-unknown ####
read_table('results/linear/pair.txt') %>% 
  mutate(newgroup = newgrp[grp]) %>% select(-grp) %>% drop_na() %>% 
  select(-newgroup) %>% 
  pivot_wider(names_from = c(grp1,grp2), values_from = aurocc) %>% 
  mutate(full_partial=(resilient_steady+resilient_compensatory+resilient_relapsed)/3.,
         non_partial=(steady_unrecovered+compensatory_unrecovered+relapsed_unrecovered)/3.) %>% 
  pivot_longer(resilient_steady:non_partial, names_to = 'comparison', values_to = 'aurocc') %>% 
  arrange(method, rep,transformation,comparison) %>% 
  mutate(grp = rep(newgrp2, dim(.)[1]/9)) %>% 
  arrange(method, rep,transformation,grp) %>% 
  group_by(method, transformation, comparison, grp) %>% 
  summarise(avg = mean(aurocc), sd = sd(aurocc)) %>% ungroup() %>% 
  select(-comparison) %>% arrange(grp, transformation) %>% 
  mutate(val = sprintf("%3.2f (%6.1E)", avg, sd)) %>% select(-avg, -sd) %>% 
  pivot_wider(names_from = grp, values_from = val) %>% 
  mutate(transformation = case_match(transformation,'Int'~'AUC', "MS"~"LMS", .default = transformation)) %>% 
  mutate(transformation = factor(transformation, levels = trsTxt)) %>% 
  arrange(transformation) %>% 
  write.table('finalFigures/pair_line.csv', quote = F, append = T, sep = ',',
              col.names = F, row.names = F)
}

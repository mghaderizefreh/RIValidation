# if needed
rm(list = ls())
{if (!require(tidyr)){install.packages('tidyr', dependencies = T); library(tidyr)}
if (!require(dplyr)){install.packages('dplyr', dependencies = T); library(dplyr)}
if (!require(quantreg)){install.packages('quantreg', dependencies = T); library(quantreg)}
if (!require(readr)){install.packages('readr', dependencies = T); library(readr)}
if (!require(purrr)){install.packages('purrr', dependencies = T); library(purrr)}
if (!require(ggplot2)){install.packages('ggplot2', dependencies = T); library(ggplot2)}
if (!require(ggpubr)){install.packages('ggpubr', dependencies = T); library(ggpubr)}
if (!dir.exists("finalFigures")){dir.create("finalFigures")}}
source('allfunctions.R')
#### Known ####
##### amplitude #####
df <- read_table('./results/known/amp.txt', show_col_types = F)
trs <- c("MS", "LV", "AC1", "S", "Int")
trsTxt <- c("LMS", "LV","AC1", "S","AUC")
lplot <- list()
for (j in 1:5){
  df %>% filter(trs[j]==transformation, LINE!="resilient") %>% 
    mutate(value=if_else(transformation=="MS",log(value),value))%>%
    mutate(LINE=if_else(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>% 
    Rmisc::summarySE('value', c('LINE', 'transformation', 'amp_increase')) %>% 
    rename(`Response Type` = LINE) %>% mutate(group=paste(`Response Type`)) %>% 
    ggplot(aes(x = amp_increase / 0.4 * 100, y = value, group = group))+
    geom_line(aes(linetype=`Response Type`)) + 
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = 1) +
    ylab(trsTxt[j])+ 
    scale_x_continuous(breaks=seq(0,25,by=10))+
    theme_pubr() + labs(title = LETTERS[j])-> lplot[[j]]
}
for (i in 1:5){lplot[[i]] <- lplot[[i]] + rremove('xlab')}
lg <- get_legend(lplot[[1]])
figs <- ggarrange(plotlist = lplot,nrow = 1, ncol = 5, legend.grob = lg)
annotate_figure(figs, bottom = 'Increase in amplitude (%)')
ggsave("finalFigures/S2_ampKnown.png", width = 8, height = 2.5, bg = "white")
##### recovery speed #####
df <- read_table('./results/known/recspeed.txt', show_col_types = F) 
trs <- c("MS", "LV", "AC1", "S", "Int")
trsTxt <- c("LMS", "LV","AC1", "S","AUC")
lplot <- list()
for (j in 1:5){
  df %>% filter(trs[j]==transformation, LINE!="resilient") %>% 
    mutate(value=if_else(transformation%in%c("MS"),log(value),value))%>%
    mutate(LINE=if_else(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>% 
    Rmisc::summarySE('value', c('LINE', 'transformation', 'rec_increase')) %>% 
    rename(`Response Type` = LINE) %>% mutate(group=paste(`Response Type`)) %>% 
    ggplot(aes(x = rec_increase / 90.0 * 100, y = value, group = group))+
    geom_line(aes(linetype = `Response Type`)) + geom_point(size = .3) +
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = 4) +
    scale_x_continuous(breaks=seq(0,70,by=30))+ylab(trsTxt[j])+ 
    theme_pubr() + labs(title = LETTERS[j])-> lplot[[j]]
}
for (i in c(1:5)){lplot[[i]] <- lplot[[i]] + rremove('xlab')}
lg <- get_legend(lplot[[1]])
figs <- ggarrange(plotlist = lplot,nrow = 1, ncol = 5, legend.grob = lg)
annotate_figure(figs, bottom = "Increase in duration of recovery (%)")
ggsave("finalFigures/S3_speedKnown.png", width = 8, height = 2.5, bg = "white")

##### onset #####
df <- read_table('./results/known/delay.txt', show_col_types = F)
lplot <- list()
trs <- c("MS", "LV", "AC1", "S", "Int")
trsTxt <- c("LMS", "LV","AC1", "S","AUC")
for (j in 1:5){
  df %>% filter(trs[j] == transformation, LINE!="resilient") %>% 
    mutate(value=if_else(transformation%in%c("MS"),log(value),value))%>%
    mutate(LINE=if_else(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>% 
    Rmisc::summarySE('value', c('LINE', 'transformation', 'Tdelay')) %>% 
    rename(`Response Type` = LINE) %>% mutate(group=paste(`Response Type`)) %>% 
    ggplot(aes(x = Tdelay, y = value, group = group))+
    geom_line(aes(linetype = `Response Type`)) + 
    geom_vline(xintercept = 0, color = 'black', linetype = 'dotted')+
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = 4) +
    scale_x_continuous(breaks=seq(-65,65,by=130), labels = c(70,200))+
    ylab(trsTxt[j])+ 
    scale_linetype_manual(values = c(1,2,3))+
    theme_pubr() + labs(title = LETTERS[j])  -> lplot[[j]]
}
lg <- get_legend(lplot[[1]])
for (i in 1:5){lplot[[i]] <- lplot[[i]] + rremove('x.title')}
figs <- ggpubr::ggarrange(plotlist = lplot,nrow = 1,ncol = 5, legend.grob = lg)
annotate_figure(figs, bottom = "Days-in-milk at start of perturbation")
ggsave("finalFigures/Figure2_onsetKnown.png", width = 8, height = 2.5, bg = "white")

##### frequency #####
df <- read_table('./results/known/freq.txt', show_col_types = F)
trs <- c("MS", "LV", "AC1", "S", "Int")
trsTxt <- c("LMS", "LV","AC1", "S","AUC")
lplot <- list()
for (j in 1:5){
  df %>% filter(transformation == trs[j])%>%
    mutate(value=if_else(transformation%in%c("MS"),log(value),value))%>%
    mutate(LINE=if_else(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>% 
    Rmisc::summarySE('value', c('LINE', 'transformation', 'freq')) %>% 
    rename(`Response Type` = LINE) %>% mutate(group=paste(`Response Type`)) %>% 
    ggplot(aes(x = freq, y = value, group =group))+
    geom_line(aes(linetype = `Response Type`)) + 
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = 1) +
    ylab(trsTxt[j])+ 
    scale_x_continuous(limits = c(0,30) ,breaks=c(1,15,30))+
    theme_pubr() + labs(title = LETTERS[j])-> lplot[[j]]
}
for (i in 1:5){lplot[[i]] <- lplot[[i]] + rremove('xlab')}
lg <- get_legend(lplot[[1]])
figs <- ggpubr::ggarrange(plotlist = lplot,nrow = 1, ncol = 5,legend.grob = lg)
annotate_figure(figs, bottom = 'Days between measurements' )
ggsave("finalFigures/Figure3_freqKnown.png", width = 8, height = 2.5, bg = "white")

##### observation ####
df<-read_table("results/known/obs.txt", show_col_types = F) %>%
  filter(LINE!="resilient") %>% 
  mutate(L=ifelse(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>%
  mutate(ratio = observation / pert_length) %>% 
  mutate(value=if_else(transformation=="MS",log(value),value))
trs <- c("MS", "LV", "AC1", "S", "Int")
trsTxt <- c("LMS", "LV","AC1", "S","AUC")
lplot <- list()
pallete <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

for (j in 1:5){
  df %>% filter(transformation == trs[j], pert_length == 90)%>%
    Rmisc::summarySE('value', c('L', 'ratio')) %>% 
    rename(`Response Type` = L) %>% mutate(group=paste(`Response Type`)) %>% 
    ggplot(aes(x = ratio, y = value, group =group))+
    geom_line(aes(linetype = `Response Type`)) + 
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = .2) +
    ylab(trsTxt[j])+ 
    scale_x_continuous(breaks=1:4)+
    theme_pubr() + labs(title = LETTERS[j])-> lplot[[j]]
  
  
}
for (i in 1:5){lplot[[i]] <- lplot[[i]] + rremove('xlab')}
lg <- get_legend(lplot[[1]])
figs <- ggpubr::ggarrange(plotlist = lplot,nrow = 1,ncol = 5, legend.grob = lg)
annotate_figure(figs, bottom = 'observation period / perturbation period' )
ggsave(paste0("finalFigures/Figure4_periodKnown.png"), width = 8, height = 2.5, bg = "white")


#### Unknown ####
##### amplitude ####
df <- read_table('./results/unknown/amp.txt', show_col_types = F) %>%
  mutate(method = as.character(method))
trs <- c("MS", "LV", "AC1", "S", "Int")
trsTxt <- c("LMS", "LV","AC1", "S","AUC")
lplot <- list()
for (j in 1:5){
  df %>% filter(trs[j]==transformation, LINE!='resilient') %>% 
    mutate(Method=factor(case_match(method,"1"~"QR","2"~"RR"), levels = c("QR","RR"))) %>% drop_na() %>% 
    mutate(value=if_else(transformation=="MS",log(value),value))%>%
    mutate(LINE=if_else(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>% #    mutate(LINE=case_match(LINE, "resilient"~"Full","unrecovered"~"Non", .default="Partial")) %>% 
    Rmisc::summarySE('value', c('LINE', 'transformation', 'amp_increase','Method')) %>% 
    mutate(`Response Type` = LINE) %>% mutate(group=paste(`Response Type`,Method)) %>% 
    ggplot(aes(x = amp_increase/ 0.4 * 100, y = value, group = group, color = Method))+
    geom_line(aes(linetype=`Response Type`)) + 
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = 1) +
    ylab(trsTxt[j])+ 
    scale_x_continuous(breaks=seq(0,25,by=10))+
    scale_color_manual(values = c('black','magenta'))+
    theme_pubr() + labs(title = LETTERS[j])-> lplot[[j]]
}
for (i in 1:5){lplot[[i]] <- lplot[[i]] + rremove('xlab')}
lg <- get_legend(lplot[[1]])
figs <- ggarrange(plotlist = lplot,nrow = 1, ncol = 5, legend.grob = lg)
annotate_figure(figs, bottom = 'Increase in perturbation amplitude')
ggsave("finalFigures/S4_ampUnknown.png", width = 8, height = 2.5, bg = "white")
##### recovery speed #####
df <- read_table('./results/unknown/recspeed.txt', show_col_types = F) %>% 
  mutate(method = as.character(method))
trs <- c("MS", "LV", "AC1", "S", "Int")
trsTxt <- c("LMS", "LV","AC1", "S","AUC")
for (j in 1:5){
  df %>% filter(trs[j]==transformation, LINE!='resilient') %>% 
    mutate(Method=factor(case_match(method,"1"~"QR","2"~"RR"), levels = c("QR","RR"))) %>% drop_na() %>% 
    mutate(value=if_else(transformation=="MS",log(value),value))%>%
    mutate(LINE=if_else(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>% #    mutate(LINE=case_match(LINE, "resilient"~"Full","unrecovered"~"Non", .default="Partial")) %>% 
    Rmisc::summarySE('value', c('LINE', 'transformation', 'rec_increase','Method')) %>% 
    mutate(`Response Type` = LINE) %>% mutate(group=paste(`Response Type`,Method)) %>% 
    ggplot(aes(x = rec_increase / 90.0 * 100, y = value, group = group, color = Method))+
    geom_line(aes(linetype=`Response Type`)) + 
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = 4) +
    ylab(trsTxt[j])+ 
    scale_x_continuous(breaks=seq(0,70,by=30))+
    scale_color_manual(values = c('black','magenta'))+
    theme_pubr() + labs(title = LETTERS[j])-> lplot[[j]]
}
for (i in 1:5){lplot[[i]] <- lplot[[i]] + rremove('xlab')}
lg <- get_legend(lplot[[1]])
figs <- ggarrange(plotlist = lplot,nrow = 1, ncol = 5, legend.grob = lg)
annotate_figure(figs, bottom = 'Increase in duration of recovery (%)')
ggsave("finalFigures/S5_speedUnknown.png", width = 8, height = 2.5, bg = "white")
##### onset #####
df <- read_table('./results/unknown/delay.txt', show_col_types = F) %>%
  mutate(method = as.character(method))
lplot <- list()
trs <- c("MS", "LV", "AC1", "Int")
trsTxt <- c("LMS", "LV","AC1", "AUC")
for (j in 1:4){
  df %>% filter(trs[j] == transformation, LINE!="resilient") %>% 
    mutate(Method=factor(case_match(method,"1"~"QR","2"~"RR"), levels = c("QR","RR"))) %>% drop_na() %>% 
    mutate(value=if_else(transformation=="MS",log(value),value))%>%
    mutate(LINE=if_else(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>%
    Rmisc::summarySE('value', c('LINE', 'transformation', 'Tdelay', 'Method')) %>% 
    mutate(`Response Type` = LINE) %>% mutate(group=paste(`Response Type`,Method)) %>% 
    ggplot(aes(x = Tdelay, y = value, group = group, color = Method, linetype = `Response Type`))+
    geom_line() + geom_vline(xintercept = 0, color = 'black', linetype = 'dashed')+
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = 4, ) +
    scale_x_continuous(breaks=seq(-65,65,by=130), labels = c(70,200))+
    scale_color_manual(values = c('black','magenta'))+
    ylab(trsTxt[j])+ 
    theme_pubr() + labs(title = LETTERS[j])  -> lplot[[j]]
}
lg <- get_legend(lplot[[1]])
for (i in 1:4){lplot[[i]] <- lplot[[i]] + rremove('x.title')}
figs <- ggpubr::ggarrange(plotlist = lplot,nrow = 1,ncol = 4, legend.grob = lg)
annotate_figure(figs, bottom = "Age/Days-in-milk at start of perturbation")
ggsave("finalFigures/Figure5_onsetUnknown.png", width = 8, height = 3, bg = "white")
##### frequency #####
df <- read_table('./results/unknown/freq.txt', show_col_types = F) %>%
  mutate(method = as.character(method))
trs <- c("MS", "LV", "AC1", "Int")
trsTxt <- c("LMS", "LV","AC1","AUC")
lplot <- list()
for (j in 1:4){
  df %>% filter(transformation == trs[j], LINE!="resilient") %>% 
    mutate(Method=factor(case_match(method,"1"~"QR","2"~"RR"), levels = c("QR","RR"))) %>% drop_na() %>% 
    mutate(value=if_else(transformation=="MS",log(value),value))%>%
    mutate(LINE=if_else(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>%
    Rmisc::summarySE('value', c('LINE', 'transformation', 'freq','Method')) %>% 
    rename(`Response Type` = LINE) %>% mutate(group=paste(`Response Type`,Method)) %>% 
    ggplot(aes(x = freq, y = value,group =group,color=Method))+
    geom_line(aes(linetype = `Response Type`)) + ylab(trsTxt[j])+ 
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = 1) +
    scale_x_continuous(limits = c(0,30) ,breaks=c(1,15,30))+
    scale_color_manual(values = c('black','magenta'))+
    theme_pubr() + labs(title = LETTERS[j])-> lplot[[j]]
}
for (i in 1:4){lplot[[i]] <- lplot[[i]] + rremove('xlab')}
lg <- get_legend(lplot[[1]])
figs <- ggpubr::ggarrange(plotlist = lplot,nrow = 1, ncol = 4,legend.grob = lg)
annotate_figure(figs, bottom = 'Days between measurements' )
ggsave("finalFigures/S6_freqUnknown.png", width = 8, height = 3, bg = "white")
##### observation #####
df <- read_table('./results/unknown/obs.txt', show_col_types = F) %>%
  filter(LINE!="resilient") %>% 
  mutate(method = as.character(method)) %>% 
  mutate(L=ifelse(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>%
  mutate(ratio = observation / pert_length) %>% 
  mutate(value=if_else(transformation=="MS",log(value),value)) %>% 
  mutate(Method=factor(case_match(method,"1"~"QR","2"~"RR"), levels = c("QR","RR"))) %>% 
  select(-method) %>% drop_na()

trs <- c("MS", "LV", "AC1", "Int")
trsTxt <- c("LMS", "LV","AC1","AUC")
ms <- c("QR","RR","AF")
lplot <- list()
#pallete <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
#             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
for (m in 1:2){
for (j in 1:4){
  df %>% filter(transformation == trs[j], Method == ms[m]) %>% filter(pert_length == 90) %>% 
    Rmisc::summarySE('value', c('L', 'ratio')) %>% 
    rename(`Response Type` = L) %>% mutate(group=paste(`Response Type`)) %>% 
    ggplot(aes(x = ratio, y = value, group = group))+
    geom_line(aes(linetype = `Response Type`)) + 
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = .2) +
    ylab(trsTxt[j])+ 
    scale_x_continuous(breaks=1:4)+
    scale_color_manual(values = c('black','magenta'))+
    theme_pubr() + labs(title = LETTERS[j+m*4-4])-> lplot[[j+m*4-4]]
}}
for (i in 1:8){lplot[[i]] <- lplot[[i]] + rremove('xlab')}
for (i in 1:4){lim <- layer_scales(lplot[[i]])$y$range$range %>% 
  c(layer_scales(lplot[[i+4]])$y$range$range) %>% range()
  lplot[[i]] <- lplot[[i]] + scale_y_continuous(limits = lim)
  lplot[[i+4]] <- lplot[[i+4]] + scale_y_continuous(limits = lim)
}
lg <- get_legend(lplot[[1]])
figs <- ggpubr::ggarrange(plotlist = lplot,nrow = 2, ncol = 4,legend.grob = lg)
annotate_figure(figs, bottom = 'observation period / perturbation period' )
ggsave("finalFigures/Figure6_periodUnknown.png", width = 8, height = 5, bg = "white")

#### Values ####
oldw <- getOption("warn")
options(warn = -1)

# cv of Ri when onset is changing
cat("cv of Ri when onset is changing (method 0 is known)\n")
read_table('./results/known/delay.txt', show_col_types = F) %>% mutate(method = 0) %>%
  bind_rows(read_table('./results/unknown/delay.txt', show_col_types = F)) %>%
  filter(LINE!="resilient", method!= "3", transformation != 'S') %>% 
  mutate(value=if_else(transformation=="MS",log(value),value))%>%
  mutate(LINE=if_else(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>% 
  group_by(transformation, method, Tdelay , rep, LINE) %>% 
  summarise(value = mean(value), .groups = "keep") %>% ungroup() %>% 
  Rmisc::summarySE('value', c('transformation', 'method', 'Tdelay', 'LINE')) %>% 
  as_tibble() %>% 
  Rmisc::summarySE('value', c('transformation', 'method', 'LINE')) %>% 
  arrange(LINE, transformation, method) %>% 
  transmute(transformation, method, LINE, value, cv = sd /value) %>% print()

# changes of value for freequncy
cat("changes of value for freequncy(method 0 is known)\n")
read_table('./results/known/freq.txt', show_col_types = F) %>% mutate(method = 0) %>%
  bind_rows(read_table('./results/unknown/freq.txt', show_col_types = F)) %>% 
  filter("resilient"!=LINE, method != "3",transformation != 'S') %>%
  mutate(value=if_else(transformation=="MS",log(value),value))%>%
  mutate(LINE=if_else(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>% 
  group_by(transformation, method, freq , rep, LINE) %>% 
  summarise(value = mean(value), .groups = "keep") %>% ungroup() %>% 
  Rmisc::summarySE('value', c('transformation', 'method', 'freq')) %>% 
  as_tibble() %>% filter("AC1"!=transformation) %>% 
  group_by(transformation, method)%>% 
  summarise((last(value) - first(value)) / first(value) * 100) %>% 
  arrange(method, transformation) %>% print(n = Inf)

# extrapolation for ac1
  {cat("extrapolation for ac1 (method 0 is known)\n")
read_table('./results/known/obs.txt', show_col_types = F) %>% mutate(method = 0) %>%
  bind_rows(read_table('./results/unknown/obs.txt', show_col_types = F)) %>%
  mutate(ratio = observation / pert_length) %>%
  filter(LINE!="resilient") %>%
  mutate(LINE=ifelse(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>%
  filter(transformation == "AC1", method != "3") %>% 
  group_by(method, ratio , rep, LINE) %>% 
  summarise(value = mean(value), .groups = "keep") %>% ungroup() %>% 
  pivot_wider(names_from = LINE, values_from = value) %>% 
  mutate(dif = `Non-Resilient` - `Partially Resilient`) %>% 
  select(method, ratio, dif) -> temp
for (j in 0:2){ temp %>% filter(method == j) %>% select(-method) %>% 
  {approx(.$dif, .$ratio, xout = 0.0)} %>% .$y %>% 
  paste(.,"perturbation period for method", j, '\n') %>% cat()}}

# values for observation period
  ## known
  cat("values for observation period (known)\n")
  trsTxt <- c("LMS", "LV","AC1", "S","AUC")
  trs <- c("MS", "LV", "AC1", "S", "Int")
  read_table('./results/known/obs.txt', show_col_types = F) %>%
    filter(LINE!="resilient") %>%
    mutate(L=ifelse(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>%
    mutate(ratio = observation / pert_length) %>%
    mutate(value=if_else(transformation=="MS",log(value),value)) %>%
    filter(transformation%in%trs[c(2,3)], pert_length == 90) %>% 
    select(value, L, ratio,transformation) %>% 
    group_by(L, ratio, transformation) %>% 
    summarise(value = mean(value),.groups = 'drop') %>% 
    pivot_wider(names_from = L, values_from = value) %>% 
    transmute(ratio,transformation,di=`Partially Resilient`-`Non-Resilient`) %>% 
    group_nest(transformation) %>% 
    mutate(y = map(data, \(d)(approx(d$di,d$ratio,0)$y))) %>% 
    select(-data) %>% unnest(c(transformation, y)) %>% print()
  ## unknown
  cat("values for observation period (unknown)\n")
  trsTxt <- c("LMS", "LV","AC1","AUC")
  trs <- c("MS", "LV", "AC1", "Int")
  read_table('./results/unknown/obs.txt', show_col_types = F) %>%
    filter(LINE!="resilient") %>%
    mutate(method = as.character(method)) %>%
    mutate(L=ifelse(LINE!="unrecovered","Partially Resilient","Non-Resilient")) %>%
    mutate(ratio = observation / pert_length) %>%
    mutate(value=if_else(transformation=="MS",log(value),value)) %>%
    mutate(Method=factor(case_match(method,"1"~"QR","2"~"RR"), levels = c("QR","RR")))%>%
    select(-method) %>% drop_na() %>%
    filter(transformation != 'S', pert_length == 90) %>% 
    select(value, L, Method, ratio, transformation) %>% 
    group_by(L, transformation, Method, ratio) %>% 
    summarise(value = mean(value),.groups = 'drop') %>% 
    pivot_wider(names_from = L, values_from = value) %>% 
    mutate(dif = `Partially Resilient` - `Non-Resilient`) %>%
    select(-contains('Resilient')) %>% 
    group_nest(transformation, Method) %>%
    mutate(y = map(data, \(d) (Hmisc::approxExtrap(d$dif, d$ratio, xout = 0)$y))) %>% 
    select(-data) %>% unnest(c(transformation, Method, y)) %>% 
    pivot_wider(names_from = Method, values_from = y) %>% print() 
options(warn = oldw)

#### Supplementary Material figure s1 ####
rm(list = ls())
my_l = c(3.26029392,-0.00491313,0.25342392) # wood curve (exp(c1 + c2*age + c3*log(age)))
growth = c(6900, 215) # linear part of growth (c1 + c2 * age)
source('allfunctions.R')
coef = NA
set.seed(123456)
runCase(treps = 100,coef = NA)$data%>% group_by(LINE,DATE) %>% 
  mutate(mean = mean(value)) %>% ungroup() %>% mutate(DATE = DATE -135) %>% 
  rename(`Days post challenge` = DATE, Deviation = value) -> data
data %>% filter(endsWith(ID,"_1")) %>%
  filter(LINE == 'resilient') %>% ggplot(aes(x = `Days post challenge`)) + 
  geom_line(aes(y = Deviation), alpha = .3) + geom_line(aes(y = mean)) +
  scale_y_continuous(limits = c(-.75,.5))+ theme_pubr()+labs(title = "A")+
  scale_x_continuous(sec.axis = sec_axis(~.+135, breaks = seq(0,360,90), name = "Days in milk" ))-> f1.a
data %>% filter(endsWith(ID,"_1")) %>%
  filter(LINE == 'unrecovered') %>% ggplot(aes(x = `Days post challenge`)) + 
  geom_line(aes(y = Deviation), alpha = .3) + geom_line(aes(y = mean)) +
  scale_y_continuous(limits = c(-.75,.5))+theme_pubr()+ labs(title = "B") +
  scale_x_continuous(sec.axis = sec_axis(~.+135, breaks = seq(0,360,90), name = "Days in milk" ))-> f1.b
data %>% filter(endsWith(ID,"_2")) %>%
  filter(!LINE%in%c('unrecovered','resilient')) %>% ggplot(aes(x = `Days post challenge`)) + 
  geom_line(aes(y = Deviation), alpha = .3) + geom_line(aes(y = mean)) + 
  facet_wrap(~LINE) +scale_y_continuous(limits = c(-.75,.5))+ 
  scale_x_continuous(sec.axis = sec_axis(~.+135, breaks = seq(0,360,90), name = "Days in milk" ))+
  theme_pubr() +labs(title = "C")-> f1.c
toprow <- ggpubr::ggarrange(plotlist = list(f1.a,f1.b), nrow = 1)
ggpubr::ggarrange(plotlist = list(toprow,f1.c), nrow = 2)
ggsave("finalFigures/S1_deviations.png", width = 8, height = 5)
#### figure 1 ####
my_l = c(3.26029392,-0.00491313,0.25342392) # wood lactation curve (exp(c1 + c2*age + c3*log(age)))
coef = my_l; mtd = 1
set.seed(1234)
runCase(treps = 100,coef = coef,method=mtd)$data %>% group_by(LINE,DATE) %>% 
  mutate(mean = mean(raw)) %>% ungroup() %>% mutate(DATE = DATE -135) %>% 
  rename(`Days post challenge` = DATE, Deviation = value, y = raw) -> data
(data %>% filter(endsWith(ID,"_1")) %>% filter(LINE == 'resilient') %>% 
  ggplot(aes(x = `Days post challenge`)) + geom_line(aes(y = y), alpha = .3) +
  geom_line(aes(y = mean)) + 
  geom_line(aes(y=exp(my_l[1]+my_l[2]*age+my_l[3]*log(age))), color='red',linetype=2)+
  ylab("Yield (liter/day)")+xlab("Days post perturbation")+
  scale_y_continuous(limits = c(8,73))+theme_pubr()+ labs(title = "A") +
  scale_x_continuous(sec.axis = sec_axis(~.+135, breaks = seq(0,360,90), name = "Days in milk" ))-> f3.a)
(data %>% filter(endsWith(ID,"_1")) %>% filter(LINE == 'unrecovered') %>% 
  ggplot(aes(x = `Days post challenge`)) + geom_line(aes(y = y), alpha = .3) +
  geom_line(aes(y = mean)) + 
  geom_line(aes(y=exp(my_l[1]+my_l[2]*age+my_l[3]*log(age))), color='red',linetype=2)+
  scale_y_continuous(limits = c(8,73))+theme_pubr()+ labs(title = "B") +
  ylab("Yield (liter/day)")+xlab("Days post perturbation")+
  scale_x_continuous(sec.axis = sec_axis(~.+135, breaks = seq(0,360,90), name = "Days in milk" ))-> f3.b)
(data %>% filter(endsWith(ID,"_1")) %>% filter(!LINE%in%c('unrecovered','resilient')) %>% 
  ggplot(aes(x = `Days post challenge`)) + geom_line(aes(y = y), alpha = .3) + 
  geom_line(aes(y = mean)) + 
  geom_line(aes(y=exp(my_l[1]+my_l[2]*age+my_l[3]*log(age))), color='red',linetype=2)+
  facet_wrap(~LINE,) + theme_pubr() + ylab("Yield (liter/day)")+
  scale_y_continuous(limits = c(8,73))+ labs(title = "C")+
  xlab("Days post perturbation")+
  scale_x_continuous(sec.axis = sec_axis(~.+135, breaks = seq(0,360,90), name = "Days in milk" ))-> f3.c)
toprow <- ggpubr::ggarrange(plotlist = list(f3.a,f3.b), nrow = 1)
ggpubr::ggarrange(plotlist = list(toprow,f3.c), nrow = 2)
ggsave("finalFigures/Figure1_response.png", width = 8, height = 5)

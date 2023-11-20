#!/usr/bin/env Rscript

Npop <- 100
args <- commandArgs(trailingOnly=T)
usage <- function(){cat("Use: ");  cat(  "TargetTrajectory Study Estimation Rep")
  cat("TargetTrajectory: 1=known, 2=line, 3=wood\n")
  cat("Study: 1=Comparison, 2=Amp, 3=Phase, 4=Delay, 5=Frequency, 6=window\n")
  cat("Estimation: 1=Quantile, 2=Repeated, 3=Average\n")
  cat("Rep=1..100 (int)\n");  stop(1)}
if (length(args) != 4) {usage()} 

targetTrajectory <- c("untrended","growth","my")
targetTrajectoryChoice <- strtoi(args[1])
scenario <- targetTrajectory[targetTrajectoryChoice]
cat("target trajectory: ", scenario,"\n")

studyChoice <- strtoi(args[2])
study <- c("comparison", "amplitude", "phase", "delay", "frequency", "window")
cat("study: ", study[studyChoice],"\n")

method <- strtoi(args[3])
allmethods <- c("QR", "RR", "AF")
cat("method employed: ", allmethods[method], '\n')
taus <- c(0.7, 0.85, NA) # default tau values for 1:QR, 2:RR, 3:AF

reps <- strtoi(args[4])
cat("replicate ", reps, '\n')

{if (!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)}
if (!require(readxl)){install.packages('readxl'); library(readxl)}
if (!require(ggpubr)){install.packages('ggpubr'); library(ggpubr)}
if (!require(mvtnorm)){install.packages('mvtnorm'); library(mvtnorm)}
if (!require(pROC)){install.packages("pROC"); library(pROC)}#if (!require(kableExtra)){install.packages("kableExtra"); library(kableExtra)}
if (!require(quantreg)){install.packages("quantreg"); library(quantreg)}
options(dplyr.summarise.inform = F)}
my = c(3.26029392,-0.00491313,0.25342392) # wood lactation curve (exp(c1 + c2*age + c3*log(age)))
growth = c(6900, 215) # linear part of growth (c1 + c2 * age)

source("allfunctions.R")
### groups for pair-wise comparison
Grp1 <- c(rep('resilient',4),rep('steady',3),rep('compensatory',2),'relapsed')
Grp2 <- c(Grp1[c(5,8,10)],"unrecovered",Grp1[c(8,10)],"unrecovered",Grp1[10],rep("unrecovered",2))
# get the coefficients
coef = `if`(scenario == "untrended", NA,`if`(scenario=='growth',growth,my))
## Known Target
switch(study[studyChoice],
comparison={
  finalResult <- tibble(grp1 = character(0), grp2 = grp1, aurocc = numeric(0), 
                        rep = aurocc, transformation = grp1)
  out <- runCase(treps = Npop,coef = coef, method = method, tauValue = taus[method])
  finalResult <- PairwiseComparison(finalResult, out, reps)
  finalResult <- finalResult %>% Regroup()
  finalResult %>%
    mutate(method = method) %>% 
    write.table(paste0("data/pair_", scenario,"_method_",method, "_rep_",reps,".txt"), row.names = F, quote = F)
},
delay={
  finalResult <- tibble(transformation = character(0), LINE = character(0),
                        value = numeric(0), Tdelay = value)
  def.val <- 135
  for (tdels in seq(-120, 120, by = 10)){
    out <- runCase(treps = Npop, coef = coef, method = method, Tper_start = def.val+tdels, tauValue = taus[method])
    finalResult <- out$result2 %>% group_by(transformation, LINE) %>% 
      summarise(value = mean(value)) %>% ungroup() %>% 
      mutate(Tdelay = tdels) %>% bind_rows(finalResult,.)
  }
  finalResult %>% 
    mutate(method = method, rep = reps) %>% 
    write.table(paste0("data/delay_", scenario,"_method_",method, "_rep_", reps, ".txt"), row.names = F, quote = F)
},
phase={
  finalResult <- tibble(transformation = character(0), LINE = character(0),
                        value = numeric(0), rec_increase = value)
  for (incr in seq(0, 70, by = 7)){
    out <- runCase(treps = Npop, rec_increase = incr, coef = coef, method = method, tauValue = taus[method])
    finalResult <- out$result2 %>% group_by(transformation, LINE) %>% 
      summarise(value = mean(value)) %>% ungroup() %>% 
      mutate(rec_increase = incr) %>% bind_rows(finalResult,.)
  }
  finalResult %>% 
    mutate(method = method, rep = reps) %>% 
    write.table(paste0("data/recspeed_", scenario,"_method_",method, "_rep_", reps, ".txt"), row.names = F, quote = F)
},
amplitude={
  finalResult <- tibble(transformation = character(0), LINE = character(0),
                        value = numeric(0), amp_increase = value)
  for (incr in seq(0.0, 0.1, by = 0.01)){
    out <- runCase(treps = Npop, amp_increase = incr, coef = coef, method = method, tauValue = taus[method])
    finalResult <- out$result2 %>% group_by(transformation, LINE) %>% 
      summarise(value = mean(value)) %>% ungroup() %>% 
      mutate(amp_increase = incr) %>% bind_rows(finalResult,.)
  }
  finalResult %>% 
    mutate(method = method, rep = reps) %>% 
    write.table(paste0("data/amp_", scenario,"_method_",method, "_rep_", reps, ".txt"), row.names = F, quote = F)
},
frequency={
  finalResult <- tibble(transformation = character(0), LINE = character(0),
                        value = numeric(0), freq = value)
  for (d in c(1:7,10,14,21,30)){
    out <- runCase(treps = Npop, freq = d, coef = coef, method = method, tauValue = taus[method])
    finalResult <- out$result2 %>% group_by(transformation, LINE) %>% 
      summarise(value = mean(value)) %>% ungroup() %>% 
      mutate(freq = d) %>% bind_rows(finalResult,.)
  }
  finalResult %>% 
    mutate(method = method, rep = reps) %>% 
    write.table(paste0("data/freq_", scenario,"_method_",method, "_rep_", reps, ".txt"), row.names = F, quote = F)
},
window={
  finalResult <- tibble(transformation = character(0), LINE = character(0),
    value = numeric(0), observation = value, obs_start = value, 
    pert_length = value, pert_start= value)
  for (Tperturb in c(90,60,30,20)){
    for (d in unique(c(seq(Tperturb,4*Tperturb,by=20), 4*Tperturb, 360))){
      out <- runCase(treps = Npop, coef = coef, Tobs = d, Tper = Tperturb, method = method,
                    Tobs_start = (360-d)/2+1, Tper_start = (360 - Tperturb)/2+1, tauValue = taus[method])
      finalResult <- out$result2 %>% group_by(transformation, LINE) %>% 
      summarise(value = mean(value)) %>% ungroup() %>% 
      mutate(observation = d, obs_start = (360-d)/2+1, pert_length = Tperturb,
              pert_start = (360 - Tperturb)/2+1) %>% bind_rows(finalResult,.)
    }
  }
  finalResult %>% 
    mutate(method = method, rep = reps) %>% 
    write.table(paste0("data/obs_", scenario,"_method_",method, "_rep_", reps, ".txt"), row.names = F, quote = F)
},
default={
  cat("error: should not been here")
}
)



#' The purpose of this function is to quickly implement standardisation the way
#' it is explained in Berghof et al (i.e., divide by s.d.). However, this did
#' not become useful at all. Therefore, it basically does a renaming
dev_cte <- function(data, cte = 0.0){ # function for calculating deviations
  data %>% group_by(trait, age) %>% 
    mutate(deviation = (value - cte)) %>% ungroup() %>% return()
}

#' returns a cosine function to be used in making deviations. This function is
#' the same sine function provided in the paper
makef_cos <- function(A, per, P0){ 
  
  #' heaviside function
  H <- function(t, T0){
    return (t >= T0)
  }
  
  #' attaching mode functions
  f <- function(t){
    nmods = length(A)
    out = 0.0
    ts = cumsum(c(0,per[1:nmods-1]))
    te = c(ts[2:nmods],sum(per))
    Am = c(P0, cumsum(A) + P0)
    Am = .5 * (Am[1:nmods] + Am[2:(nmods+1)])
    for (i in seq(1,nmods)){
      out = out + (A[i]*cos(pi*(t-te[i])/(te[i]-ts[i]))/2 + Am[i]) *
        as.numeric(xor(H(t, ts[i]), H(t, te[i])))
    }
    return(out)
  }
  return(f)
}

summarySE<-function(data=NULL,measurevar,groupvars=NULL,na.rm=F,conf.interval=.95,.drop=T){
  # Function adapted from Rmisc package V1.5.1 by Ryan M. Hope
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )
  # Rename the "mean" column
  datac <- plyr::rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

#' calculates RI (skewness, LV, AUC, MS and ACF1) given data
#' 
#' @param data data that has been already passed to ref function
#' @param ... other distinct features to be passed to distinct function 
addAggregate <- function(data,...){
  # mean_ is the mean of deviations of an individual
  # sd_ is the sd of ....
  # diff is the nominator in calculating logVar and S
  # n is number of deviation observations for an individual
  data %>% group_by(LINE,trait, ID) %>%
    mutate(mean_ = mean(deviation, na.rm = T), sd_ = sd(deviation, na.rm=T),
           diff = deviation - mean_, n = sum(!is.na(deviation)),
           LV = log( sum(diff^2, na.rm = T) / (n-1) ),
           S = ( n / ((n-1)*(n-2)) ) * sum( (diff / sd_)^3, na.rm = T ) ,
           AC1 = sum(diff * (lead(deviation) - mean_), na.rm = T) / 
             sum(diff^2, na.rm = T),
           MS = mean(deviation**2,na.rm = T)) %>% 
    mutate(Int = (deviation+lag(deviation))/2 * c(NaN, diff(age))) %>% # trapezoid
    mutate(Int = abs(sum(Int, na.rm = T))) %>% 
    ungroup() %>% 
    select(ID:trait, LV:Int) %>% 
    pivot_longer(c('LV', 'S', 'AC1','Int','MS'), 
                 names_to = 'transformation') %>%
    mutate(value = ifelse(is.infinite(value),NA,value)) %>% 
    distinct(LINE, trait, transformation, ID, value, ... = ...) %>% 
    drop_na() %>% # some points (e.g.,first time for Int) are NaN
    return()
}

#' a repetitive regression model
#' 
#' @param formula formula
#' @param data data
#' @param tol threshold
#' @param respVar response variable without any transformation in formula
#' @param fun back-transfer function (if formula include transformation on response variable
replmG <- function(formula, data, tol = 0.7, respVar = formula[[2]], fun = I){
  df <- data
  repeat{
    model <- lm(formula, data = df)
    ind <- fun(predict(model)) * tol > df[[respVar]]
    df <- df[!ind,]
    if (all(!ind)){break;}
  } 
  return(model)
}

# An object similar to lm/qr for average flock
new_avgFlockModel <- function(data, var, fun,...) {
  stopifnot(is.data.frame(data)|is_tibble(data))
  x <- list()
  x$data = data
  x$var = var
  x$fun = fun
  structure(x, class = "avgFlockModel")
}
predict.avgFlockModel <- function(object,...){
  other <- list(...)
  # I just need to replicate the `val`, `l` time
  # get l:
  ## if new data not provided then it comes from the model object
  if (length(other) == 0){
    l <- nrow(object$data) 
  }
  ## otherwise, there should be a dataframe/tibble with column "var"
  else if (length(names(other)) == 0){
    if (is.data.frame(other[[1]]) || is_tibble(other[[1]])){
      if(object$var%in%names(other[[1]])){
        l <- nrow(other[[1]])
      }
    }
  }
  # get val:
  val = mean(pull(object$data,object$var))
  # if we are in wood curve, take a log
  if (identical(object$fun,exp)){val = log(val)}
  rep(val, l)
}
avgFlockModel <- function(formula=NULL,data,respVar = "Value", fun = I){
  stopifnot(identical(fun,I)||identical(fun,exp))
  new_avgFlockModel(data, respVar, fun)
}

#' main function that simulates response of animals, estimates the deviations if
#' needed and calculates resilience indicators
#'
#' @param treps number of representatives. either scalar or a vector of 5 (20)
#' @param coef coefficient. NA, arr_2, or arr_3 (NA)
#' @param sigmaRes sd of final residual (0.1)
#' @param amp_increase increase in amp of 1st mode (0)
#' @param rec_increase increase in period of 2nd mode (0)
#' @param freq frequency of measurement, 1:everyday, 2:every other day,... (1)
#' @param cvPer cv for period (2)
#' @param cvAmp cv for amp (0.01)
#' @param tauValue tau for QR. Redundant if method is 3 (?)
#' @param method fitting method. 1:QR, 2:Repeated regression, 3:Average (1)
#' @param residue_divide should the residues be divided by predicted value (T)
#' @param Tobs observation time (360)
#' @param Tper perturbation period (90)
#' @param Tobs_start start of observation (1)
#' @param Tper_start start of perturbation (136)
runCase <- function(treps = 20, coef, sigmaRes = 0.1, 
                    amp_increase = 0, rec_increase = 0, freq = 1,
                    cvPer = .2, cvAmp = 0.1, tauValue = 0.7, method = 1,
                    residue_divide = TRUE, Tobs = 360, Tper = 90, 
                    Tobs_start = 1, Tper_start = 136){
  #### number of representatives ####
  if (length(treps) == 1) {
    t1 = t2a = t2b = t2c = t3 = treps # equal representatives in case one val is provided
    treps = c(t1, t2a, t2b, t2c, t3)
  } else if (length(treps)!=5) {
    stop("Must input a variable of size 1 or 5")
  } else{
    t1 = treps[1]; t2a = treps[2]; t2b = treps[3]; t2c = treps[4]; t3 = treps[5]
  }
  # Tdel is obsolete, it has to be determined and calculated by user
  Tdel <- Tper_start - Tobs_start
  Tfin <- Tobs_start + Tobs
  #### input checking ####
  if (Tobs < (Tper+rec_increase)){stop("Tobs must be greater than Tper and rec_increase")}
  if (Tdel < 0){stop("Tper_start must be after (g.t.) Tobs_start")}
  if (Tfin<(Tper+Tper_start+rec_increase)){stop(
    "The whole perturbation (including extended recovery) shall be in the window of observation"
  )}
  if (Tper %% 10 != 0){stop("Better to have Tper divisble by 10")}
  if (amp_increase > 1.01){stop("too much increase in amplitude")}
  data <- tibble(ID = character(0), LINE = character(0), DATE = numeric(0),
                 age = numeric(0), trait = character(0), value = numeric(0))
  #### parameter inputs ####
  design <- tibble(type_1 = t1, type_2a = t2a, type_2b = t2b, type_2c = t2c, type_3 = t3)
  params <- list(type_1 = NULL,type_2a = NULL,type_2b = NULL, type_2c = NULL, type_3 = NULL)
  types <- colnames(design); 
  ref <- function(data){return(dev_cte(data,cte=0))} # reference fun to calculation dev
  sigma_res = sigmaRes
  cv <- list(per = cvPer, amp = cvAmp)
  P0 <- .0; TRec <- 0.7 * Tper ; N <- Tobs
  # for now use this (for calculating deviations, i.e., makefcose(A,phi,P)).
  # Then, shift the calculations by Tdel=Tper_start-Tobs_start.
  # finally change age to real value (i.e., 1:Tobs+Tobs_start)
  age = 1:Tobs 
  age[N] <- age[N] - 1e-6 # this is for the heaviside function to be true at Tf
  #### defining types ####
  ### type 1 (resilient)
  amp <- c(0., 0., 0., 0., 0.)
  per <- c(0.3 * Tper, 0.7 * Tper, 10, 10, NA) # NA to be valued later
  params$type_1 <- data.frame(amp = amp, per = per) %>% 
    mutate(sd_amp = abs(cv$amp*amp), sd_per = cv$per*per)
  ### type 2a (recovered - steady)
  amp <- c(-.4 - amp_increase, .4 + amp_increase, 0., 0., 0.) # -0.4 is default reduction
  per <- c(0.3 * Tper, 0.7 * Tper,5 ,5, NA) # 
  params$type_2a <- data.frame(amp = amp, per = per)%>% 
    mutate(sd_amp = abs(cv$amp*amp), sd_per = cv$per*per)
  ### type 2b (recovered - over-compensatory)
  amp <- c(-.4 - amp_increase, .6 + amp_increase, -.2, 0., 0.)
  per <- c(0.3 * Tper, 0.5 * Tper, 0.2 * Tper, 10, NA) # 
  params$type_2b <- data.frame(amp = amp, per = per)%>% 
    mutate(sd_amp = abs(cv$amp*amp), sd_per = cv$per*per)
  ### type 2c (recovered - relapsed)
  amp <- c(-.4 - amp_increase, .3+ amp_increase, -.10, .2, 0)
  per <- c(0.3 * Tper, 0.2 * Tper, 0.3 * Tper, 0.2 * Tper, NA)
  params$type_2c <- data.frame(amp = amp, per = per)%>% 
    mutate(sd_amp = abs(cv$amp*amp), sd_per = cv$per*per)
  ### type 3
  amp <- c(-.4 - amp_increase, .1 + amp_increase/4.0, 0, 0, 0)
  per <- c(0.3 * Tper, 0.7 * Tper, 10, 10, NA)
  params$type_3 <- data.frame(amp = amp, per = per)%>% 
    mutate(sd_amp = abs(cv$amp*amp), sd_per = cv$per*per)
  #### creating data ####
  for (t in types){
    if (design[[t]] <= 0) {next} # skip classes that don't have representatives
    temp <- params[[t]]
    d <- length(temp$per) # this must be always 5
    sigma_amp <- diag(temp$sd_amp)
    Amp <- mvtnorm::rmvnorm(design[[t]], mean = temp$amp, sigma = sigma_amp^2) # sample amplitudes for all individuals in that group
    # re-scaling sampled values, i.e., forcing the sum of modes to be equal to the type of response.
    # if this is not the case there will be individuals with values over/below the final deviation
    for(j in seq(d,1)){if(temp$amp[j]!=0){break}} # first, find j for last mode with non-zero amplitude
    for (i in seq(1, design[[t]])){
      Amp[i,j] = Amp[i,j] - sum(Amp[i,1:j]) + sum(temp$amp)}
    
    sigma_per <- diag(temp$sd_per[1:(d-1)]) # sd for phases
    for (i in seq(1, design[[t]])){  # for each individual
      # the last time should not be random
      per <- mvtnorm::rmvnorm(1, mean = temp$per[1:(d-1)], sigma = sigma_per^2)
      per[2] <- per[2] + rec_increase 
      per <- c(per, Tfin - sum(per)) # in fact fix the last time such that sum = Tfin
      f <- makef_cos(Amp[i,], per, P0) # make the mode function
      val <- f(age) + rnorm(N, mean = 0, sd = sigma_res) # run the mode function and add residue
      
      # make a dataframe with these values and append to `data`
      data <- tibble(ID = paste(t,i,sep='_'), LINE = t, DATE = age, age = age,
                     trait = 'production', value = val) %>% 
        bind_rows(data,.)
    }
  }
  
  if (Tdel > 0){ # if there is a delay, shift the simulated deviations
    data <- data %>% group_by(LINE, ID) %>% 
      mutate(del = ifelse(DATE>Tdel, lag(value, Tdel),
                          P0 + rnorm(Tdel,sd = sigma_res))) %>% ungroup() %>% 
      mutate(value = del) %>% select(-del)}
  
  # readjusting age as per the comment above (for variable age)
  # therefore, DATE shall always start from 1
  data <- data %>% mutate(age = age + Tobs_start - 1)
  # just renaming some items
  data <- data %>% 
    mutate(LINE = case_when(LINE == 'type_1'~'resilient', 
                            LINE == 'type_2a'~'steady',
                            LINE == 'type_2b'~'compensatory',
                            LINE == 'type_2c'~'relapsed',
                            LINE == 'type_3'~'unrecovered')) %>% 
    mutate(LINE = factor(LINE, 
                         levels = c('resilient', 'steady','compensatory',
                                    'relapsed','unrecovered'))) 
  #### adding values (trending) if needed ####
  if (any(is.na(coef))){ # pass
  }else{
    if (length(coef) == 2){ # case of linear growth
      data <- data %>% group_by(ID) %>% 
        mutate(Value = (coef[1] + coef[2]*age)*(value + 1) ) %>%
        ungroup() %>% rename(origvalue = value)
      formula <- Value ~ age
      linkFun <- I # for repeated regression
    }
    else if (length(coef) == 3){ # case of wood lactation curve
      data <- data %>% group_by(ID) %>%
        mutate(Value = exp(coef[1] + coef[2]*age + coef[3]*log(age))*(value + 1) ) %>%
        ungroup() %>% rename(origvalue = value)
      ### a small adjustment needed in case a milk-yield value goes negative
      if (any(data$Value < 0)){
        warning(paste(sum(data$Value < 0)), " values adjusted!")
        data <- data %>% group_by(ID) %>% mutate(j = min(abs(Value))) %>% 
          mutate(Value = ifelse(Value < 0, j, Value)) %>% select(-j) %>% ungroup()
      }
      formula <- I(log(Value))~age+I(log(age))
      linkFun <- exp # for repeated regression
    }
    else {return(-1)}
    
    # estimating target trajectory
    if (method == 1){ # quantile regression
      data <-  data %>% group_nest(ID) %>% 
        mutate(model = map(data,~rq(formula, data = ., tau = tauValue))) # quantile regression
    }
    else if (method == 2){
      data <- data %>% group_nest(ID) %>% 
        mutate(model = map(data,~replmG(formula, data = ., tol = tauValue,
                                        respVar = "Value", fun = linkFun)))
      # instead of qr, I do a repeated regression with model 
      #   log(y) ~ a * log(x) + b * x + c )
      # and then remove those points s.t. exp(pred(x)) * tol > y ,
      # where tol = tau_value, see replmG (repeated lm for general model)
    } else if (method == 3){
      data <- data %>% group_nest(age) %>% # for avg, grouping is on age rather than ID
        mutate(model = map(data,~avgFlockModel(formula, data = .,
                                        respVar = "Value", fun = linkFun)))
      # with the custom S3 class (avgFlockModel) and its overriden method 
      # predict, computation with target trajectory as average flock (method3)
      # is computed with little change in the code
    } else {return(-2)}
    
    ### since y = (1+noise)*trend , one could divide the residue by trend (predict)
    ## whether residues need to presented as is or standardised (divided by phenotype)
    ## IMPORTANT: CURRENTLY THIS IS WHAT I DO: BASED ON CORRESPONDENCE WITH 
    ## M. POPPE, ONE WANTS TO STANDARDISE RESIDUALS IF THEY HAVE LARGE VARIATION
    ## THIS IS MY WAY OF STANDARDISING
    data <- data %>% mutate(pred = map2(model, data, \(x,y) linkFun(predict(x, y))))
    data <- data %>% mutate(residual = map2(pred, data, \(x,y) y$Value-x))
    if (residue_divide){
      data <- data %>% mutate(residual = map2(pred, residual, \(x,y) y/x))
    }

    data <- data %>% select(-model) %>% unnest(cols = c(data, residual, pred)) %>% 
      rename(value = residual, raw = Value)   
  }
  data <- data %>% ungroup()
  # last day was fiddled a bit for heaviside function. Restoring that
  data <- data %>% mutate(DATE = ceiling(DATE), age = ceiling(age))
  ### removing data based on given frequency ####
  data <- data %>% filter((DATE-1)%%freq==0) # keeping the first datapoint (DATE-1=0 for first day)
  #### defining groups for pair-wise analysis ####
  cmbs <- data %>% distinct(LINE) %>% pull(LINE) %>% combn(., 2)
  nc <- cmbs %>% ncol()
  grps <- list()
  for (i in seq(1,nc)){ grps[[i]] <- cmbs[,i] }
  #### analysis ####
  i <- 1
  result2 <- data %>% filter(LINE%in%grps[[i]]) %>% ref() %>% addAggregate() %>%
    mutate(grp = i)
  if (nc > 1){for (i in seq(2, nc)){
    result2 <- data %>% filter(LINE%in%grps[[i]]) %>% 
      ref() %>% addAggregate() %>% mutate(grp = i) %>% bind_rows(result2) 
  }}
  #### summarising results ####
  # averaging over LINE for each RI
  sresult <- result2 %>% group_by(transformation, LINE) %>% 
    summarise(value = mean(value)) %>% ungroup()
  #### output ####
  return(list(result2 = result2, data = data , nc = nc, grps=grps, method = method, coef = coef))
}

#' calculates the AUROC values for each group in a simulated scenario
PairwiseComparison <- function(finalResult,out, reps){
  data = out$data; result2 = out$result2 ; nc = out$nc; grps = out$grps
  method = out$method; coef = out$coef
  # who is more resilient than who (ranking)
  ranks <- list(resilient = 5, compensatory = 3, steady = 3,
              relapsed = 3, unrecovered = 1)
  for (tr in c("LV","S","AC1","Int","MS")){
    rt <- result2 %>% filter(transformation == tr)
    roctble <- data.frame(row.names = distinct(data,LINE) %>% pull(LINE))
    for (x in rownames(roctble)){
      roctble[[x]] <- NA
    }
    colnames(roctble)<- row.names(roctble)
    for (i in seq(1,nc)){
      y <- as.character(grps[[i]])
      temp <- rt %>% filter(grp == i)
      
      case <- temp %>% filter(y[1] == LINE) %>% pull(value)
      cont <- temp %>% filter(y[2] == LINE) %>% pull(value)
      if (tr %in% c('AC1')){case <- abs(case) ; cont <- abs(cont)} 
      dirc = ifelse(ranks[[y[1]]] > ranks[[y[2]]], '>', '<') # normal direction
      # for skewness
      if (tr %in% c("S")) {
         if (!any(is.na(coef)) && method == 3) {# if we are in method3 for trended data
            dirc = ifelse(ranks[[y[1]]] > ranks[[y[2]]], '<', '>') # reverse the direction
            # but use the values as it is
         } else {  # use as before 
            case <- abs(case) ; cont <- abs(cont)  # but use absolute values for comparison
         }
      }
      # roc assumes "control" {direction} "case"
      #if (tr == 'Int'){ # but reverse for Int (removed)
      #  dirc = ifelse(ranks[[y[1]]] > ranks[[y[2]]], '<', '>')
      #}
      roctble[y[1],y[2]] <- roc(case = case, control = cont, direction = dirc) %>% auc() 
    }
    roc <- roctble %>% mutate(grp1 = rownames(roctble)) %>% 
      pivot_longer(-grp1, names_to = 'grp2', values_to = 'aurocc') %>% 
      drop_na()
    finalResult <- roc %>% 
      mutate(transformation = tr, rep = reps) %>%
      bind_rows(finalResult)
  }
  return(finalResult)
}

#' produces a constant array to be used for regrouping results
Regroup <- function(finalResult){
  finalResult %>% mutate(grp = case_when(
    (grp1 == 'resilient')&(grp2 == 'steady')~1, 
    (grp1 == 'resilient')&(grp2 == 'compensatory')~2,
    (grp1 == 'resilient')&(grp2 == 'relapsed')~3,
    (grp1 == 'resilient')&(grp2 == 'unrecovered')~4,
    (grp1 == 'steady')&(grp2 == 'compensatory')~5,
    (grp1 == 'steady')&(grp2 == 'relapsed')~6,
    (grp1 == 'steady')&(grp2 == 'unrecovered')~7,
    (grp1 == 'compensatory')&(grp2 == 'relapsed')~8,
    (grp1 == 'compensatory')&(grp2 == 'unrecovered')~9,
    (grp1 == 'relapsed')&(grp2 == 'unrecovered')~10))  %>% return()
}

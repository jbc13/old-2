qc_1 <- function(data,var,exmin,exmax)
  {
  # d <- tar_data_inmet_2008_2016_4yrs_south_qc
  d <- data[,c('site','date', var)]
  d[is.na(d[, var]), var] <- FALSE
  lgcl <- (d[[var]] > exmin & d[[var]] < exmax)
  qc <- 1
  suspect <- ifelse(lgcl == FALSE,1,0)
  rt1 <- data.frame(d,qc,suspect)
  return(rt1)
  }


# dates = tar_data_2008_2016_4y
# var = 'tair'
# exmin = -30
# exmax = 50





################################################################################

range.test.1 <- function(dates,var,exmin,exmax)
{ d <- dates[,c('site','date', var)]
  lgcl <- (d[[var]] > exmin & d[[var]] < exmax)
  qc <- 1
  suspect <- ifelse(lgcl == FALSE | is.na(lgcl),1,0)
  rt1 <- data.frame(d,qc,suspect)
  return(rt1) }



range.test.2 <- function(dates, abs_dates, var, na_equal_nopass = FALSE, return = 'grouped')
{ library(dplyr)
  abs_all <- abs.by.site(abs_dates)
  df_all <- full_join(dates, abs_all, by = 'site')
  if (var == 'tair')
  {df_var = df_all[c('site','date','tair','abs_tair_min','abs_tair_max')]
  var_min = 'abs_tair_min'
  var_max = 'abs_tair_max'}
  if (var == 'prec')
  {df_var = df_all[c('site','date','prec','abs_prec_min','abs_prec_max')]
  var_min = 'abs_prec_min'
  var_max = 'abs_prec_max'}
  if (var == 'p')
  {df_var = df_all[c('site','date','p','abs_p_min','abs_p_max')]
  var_min = 'abs_p_min'
  var_max = 'abs_p_max'}
#  rt01 = c(T,T,T,T,T,T,T,T,T,NA,NA,NA,NA,T,T,T,T,T,F,T,T,F,T,T,T,T,T,F)
#  rt01
#  table(rt01 == -1)
  rt01 <- df_var[[var]] >= df_var[[var_min]] & df_var[[var]] <= df_var[[var_max]]
  rt01[is.na(rt01)] <- -1
  df_sdrt01 <- data.frame(df_all['site'],df_all['date'],range_test_1 = rt01)
  rt <- df_sdrt01 %>%
    group_by(site) %>%
    summarise(
      pass = sum(rt01 == 1),
      not_pass = sum(rt01 == 0),
      NA_val = sum(rt01 == -1))
  return(rt) }

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

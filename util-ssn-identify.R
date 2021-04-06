

acf_signif <- function(u, max_lag = 38) {
  # calculate ACF and get the values
  xacf <- acf(u, plot = F, lag.max = max_lag)$acf[-1]    # The first acf is for lag 0
  # calculate significance threshold
  psig <- qnorm(1 - 0.05 / 2) / sqrt(length(u))
  # lags that are not sinificant are set to NA
  xacf[abs(xacf) <= psig] <- NA
  return(xacf)
}


get_ordered_acf <- function(dat, max_lag = 38, detrend = FALSE) {
  
  if (ncol(dat) > 2) {stop("Input data needs to have only 2 columns")}
  
  # Rename columns to date & value for easier referencing
  col_class <- lapply(dat, class) %>% unlist() %>% as.vector()
  kdd <- which(col_class == "Date")
  if ((length(kdd)  == 0) | (length(kdd) > 1)) {stop("One column needs to be in date format")}
  kval <- setdiff(c(1,2), kdd)
  cname0 <- colnames(dat)
  colnames(dat)[c(kdd, kval)] <- c("date", "val")
  
  xdf <- dat %>% arrange(date)
  
  # Detrend series (if selected) using time as the covariate
  if (detrend) {
    xdf$tt <- 1:nrow(xdf)
    mtr <- lm(val ~ tt, data = xdf)
    xdf$val <- mtr$residuals
  }
  
  a <- acf_signif(xdf$val, max_lag = max_lag)
  
  # order the significant acf lags 
  if (all(is.na(a))) {
    ysig <- "none"
    ysigmax <- NA
  } else {
    alag <- 1:length(a)
    alag[is.na(a)] <- NA
    lagsig <- alag[order(a, decreasing = TRUE)]
    lagsig <- lagsig[!is.na(lagsig)]
    ysig <- paste(lagsig, collapse = ", ")
    ysigmax <- lagsig[1]
  }
  
  (list(lag_sig = ysig, lag_max = ysigmax))
}


get_ordered_spec <- function(dat, detrend = FALSE){
  
  if (ncol(dat) > 2) {stop("Input data needs to have only 2 columns")}
  
  # Rename columns to date & value for easier referencing
  col_class <- lapply(dat, class) %>% unlist() %>% as.vector()
  kdd <- which(col_class == "Date")
  if ((length(kdd)  == 0) | (length(kdd) > 1)) {stop("One column needs to be in date format")}
  kval <- setdiff(c(1,2), kdd)
  cname0 <- colnames(dat)
  colnames(dat)[c(kdd, kval)] <- c("date", "val")
  
  xdf <- dat %>% arrange(date)
  
  # Detrend series (if selected) using time as the covariate
  if (detrend) {
    xdf$tt <- 1:nrow(xdf)
    mtr <- lm(val ~ tt, data = xdf)
    xdf$val <- mtr$residuals
  }
  
  xspec <- spectrum(xdf$val, plot = F)
  spx <- 1/(xspec$freq)
  spy <- 2*xspec$spec
  sp_order <- spx[order(spy, decreasing = TRUE)]
  
  return(sp_order)
}




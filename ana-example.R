library(plyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)

source("util-ssn-identify.R")



# Read synthetic data from csv files
# Columns in the datasets are:
#     Country           country name (only 1 country included)
#     admin_level_1     Administrative level 1 name (total of 4 admin 1)
#     admin_level_2     Administrative level 2 name (total of 35 admin 2)
#     facility          Health facility (total of 291 facilities)
#     date              Dates (monthly)
#     tested_cases      Number of people tested for malaria in the facility per month
#     confirmed_cases   Number of people confirmed for malaria cases (from tested cases) in the facility per month
#     total_population  Population
#     rf                Monthly rainfall
#     tpr               Test positivity ratio (confirmed_cases / tested_cases)

x0 <- read_csv("data/synthetic_data.csv")


# Aggregate TPR to check the TPR pattern

xadm1 <- x0 %>%
  group_by(admin_level_1, date) %>%
  summarise(across(c("tested_cases", "confirmed_cases", "total_population"),  ~ sum(., na.rm = TRUE)),
               across("rf", ~ mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(tpr = confirmed_cases / tested_cases)

ggplot(xadm1, aes(x = date, y = tpr)) + geom_line() +
  facet_wrap(~ admin_level_1)


# List unique health facility ID
hfid <- unique(x0$facility)



# Identify seasonality using ACF -----------------------------------------------------------------


# placeholder for output data
ya <- data.frame()

# Loop around facilities to calculate the ACF (and extract the significant values)
for (i in 1:length(hfid)) {
  
  xi <- x0 %>% filter(facility == hfid[i]) %>%
    arrange(date)
  
  # Input data needs to be a data frame with 2 columns (date and TPR)
  x4acf <- xi %>% select(date, tpr)
  
  yacf <- get_ordered_acf(x4acf, max_lag = 30, detrend = FALSE)
  
  # Parse the significant lags, and if it meets the criteria (i.e. lag 11 to 13 
  # are in the first 2 highest), then label the facility as seasonal
  if ("none" %in% yacf$lag_sig) {
    ssn <- "No"
  } else {
    xlag <- as.numeric(as.character(str_split(yacf$lag_sig, ", ")[[1]]))
    if (any(c(11:13) %in% xlag[c(1:2)])) {
      ssn <- "Yes"
    } else {
      ssn <- "No"
    }
  }
  
  # put together output data frame
  yi <- data.frame(xi[1, c("admin_level_1", "admin_level_2", "facility")], ssn_detected = ssn)
  ya <- rbind(ya, yi)
  
}

# merge the seasonality label with original data
xa <- merge(x0, ya, by = c("admin_level_1", "admin_level_2", "facility"), all.x = TRUE, all.y = TRUE)

# Aggregate to province level for plotting/viewing the results
xa_adm1 <- xa %>%
  group_by(admin_level_1, date, ssn_detected) %>%
  summarise(across(c("tested_cases", "confirmed_cases", "total_population"),  ~ sum(., na.rm = TRUE)),
            across(c("rf", "tpr"), ~ mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  rename(tpr_average = tpr) %>%
  mutate(tpr = confirmed_cases / tested_cases)

ggplot(xa_adm1, aes(x = date, y = tpr)) + geom_line(aes(colour = ssn_detected)) +
  facet_wrap(~ admin_level_1) +
  scale_colour_manual(values = c("Yes" = "blue", "No" = "red")) +
  theme_minimal()

table(ya[, c("admin_level_1", "ssn_detected")])




# Identify seasonality using spectral density (FFT) ----------------------------------------------

# placeholder for output data
ys <- data.frame()

# Loop around facilities to calculate the ACF (and extract the significant values)
for (i in 1:length(hfid)) {
  
  xi <- x0 %>% filter(facility == hfid[i]) %>%
    arrange(date)
  
  # Input data needs to be a data frame with 2 columns (date and TPR)
  x4spec <- xi %>% select(date, tpr)
  
  yspec <- get_ordered_spec(x4spec, detrend = FALSE)
  
  ssn <- ifelse(any(c(11:13) %in% yspec[1:2]), "Yes", "No")
  
  # put together output data frame
  yi <- data.frame(xi[1, c("admin_level_1", "admin_level_2", "facility")], ssn_detected = ssn)
  ys <- rbind(ys, yi)
  
}


# merge the seasonality label with original data
xs <- merge(x0, ys, by = c("admin_level_1", "admin_level_2", "facility"), all.x = TRUE, all.y = TRUE)

# Aggregate to province level for plotting/viewing the results
xs_adm1 <- xs %>%
  group_by(admin_level_1, date, ssn_detected) %>%
  summarise(across(c("tested_cases", "confirmed_cases", "total_population"),  ~ sum(., na.rm = TRUE)),
            across(c("rf", "tpr"), ~ mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  rename(tpr_average = tpr) %>%
  mutate(tpr = confirmed_cases / tested_cases)

ggplot(xs_adm1, aes(x = date, y = tpr)) + geom_line(aes(colour = ssn_detected)) +
  facet_wrap(~ admin_level_1) +
  scale_colour_manual(values = c("Yes" = "blue", "No" = "red")) +
  theme_minimal()

table(ys[, c("admin_level_1", "ssn_detected")])

library(data.table)
library(dplyr)
library(summarytools)  # for creating data summaries
library(lubridate)  # for working with time
library(runner)  # for sum_run() function

##########################SANBS#####################################
df <- fread("../1_data/private/SANBSdata.csv", fill=TRUE)

df <- na.omit(df)
# first create variables that can be used
setkey(df, DonorID, Visit_Date)  # sorts the dataframe

# create unit_loss for counting units loss
df$unit_rbc_loss <- ifelse(df$Outcome == "SUCCESSFUL DONATION" & df$DonProc == "WHOLE BLOOD", 1,
                           ifelse(df$Outcome == "SUCCESSFUL DONATION" & df$DonProc == "DOUBLE RBC", 2, 0))

## rbc_loss_last_12_months ----
## rbc_loss_last_24_months ----


df <- df %>%
  group_by(DonorID) %>%
  mutate(rbc_loss_last_12_months = sum_run(x = unit_rbc_loss,
                                           k = 365,  # last 12 months
                                           idx = as.Date(Visit_Date, format="%Y-%m-%d")))

df <- df %>%
  group_by(DonorID) %>%
  mutate(rbc_loss_last_24_months = sum_run(x = unit_rbc_loss,
                                           k = 365*2,  # last 24 months
                                           idx = as.Date(Visit_Date, format="%Y-%m-%d")))

fwrite(df, "../1_data/private/don_hist2.csv")

########################## VITALANT ##########################################
df <- fread("../1_data/private/donation_donor(w2015-2017).csv", fill=TRUE)

res <- as.POSIXct(as.character(df$DON_DATE_KEY), format = "%Y%m%d", tz ="UTC")

df$DON_DATE_KEY <- res


setkey(df, DONOR_NUMBER, DON_DATE_KEY)  # sorts the dataframe

df$donation <- ifelse(df$PHLEBOTOMY_STATUS == "Successful Phlebotomy", 1, 0)
df <- df %>% group_by(DONOR_NUMBER) %>% mutate(cum_lifetime_donations = cumsum(donation))


## Rename variables ----
df <- df %>% rename(DonorID = DONOR_NUMBER,
                    Visit_Date = DON_DATE_KEY)

# create unit_loss for counting units loss
df$unit_rbc_loss <- ifelse( ((df$PHLEBOTOMY_STATUS == "Successful Phlebotomy" & df$PHLEBOTOMY_GROUP == "Whole Blood") |
                               (df$PHLEBOTOMY_STATUS == "Successful Phlebotomy" & df$PHLEBOTOMY_GROUP == "RBCP") |  # RBCP (collection of one unit of red cells and two units of plasma)
                               (df$PHLEBOTOMY_STATUS == "Successful Phlebotomy" & df$PHLEBOTOMY_GROUP == "Platelet/RBC") |
                               (df$PHLEBOTOMY_STATUS == "Successful Phlebotomy" & df$PHLEBOTOMY_GROUP == "Platelet/RBC/Plasma")
), 1,
ifelse(df$PHLEBOTOMY_STATUS == "Successful Phlebotomy" & df$PHLEBOTOMY_GROUP == "2RBC", 2, 0))


df <- df %>%
  group_by(DonorID) %>%
  mutate(rbc_loss_last_12_months = sum_run(x = unit_rbc_loss,
                                           k = 365,  # last 12 months
                                           idx = as.Date(Visit_Date, format="%Y-%m-%d")))

df <- df %>%
  group_by(DonorID) %>%
  mutate(rbc_loss_last_24_months = sum_run(x = unit_rbc_loss,
                                           k = 365*2,  # last 24 months
                                           idx = as.Date(Visit_Date, format="%Y-%m-%d")))

fwrite(df, "../1_data/private/don_hist2_V.csv")



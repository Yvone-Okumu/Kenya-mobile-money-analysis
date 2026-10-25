# Kenya Mobile money growth and adoption trends

setwd("C:/Users/yvonn/OneDrive/Desktop/Projects/Project 1")


# 1.Load libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(zoo)
#_________________________________________________________________________________

# 2. Load and Clean Dataset

mobile <- read_csv("Mobile Payments.csv")
View(mobile)
data <- mobile |> 
  clean_names() |> 
  # rename columns
  rename(
    reg_accounts_millions = total_registered_mobile_money_accounts_millions,
    volume_millions = total_agent_cash_in_cash_out_volume_million,
    value_ksh_billions = total_agent_cash_in_cash_out_value_k_sh_billions
  ) |> 
  # month and year into a forma data object
  mutate(
    month_num = match(month, month.name),
    date = make_date(year, month_num, 1)
  ) |> 
  # sort from 2007 to 2026
  arrange(date) |> 
  # derive year-over-year growth and average transaction value
  mutate(
    avg_tx_value_ksh = (value_ksh_billions * 1e9)/(volume_millions * 1e6),
    value_yoy_growth = (value_ksh_billions - lag(value_ksh_billions, 12))/lag(value_ksh_billions, 12)*100
  )

# preview cleaned structure
glimpse
(data)
# save
write_csv(data, "Cleaned_mobile_payments.csv")
#_______________________________________________________________________________________
# 3. Summary statistics and key milestones
summary_stats <- data |> 
  summarise(
    start_date = min(date),
    end_date = max(date),
    max_agents = max(active_agents, na.rm = TRUE),
    max_accounts_millions = max(reg_accounts_millions, na.rm = TRUE),
    max_monthly_value_billions = max(value_ksh_billions, na.rm = TRUE),
    latest_avg_tx_size_ksh = last(avg_tx_value_ksh)
  )
print( "Key Growth Milestones")
print(summary_stats)

# Annual summary table
annual_summary <- data |> 
  group_by(year) |> 
  summarise(
    avg_active_agents = mean(active_agents),
    total_volume_millions = sum(volume_millions),
    total_value_ksh_billions = sum(value_ksh_billions),
    end_of_year_accounts = last(reg_accounts_millions)
  )

print("Annual Summary(2007-2026)")
print(annual_summary, n = 25)
#_____________________________________________________________________________________
# 4.Statistical and Correlation Analysis

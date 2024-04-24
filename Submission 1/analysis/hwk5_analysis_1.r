# Meta --------------------------------------------------------------------

## Title:         Econ 470 Homework 5 
## Author:        Leila Mulveny
## Date Created:  4/23/2024
## Description:   This file renders/runs all relevant R code for the assignment


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, modelsummary, fixest)


# ##Question 1: Plot the share of the adult population with direct purchase health insurance over time.

final.insurance <- read_tsv("data/output/acs_insurance.txt")
mcaid.data <- read_tsv("data/output/acs_medicaid.txt")

# Calculate the share of adult population with direct purchase health insurance for each year
final.insurance <- final.insurance %>%
  group_by(year) %>%
  summarise(total_adult_pop = sum(adult_pop),
            total_direct_ins = sum(ins_direct)) %>%
  mutate(share_direct_ins = total_direct_ins / total_adult_pop)

# Plotting the trend over time
question1 <- ggplot(final.insurance, aes(x = year, y = share_direct_ins)) +
  geom_line() +
  labs(x = "Year", y = "Share of Adult Population with Direct Purchase Health Insurance", 
       title = "Trend of Direct Purchase Health Insurance Over Time")

question1

#Question 3 
final.insurance <- read_tsv("data/output/acs_insurance.txt")

# Calculate the share of the adult population with Medicaid for each year
final.insurance <- final.insurance %>%
  mutate(share_medicaid = ins_medicaid / adult_pop)

# Create the ggplot object and store it in the variable question3
question3 <- final.insurance %>% group_by(year) %>% summarize(mean=mean(share_medicaid)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_bw() +
  labs(
    x="Year",
    y="Medicaid",
    title="Adult Population with Medicaid"
  ) +
  geom_vline(xintercept=2013.5, color="red")

# Display the plot
question3


#question 4 

ins.plot.dat <- mcaid.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop) %>%
  group_by(expand_ever, year) %>% summarize(mean=mean(perc_unins))

question4 <- ggplot(data=ins.plot.dat, aes(x=year,y=mean,group=expand_ever,linetype=expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = ins.plot.dat %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype="none") +
  labs(
    x="Year",
    y="Fraction Uninsured",
    title="Share of Uninsured over Time"
  )
question4

##Question5: Calculate the average percent of uninsured individuals in 2012 and 2015, separately for expansion and non-expansion states. Present your results in a basic 2x2 DD table.

# Filter the data for the years 2012 and 2015
mcaid.data_filtered <- mcaid.data %>%
  filter(year %in% c(2012, 2015))

# Calculate the difference in the percent uninsured between 2012 and 2015 for each state
mcaid.data_diff <- mcaid.data_filtered %>%
  group_by(State) %>%
  mutate(diff_uninsured = last(uninsured / adult_pop) - first(uninsured / adult_pop))

# Group the data by expand_ever (Medicaid expansion status) and calculate the average difference-in-differences estimator
avg_did <- mcaid.data_diff %>%
  group_by(expand_ever) %>%
  summarise(avg_diff_uninsured = mean(diff_uninsured, na.rm = TRUE))

# Print the result
print(avg_did)

#Question 6: Estimate the effect of Medicaid expansion on the uninsurance rate using a standard DD regression estimator, 
#again focusing only on states that expanded in 2014 versus those that never expanded.

mcaid.data <- read_tsv("data/output/acs_medicaid.txt")
reg.dat <- mcaid.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

dd.ins.reg <- lm(perc_unins ~ post + expand_ever + post*expand_ever, data=reg.dat)


#Question 7: Include state and year fixed effects in your estimates. 
#Try using the lfe or fixest package to estimate this instead of directly including the fixed effects.

mcaid.data <- read_tsv("data/output/acs_medicaid.txt")
reg.dat <- mcaid.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

m.dd7 <- lm(perc_unins ~ post + expand_ever + treat, data=reg.dat)

m.twfe7 <- feols(perc_unins ~ treat | State + year, data=reg.dat)


#Question 8 
mcaid.data <- read_tsv("data/output/acs_medicaid.txt")
reg.dat <- mcaid.data %>% filter(!is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)


# Run the Difference-in-Differences (DD) regression
m.dd8 <- lm(perc_unins ~ post + expand_ever + treat, data = reg.dat)

# Run the Two-Way Fixed Effects (TWFE) regression
m.twfe8 <- feols(perc_unins ~ treat | State + year, data = reg.dat)



#Question 9 

reg.dat9 <- mcaid.data %>% 
  filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

mod.twfe9 <- feols(perc_unins~i(year, expand_ever, ref=2013) | State + year,
                  cluster=~State,
                  data=reg.dat9)



# #Question 10 


reg.dat10 <- mcaid.data %>% 
  filter(!is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))

mod.twfe10 <- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=reg.dat10)


rm(list=c("final.insurance", "mcaid.data","ins.plot.dat", "mcaid.data_filtered"))
save.image("submission 1/Hw5_workspace.Rdata")

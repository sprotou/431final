library(tidyverse)
library(stringr)
install.packages('seplyr')
library(seplyr)

getwd()

rawData = read.csv('data.csv')
noCurrent = rawData %>%
  filter(loan_status != 'Current')

regexp = "[[:digit:]]+"
test1 = str_extract(rawData$emp_length, regexp) #turn emp_length from string to numeric

modData = rawData %>%
  mutate(our_emp_length = as.numeric(test1))

# dat %>% 
#   mutate(var = replace(var, var != "Candy", "Not Candy"))
modData = modData %>%
  mutate(our_emp_length = replace(our_emp_length, emp_length == '< 1 year', 0))

modData$our_emp_length[is.na(modData$our_emp_length)] = 0


#x[is.na(x)] <- 0

#categorize by loan size, installment size
ggplot(data = modData) + geom_histogram(mapping = aes(modData$installment), bins = 200)

loanAmtQuants = quantile(modData$loan_amnt, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1), na.rm=T) #added 0.9
instQuants = quantile(modData$installment, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1), na.rm=T) #added 0.9
revolBalQuants = quantile(modData$revol_bal, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1), na.rm =T) 
dtiQuants = quantile(modData$dti, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1), na.rm = T) 


# case_when(
#   x %% 35 == 0 ~ "fizz buzz",
#   x %% 5 == 0 ~ "fizz",
#   x %% 7 == 0 ~ "buzz",
#   TRUE ~ as.character(x)
# )
modData = modData %>%
  mutate(loanAmtPerc = case_when(
     
    loan_amnt < loanAmtQuants[2] ~ 0.25,
    loan_amnt < loanAmtQuants[3] ~ 0.5,
    loan_amnt < loanAmtQuants[4] ~ 0.75,
    loan_amnt < loanAmtQuants[5] ~ 0.9,
    loan_amnt <= loanAmtQuants[6] ~ 1
    
    ),
    loanInstPerc = case_when(
      installment < instQuants[2] ~ 0.25,
      installment < instQuants[3] ~ 0.5,
      installment < instQuants[4] ~ 0.75,
      installment < instQuants[5] ~ 0.9,
      installment <= instQuants[6] ~ 1
    ),
    dtiPerc = case_when(
      dti < dtiQuants[2] ~ 0.25,
      dti < dtiQuants[3] ~ 0.5,
      dti < dtiQuants[4] ~ 0.75,
      dti < dtiQuants[5] ~ 0.9,
      dti <= dtiQuants[6] ~ 1
    ),
    revolBalPerc = case_when(
      revol_bal < revolBalQuants[2] ~ 0.25,
      revol_bal < revolBalQuants[3] ~ 0.5,
      revol_bal < revolBalQuants[4] ~ 0.75,
      revol_bal < revolBalQuants[5] ~ 0.9,
      revol_bal <= revolBalQuants[6] ~ 1
    )
  )


statusByAmountTest = modData %>%
  group_by(loanAmtPerc,
           loan_status) %>%
  add_group_summaries(c('loanAmtPerc','loan_status'), #check with Yifu, this could be very useful to know
                      test1 = sum(loanAmtPerc), 
                      test2 = frequency(loan_status)) 


# # NOT RUN {
# 
# add_group_summaries(datasets::mtcars,
#                     c("cyl", "gear"),
#                     group_mean_mpg = mean(mpg),
#                     group_mean_disp = mean(disp)) %.>%
#   head(.)
# 
# # }

statusByAmount2 = modData %>%
  group_by(loanAmtPerc,
           loan_status) %>%
  summarize(numInPerc = n())

# How to do multiple level summarize and mutate


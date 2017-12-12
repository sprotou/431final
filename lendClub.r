library(tidyverse)
library(stringr)



getwd()

rawData = read.csv('data.csv')

regexp = "[[:digit:]]+"
test1 = str_extract(rawData$emp_length, regexp)

modData = rawData %>%
  mutate(our_emp_length = as.numeric(test1))

library("tidyverse")

matches <- read_csv("https://www.dropbox.com/scl/fi/4pptj15s4czpv52dhcjej/10_train.csv?rlkey=iqjivkpptxavii3zghigcrlr0&dl=1")

matches %>% 
  filter(is.na(has_null)) %>% 
  count(has_null)

matches %>% 
  distinct(race)

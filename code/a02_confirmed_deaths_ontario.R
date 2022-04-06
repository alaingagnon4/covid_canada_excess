
# case and death data
url <- "https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv"
download.file(url, destfile = "data_input/confirmed_ontario.csv")
# loading data
df <- read_csv('data_input/confirmed_ontario.csv')

#------- If the episode date is before Jan 21, 2020 - changing to case reported date
df2 <- 
  df %>%
  mutate(date = if_else(Accurate_Episode_Date < ymd("2020-01-22"), 
                      Case_Reported_Date, 
                      Accurate_Episode_Date),
         new = ifelse(Outcome1 == "Fatal", 1, 0),
         age = str_sub(Age_Group, 1, 2),
         age = ifelse(age == "<2", 0, as.double(age))) %>% 
  select(date, age, new) %>% 
  group_by(date, age) %>% 
  summarise(new = sum(new)) %>% 
  ungroup()

df3 <- 
  df2 %>% 
  complete(date, age, fill = list(new = 0)) %>% 
  group_by(age) %>% 
  mutate(dts = cumsum(new)) %>% 
  ungroup()


write_rds(df3, "data_inter/confirmed_deaths_ontario.rds")

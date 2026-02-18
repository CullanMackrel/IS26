#Importing Wood Buffalo Data
WBH2S <- read_csv("WBH2S(Data).csv") 

#Cleaning up data
WBH2S <- read_csv("WBH2S(Data).csv", skip = 1, n_max = 227954) 
 StdWBH2S <- WBH2S %>%
  mutate( "Date" = WBH2S$...1) %>%
   slice(-1) %>%
 select(-`...1`) %>%
  mutate(across(.cols = starts_with("AMS"), ~ replace(., . == 'Z', 0))) %>%
select( -c(`...27`, `...28`,`Wood Buffalo Environmental Association - Continuous Ambient Air Quality Monitoring Program`, `...30`, `...31`, `...32`, `...33`, `...34`)) %>%
   mutate(across(-Date, as.numeric)) %>%
   select(`Date`, everything()) %>%
   separate(Date,
            into = c("Date", "Time"),
            sep = " ") %>%
 mutate(Time = case_when(Time == "01:00" ~ 08:00,
                         Time == "02:00" ~ 09:00,
                         Time == "03:00" ~ 10:00,
                         Time == "04:00" ~ 11:00,
                         Time == "05:00" ~ 12:00,
                         Time == "06:00" ~ 13:00,
                         Time == "07:00" ~ 14:00,
                         Time == "08:00" ~ 15:00,
                         Time == "09:00" ~ 16:00,
                         Time == "10:00" ~ 17:00,
                         Time == "11:00" ~ 18:00,
                         Time == "12:00" ~ 19:00,
                         Time == "13:00" ~ 20:00,
                         Time == "14:00" ~ 21:00,
                         Time == "15:00" ~ 22:00,
                         Time == "16:00" ~ 23:00,
                         Time == "17:00" ~ 00:00,
                         Time == "18:00" ~ 01:00,
                         Time == "19:00" ~ 02:00,
                         Time == "20:00" ~ 03:00,
                         Time == "21:00" ~ 04:00,
                         Time == "22:00" ~ 05:00,
                         Time == "23:00" ~ 06:00,
                         Time == "00:00" ~ 07:00))
 
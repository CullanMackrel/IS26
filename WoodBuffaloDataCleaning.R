#Importing Wood Buffalo Data
WBH2S <- read_csv("WBH2S(Data).csv") 

#Cleaning up data
WBH2S <- read_csv("WBH2S(Data).csv", skip = 1, n_max = 227954) 
 StdWBH2S <- WBH2S %>% 
  mutate( "date" = WBH2S$...1) %>%
   slice(-1) %>%
 select(-`...1`) %>%
  mutate(across(.cols = starts_with("AMS"), ~ replace(., . == 'Z', 0))) %>%
select( -c(`...27`, `...28`,`Wood Buffalo Environmental Association - Continuous Ambient Air Quality Monitoring Program`, `...30`, `...31`, `...32`, `...33`, `...34`)) %>%
   mutate(across(-date, as.numeric)) %>%
   select(`date`, everything()) %>%
   
   separate(date,
            into = c("date", "Time"),
            sep = " ") %>%
   
   rename(Hour = Time) %>%
   
   unite("date", c("date", "Hour"), sep = " " ) %>%
   arrange(date) %>%
   
   mutate(date = paste(as.character(date), StdWBH2S$Hour, sep = " "),
          date = as.POSIXct(date, format = "%Y-%m-%d %H", tz = "MST"),
          date = with_tz(date, tz = "UTC")) %>%
   slice(-(1:4)) %>%
   
 
   timeAverage(avg.time = "6 hour") %>%
   separate(col = date, into = c("date", "Hour"),sep = " ")  %>%
   mutate(Hour = replace_na(Hour, "00:00:00"), 
          Hour = case_when(Hour == "00:00:00" ~ 00,
                           Hour == "06:00:00" ~ 06,
                           Hour == "12:00:00" ~ 12,
                           Hour == "18:00:00" ~ 18,
                           Hour == "07:00:00" ~ 06,
                           Hour == "13:00:00" ~ 12,
                           Hour == "19:00:00" ~ 18,
                           Hour == "01:00:00" ~ 00)) 
 
#Pivoting data to long format
 LongH2S <- StdWBH2S %>%
   pivot_longer(cols = -c("date", "Hour"),
                names_to = "Station",
                values_to = "H2S Conc") 
   group_by(LongH2S$Hour)
 
          
      
 
#Plotting
H2SPlot <- StdWBH2S %>%
  selectByDate(year = 2020:2025) %>%
   timePlot(pollutant = "AMS 4",
            group = TRUE,
            stack = TRUE,
            date.pad = TRUE) 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
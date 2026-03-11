#Importing Wood Buffalo Data
setwd("C:/Users/culla/Documents/data")
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
                           Hour == "01:00:00" ~ 00)) %>%
   mutate(date = as.POSIXct(date, format = "%Y-%m-%d")) %>%
 unite(col = date,
       c("date","Hour"),
       sep = " ") 
 
 #Specifying the two Stations I'll be looking at
 StationH2S <- StdWBH2S %>%
   select(date, `AMS 4`, `AMS 19`)
 
 
#Pivoting data to long format
 LongH2S <- StationH2S %>%
   pivot_longer(cols = -c("date"),
                names_to = "Station",
                values_to = "H2SConc") 

#Importing AirQuality Data for 2020
 setwd("E:/")
 FireDataAMS4 <- read_csv("2020AQData.csv") %>%
   select(-`...1`)


#Forming An H2S conc and fire influence table
setwd("C:/Users/culla/Documents/data")
StationMerge <- StationH2S %>%
  separate(col = date, into = c("date", "hour"), sep = " ") %>%
  selectByDate(year = 2020) %>%
  unite(col = date,
        c("date", "hour"),
        sep = " ") %>%
  select(-`AMS 19`)
setwd("E:\ ")
AQMerge <- rename(FireData, hour = Hour) %>%
  mutate(hour = as.character(hour)) %>%
  unite(col = date,
        c("date", "hour"),
        sep = " ") 

MergedData <- left_join(StationMerge, AQMerge, by = "date") %>%
  pivot_longer(cols = "AMS 4",
               names_to = "Station",
               values_to = "H2SConc") %>%
  select(-"Station")

 
 
 
 
 
 
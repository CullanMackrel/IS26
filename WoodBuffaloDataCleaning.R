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

 
 
#Wood Buffalo Meteorological data cleaning
setwd("C:/Users/culla/Documents/data")
Meteor <- read_csv("WBEAMeteor.csv", skip = 1, n_max = 8761) 

  CleanMeteor <- Meteor %>%
    clean_names() %>%
    separate(col = x1,
             into = c("date", "Hour"),
             sep = " ") %>%
    slice(-1) %>%
    mutate(date = paste(as.character(date), Hour, sep = " "),
           date = as.POSIXct(date, format = "%Y-%m-%d %H", tz = "MST"),
           date = with_tz(date, tz = "UTC")) %>%
    select(-"Hour") %>%
    mutate(across(where(is.character), as.numeric)) %>%
    slice(-(1:4)) %>%
    rename(AT2 = ams_4_2,
           PM2.5 = ams_4_3,
           SO2 = ams_4_4,
           WindDirection = ams_4_5,
           WindSpeed = ams_4_6) %>%
    select(c("date", "AT2", "PM2.5", "SO2", "WindDirection", "WindSpeed")) %>%
    timeAverage(avg.time = "6 hour") %>%
    separate(col = date,
             into = c("date", "Hour"),
             sep = " ") %>%
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
  
# Merging All Data Sets
  
  Merge <- FireDataAMS4 %>%
    unite(col = date,
          c("date", "Hour"),
          sep = " ") %>%
    select(-c("bop", "day")) %>%
    slice(-(1:2)) 
    
   Merge2 <- CleanMeteor 
    
   AirData <- left_join(Merge2, Merge, by = "date") 
  
  Merge3 <- StationMerge %>%
    slice(-(1:2)) %>%
    select("date", "AMS 4")

  AllData <- left_join(Merge3, AirData, by = "date") %>%
    rename(H2SConc = "AMS 4") %>%
    separate(col = date,
             into = c("date", "Hour"),
             sep = " ") %>%
    slice(-(1461:1462))

  
  
  
  
  

 
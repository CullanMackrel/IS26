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
  
 
          
#Plotting With OpenAir
H2SPlot <- LongH2S %>%
  selectByDate(year = 2020) %>%
   timePlot(pollutant = "H2SConc",
   type = "Station",
            lwd = 4, lty = 1,
            group = TRUE) 
timePlot(selectByDate(LongH2S, year = 2020),
         pollutant = "H2SConC",
         type = "Station",
         y.relation = "free",
         avg.time = "month")
 


#Plotting With ggplot
#Different Station's H2S Conc by month in 2020
MonthPlot <- LongH2S %>%
 
mutate(LongH2S, date = as.Date(LongH2S$date)) %>%
timeAverage(avg.time = "month",
            type = "Station")

ggplot(MonthPlot %>% 
         selectByDate(year = 2020),
       aes(x = date, y = H2SConc, colour = Station)) + 
      geom_point() + 
  labs(x = "Months Of 2020", y = "Mixing Ratio (ppb)", title = "H2S Mixing Ratio (ppb) Throughout 2020") +
  scale_x_date(labels = date_format("%b"), breaks = "1 month") +
  theme_classic() 

#Different Station's H2S Conc by month and by year from 2020-2025, Year1 = month, Year2 = year
Year1Plot <- LongH2S %>%
  separate(col = date, into = c("date", "Hour"), sep = " ") %>%
  mutate(LongH2S, date = as.Date(date)) %>%
  timeAverage(avg.time = "month",
              type = "Station")

ggplot(Year1Plot %>%
         selectByDate(year = 2020:2024),
       aes(x = date, y = H2SConc, colour = Station)) +
  labs(x = "Years", 
       y = "Mixing Ratio (ppb)",
       title = "Monthly Average H2S Mixing Ratio (ppb) From 2020-2025") + 
  scale_x_date(labels = date_format("%Y"), breaks = "1 year") +
  geom_point() +
  geom_line() +
  theme_classic()



Year2Plot <- LongH2S %>%
  separate(col = date, into = c("date", "Hour"), sep = " ") %>%
  mutate(LongH2S, date = as.Date(date)) %>%
  timeAverage(avg.time = "year",
              type = "Station")

ggplot(Year2Plot %>%
         selectByDate(year = 2020:2024),
       aes(x = date, y = H2SConc, colour = Station)) +
  labs(x = "Years", 
       y = "Mixing Ratio (ppb)",
       title = "Yearly Average H2S Mixing Ratio (ppb) From 2020-2025") + 
  scale_x_date(labels = date_format("%Y"), breaks = "1 year") +
  geom_point() +
  geom_line() +
  theme_classic()


  

#Violin Plot
ggplot(LongH2S %>%
         selectByDate(year = 2020, month = 7, day = 23),
       aes(x = date, y = H2SConc, fill = Station)) +
  geom_violin() + 
  coord_flip() +
  labs(x = "Station",
       y = " Mixing Ratio (ppb)")
       
 #Marginal Plot
MarginalPlotdata <- LongH2S %>%
  separate(col = date, into = c("date", "Hour"), sep = " ") %>%
  mutate(LongH2S, date = as.Date(LongH2S$date)) %>%
  timeAverage(avg.time = "month",
              type = "Station")
  
MarginalPlot <- ggplot(MarginalPlotdata %>%
         selectByDate(year = 2020),
       aes(x = date, y = H2SConc, colour = Station)) +
  labs(x = "Months", 
       y = "Mixing Ratio (ppb)",
       title = "Monthly Average H2S Mixing Ratio (ppb) in 2020") + 
  scale_x_date(labels = date_format("%b"), breaks = "1 month") +
  geom_point() +
  geom_line() +
  theme_classic() 
  

ggExtra::ggMarginal(MarginalPlot, margins = "y", groupColour = TRUE, groupFill = TRUE) 
 
 
 
 #Plotting with Fire Data

 FireH2S <- LongH2S %>%
   separate(col = date, into = c("date", "hour"), sep = " ") %>%
   mutate(date = as.Date(date)) %>%
   timeAverage(avg.time = "month",
               type = "Station") %>%
   selectByDate(year = 2020) 
   pivot_wider(names_from = Station,
               values_from = H2SConc) %>%
   mutate(FireNumber = FiresWithInfluence$Number_of_Fires) %>%
   pivot_longer(cols = c("AMS 19", "AMS 4", "FireNumber"),
                names_to = "Series",
                values_to = "Values")

 ggplot(FireH2S, aes(y = Values, x = date, colour = Series)) +
   geom_point() +
   geom_line()
 
 
 
 
 
 #Forming An H2S conc and fire influence table
 StationMerge <- StationH2S %>%
   separate(col = date, into = c("date", "hour"), sep = " ") %>%
   selectByDate(year = 2020) %>%
   unite(col = date,
         c("date", "hour"),
         sep = " ")
 AQMerge <- rename(AQ_data, hour = Hour) %>%
   mutate(hour = as.character(hour)) %>%
   unite(col = date,
         c("date", "hour"),
         sep = " ")

 MergedData <- left_join(StationMerge, AQMerge, by = "date") %>%
  pivot_longer(cols = c("AMS 4", "AMS 19"),
               names_to = "Station",
               values_to = "H2SConc")



#Bubble plot with fire influence 
BubblePlot <- MergedData %>%
  separate(col = date, into = c("date", "hour"), sep = " ") %>%
  mutate(date = as.Date(date)) 

 
 ggplot(BubblePlot, aes(x = date, y = H2SConc, size = Fire_Influence, colour = Station)) +
   geom_point(alpha = 0.5)

 
 

 
 
 
 
 
 
 
 
 
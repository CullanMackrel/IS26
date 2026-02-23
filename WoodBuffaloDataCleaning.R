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
#scatter plot
ggplot(LongH2S %>% 
         selectByDate(year = 2020, month = 7, day = 23),
       aes(x = date, y = H2SConc, colour = Station)) + 
      geom_point() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

#Violin Plot
ggplot(LongH2S %>%
         selectByDate(year = 2020, month = 7, day = 23),
       aes(x = date, y = H2SConc, fill = Station)) +
  geom_violin() + 
  coord_flip() +
  labs(x = "Station",
       y = "H2S Concentration (ppb)")
       
 #Marginal Plot
mP <- ggplot(data = LongH2S %>%
         selectByDate(year = 2020, month = 7),
       aes(x = date,
           y = H2SConc,
           colour = Station)) +
  geom_point() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) 

  ggExtra::ggMarginal(mP, margins = "y", groupColour = TRUE, groupFill = TRUE) 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
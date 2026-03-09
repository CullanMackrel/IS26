FireData <- read_csv("2020AQData.csv") %>%
 select(-`...1`)

#Forming An H2S conc and fire influence table
StationMerge <- StationH2S %>%
  separate(col = date, into = c("date", "hour"), sep = " ") %>%
  selectByDate(year = 2020) %>%
  unite(col = date,
        c("date", "hour"),
        sep = " ") %>%
  select(-`AMS 19`)
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



#Bubble plot with fire influence 
BubblePlot <- MergedData %>%
  separate(col = date, into = c("date", "hour"), sep = " ") %>%
  mutate(date = as.Date(date)) 


ggplot(BubblePlot, aes(x = date, y = H2SConc, size = Fire_Influence)) +
  geom_point(alpha = 0.5) +
  labs(x = "2020", y = "H2S Mixing Ratio (ppb)", title = "H2S (ppb) Throughout 2020", size = "Fire Influence") + 
  theme_classic()



#Only plotting Influence = Yes

YesPlot <- MergedData %>%
  separate(col = date, into = c("date", "hour"), sep = " ") %>%
  mutate(date = as.Date(date)) %>%
  filter(Fire_Influence == 'Yes') %>%
  timeAverage(avg.time = "month")

ggplot(YesPlot, aes(x = date, y = H2SConc)) +
  geom_point() +
  labs(x = "2020", y = "H2S Mixing Ratio (ppb)", title = "H2S (ppb) Affected By Wildfires Throughout 2020") +
  geom_line() +
  theme_classic()


#Violin of H2S with influence
ViolinPlot <- MergedData %>%
  separate(date, into = c("date", "hour"), sep = " ") %>%
  mutate(date = as.Date(date)) 

ggplot(ViolinPlot, aes(x = date, y = H2SConc, colour = Fire_Influence)) +
  geom_violin()












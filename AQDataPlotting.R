AQPlot <- AQ_data



#Monthly Average Number of fires with fire influence in 2020
FiresNumber <- AQPlot %>%
  select(date, Number_of_Fires, Fire_Influence) %>%
  mutate(date = as.Date(date)) %>%
  timeAverage(avg.time = "month")

ggplot(FiresNumber, aes(x = date, y = Number_of_Fires)) + 
  geom_point() + 
  geom_line() + 
  labs(x = "Months Of 2020", y = "Number Of Fires", title = "A Monthly Average Of The Number Of Fires in 2020")
  theme_classic()
  
#Same thing with influence
  FiresWithInfluence <- AQPlot %>%
    select(date, Number_of_Fires, Fire_Influence) %>%
    filter(Fire_Influence == "Yes") %>%
    mutate(date = as.Date(date)) %>%
    timeAverage(avg.time = "month")

ggplot(FiresWithInfluence, aes(x = date, y = Number_of_Fires)) + 
  geom_point() +
  geom_line() +
  labs(x = "Months of 2020", y = "Number Of Fires With Influence", title = "A Monthly Average Of The Number Of Fires With Influence In 2020") +
  theme_classic()



  
  









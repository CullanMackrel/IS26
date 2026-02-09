

TFIM <- function(lat, lon, duration, start_date, end_date, met = NULL, satellite = NULL, location = NULL){
  
  #Defining default meteorology, which is based on year
  if(is.null(met) & format(as.POSIXct(strptime(end_date,"%Y/%m/%d",tz="")) ,format = "%Y") <= 2019) {
    met <- "narr"
  } else if (is.null(met) & format(as.POSIXct(strptime(end_date,"%Y/%m/%d",tz="")) ,format = "%Y") > 2019) {
    met <- "nam12"
  }
  
  #Creating output to promt FIRMS update
  if (exists('FIRMS') == FALSE) {
    return("Import FIRMS first!")
  }
  
  #creating dataframe which can be accepted by the HYSPLIT model; includes pre-set 6-hour periods (in UTC)
  day <- seq(as.Date(start_date), as.Date(end_date), by = "days")
  AQ_data <- data.frame(day, "0")
  AQ_data <- AQ_data %>%
    
    mutate("00" = 0,
           
           "06" =0,
           
           "12" = 0,
           
           "18" = 0) 
  AQ_data <- select(AQ_data, -'X.0.')  
  
  AQ_data <-  pivot_longer(AQ_data, cols = c("00", "06", "12", "18"), names_to = "Hour", values_to = "bop") %>%
    
    mutate(date = paste(as.character(day), Hour, sep = " "),
           
           date = as.POSIXct(date, format = "%Y-%m-%d %H")) %>%
    
    timeAverage(avg.time = "6 hour") %>%
    
    mutate(date2 = date) %>%
    separate(col = date, into = c("date", "Hour"),sep = " ")  %>%
    mutate(Hour = replace_na(Hour, "00:00:00"), 
           Hour = case_when(Hour == "00:00:00" ~ 00,
                            Hour == "06:00:00" ~ 06,
                            Hour == "12:00:00" ~ 12,
                            Hour == "18:00:00" ~ 18,
                            Hour == "07:00:00" ~ 06,
                            Hour == "13:00:00" ~ 12,
                            Hour == "19:00:00" ~ 18,
                            Hour == "01:00:00" ~ 00),
           
           day = as.Date(day,"%Y-%m-%d", tz= "UTC")) 
  
           
           Number_of_Fires = as.numeric(""),
           
           FRP = as.numeric(""),
           
           Fire_Influence = as.character(""),
           
           intercept_time = as.numeric(""),
           
           Hour = case_when(Hour == 24~0,
                            
                            Hour == 23~0,
                            
                            Hour == 5~6,
                            
                            Hour == 17~18,
                            
                            Hour == 11~12,
                            
                            Hour == 23~0,
                            
                            Hour %in% c(0, 6, 12, 18)~Hour)) %>%
    select(-date2)
  
  #defining variables for HYSPLIT
  for (row in 1:NROW(AQ_data)){
    for (start_day in AQ_data[row, "day"]){
      start_day = start_day
      start_day2 = start_day - 1
      start_day3 = start_day - 2
    }
    for (start_time in AQ_data[row, "Hour"]){
      start_time = start_time
    }
    
    mainDir <- getwd()
    metDir <- "HYSPLIT"
    
    dir.create(file.path(mainDir, metDir), showWarnings = FALSE)
    
    #HYSPLIT section
    with_dir(paste(getwd(), "/HYSPLIT", sep = ""), trajectory_model <-
               create_trajectory_model() %>%
               add_trajectory_params(
                 lat = lat,
                 lon = lon,
                 height = 0,
                 duration = duration,
                 days = start_day,
                 daily_hours = c(start_time, start_time-1, start_time-2, start_time-3, start_time-4, start_time-5),
                 direction = "backward",
                 met_type = met,
               ) %>%
               run_model()
    )
    trajectory_tbl <- trajectory_model %>% get_output_tbl()
    
    trajectory_tbl <- trajectory_tbl
    
    for (start_daytime in AQ_data[row, "day"]){
      start_daytime = as.POSIXct(start_daytime)
      start_daytime3 = as.POSIXct(start_daytime - 172800)
    }
    
    #Then I need to get the FIRMS data for the correct date bracket
    #Fixed on January 11th so it now includes time constraints to better match FireSmoke data
    FIRMS_date <- FIRMS %>%
      filter(datetime > start_daytime3) %>%
      filter(datetime < start_daytime) %>%
      mutate(Fire_Influence = as.character(""))
    
    
    latmin = min(trajectory_tbl$lat, na.rm = TRUE) - 0.5
    latmax = max(trajectory_tbl$lat, na.rm = TRUE) + 0.5
    lonmin = min(trajectory_tbl$lon, na.rm = TRUE) - 0.5
    lonmax = max(trajectory_tbl$lon, na.rm = TRUE) + 0.5
    
    FIRMS_date <- subset(FIRMS_date, FIRMS_date$latitude < latmax &
                           FIRMS_date$latitude > latmin &
                           FIRMS_date$longitude < lonmax &
                           FIRMS_date$longitude > lonmin)
    
    
    #FIRMS/HYSPLIT interception section
    if (nrow(FIRMS_date)>0){
      
      for (fire in 1:NROW(FIRMS_date)) {
        latmin = as.numeric(FIRMS_date[fire, "latitude"] - 0.5)
        latmax = as.numeric(FIRMS_date[fire, "latitude"] + 0.5)
        lonmin = as.numeric(FIRMS_date[fire, "longitude"] - 0.5)
        lonmax = as.numeric(FIRMS_date[fire, "longitude"] + 0.5)
        
        traj_test <- trajectory_tbl %>%
          filter(between(trajectory_tbl$lat,latmin, latmax),
                 between(trajectory_tbl$lon ,lonmin,lonmax))
        
        if(nrow(traj_test)>0) {
          FIRMS_date[fire, "Fire_Influence"] = "Yes"
          FIRMS_date[fire, "interception"] = mean(traj_test$hour_along)
        } else {
          FIRMS_date[fire, "Fire_Influence"] <- "No"
        }
        
        if (fire == nrow(FIRMS_date)){
          AQ_data[row, "Number_of_Fires"] <- length(which(FIRMS_date$Fire_Influence == 'Yes'))
          AQ_data[row, "FRP"] <- mean(FIRMS_date$frp[FIRMS_date$Fire_Influence=="Yes"])
          AQ_data[row, "intercept_time"] <- mean(FIRMS_date$interception[FIRMS_date$Fire_Influence=="Yes"])
        }
        fire = fire+1
      }
    } else {
      AQ_data[row, "Number_of_Fires"] <- 0
      AQ_data[row, "FRP"] <- NA
      AQ_data[row, "intercept_time"] <- NA
    }
    AQ_data[row, "Fire_Influence"] <- ifelse(AQ_data[row, "Number_of_Fires"] >= 20, "Yes", "No")
    if (row %% 10 == 0) {print(paste(round(row/nrow(AQ_data)*100, 2), "%", sep = " "))}
    row = row + 1
  }
  
  AQ_data$FRP <- round(AQ_data$FRP, digits = 1)
  
  name <- paste(AQ_data[1, "day"], ifelse(is.null(location), "", location), duration, ifelse(is.null(satellite), "", satellite),  sep = "_")
  
  write.csv(AQ_data, paste(name, ".csv", sep = ""))
  return(AQ_data)
}


TFIM_plot <- function(NAPS_ID, date, hour, duration, met = NULL){
  
  #Defining default meteorology, which is based on year
  if(is.null(met) & format(as.POSIXct(strptime(end_date,"%Y/%m/%d",tz="")) ,format = "%Y") <= 2019) {
    met <- "narr"
  } else if (is.null(met) & format(as.POSIXct(strptime(end_date,"%Y/%m/%d",tz="")) ,format = "%Y") > 2019) {
    met <- "nam12"
  }
  
  NAPS_Station <- NAPS()
  
  date <- as.Date(date)
  
  lat = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Latitude"])
  lon = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Longitude"])
  
  #HYSPLIT section
  with_dir(paste(getwd(), "/HYSPLIT", sep = ""), trajectory_model <-
             create_trajectory_model() %>%
             add_trajectory_params(
               lat = lat,
               lon = lon,
               height = 0,
               duration = duration,
               days = date,
               daily_hours = c(hour, hour-1, hour-2, hour-3, hour-4, hour-5),
               direction = "backward",
               met_type = met,
             ) %>%
             run_model()
  )
  trajectory_tbl <- trajectory_model %>% get_output_tbl()
  
  start_daytime = as.POSIXct(date, tz = "UTC")
  start_daytime3 = as.POSIXct(start_daytime - 172800)
  
  #Then I need to get the FIRMS data for the correct date bracket
  #Fixed on January 11th so it now includes time constraints to better match FireSmoke data
  FIRMS_date <- FIRMS %>%
    filter(datetime > start_daytime3) %>%
    filter(datetime < start_daytime) %>%
    mutate(Fire_Influence = as.character(""))
  
  latmin = min(trajectory_tbl$lat, na.rm = TRUE) - 1
  latmax = max(trajectory_tbl$lat, na.rm = TRUE) +1
  lonmin = min(trajectory_tbl$lon, na.rm = TRUE) - 1
  lonmax = max(trajectory_tbl$lon, na.rm = TRUE) + 1
  
  FIRMS_date <- subset(FIRMS_date, FIRMS_date$latitude < latmax &
                         FIRMS_date$latitude > latmin &
                         FIRMS_date$longitude < lonmax &
                         FIRMS_date$longitude > lonmin)
  
  
  #FIRMS/HYSPLIT interception section
  if (nrow(FIRMS_date)>0){
    for (fire in 1:NROW(FIRMS_date)) {
      latmin = as.numeric(FIRMS_date[fire, "latitude"] - 0.5)
      latmax = as.numeric(FIRMS_date[fire, "latitude"] + 0.5)
      lonmin = as.numeric(FIRMS_date[fire, "longitude"] - 0.5)
      lonmax = as.numeric(FIRMS_date[fire, "longitude"] + 0.5)
      
      traj_test <- trajectory_tbl %>%
        filter(between(trajectory_tbl$lat,latmin, latmax),
               between(trajectory_tbl$lon ,lonmin,lonmax))
      
      if(nrow(traj_test)>0) {
        FIRMS_date[fire, "Fire_Influence"] <- "Yes"
      } else {
        FIRMS_date[fire, "Fire_Influence"] <- "No"
      }
      fire = fire+1
    }
  }
  
  print(ggplot(data = trajectory_tbl, aes(x=lon, y=lat)) +
          geom_point(color='black') +
          geom_point(data=FIRMS_date,size=2, aes(x=longitude, y=latitude, color= Fire_Influence)))+
    theme_bw()+
    annotate("point", x = lon, y = lat, colour = "purple", size = 5)
}

Import_FIRMS <- function(instrument, NameofFile = NULL) {
  
  if(is.null(NameofFile)){
    NameofFile <- "FIRMS_data"
  }
  
  if(instrument != "MODIS" & instrument != "VIIRS"){
    print("Please indicate either MODIS or VIIRS")
  }
  
  NameofFile <- as.character(NameofFile)
  
  if (exists('FIRMS') == FALSE) {
    FIRMS <- readr::read_csv(paste(NameofFile, ".csv", sep="")) %>%
      mutate(acq_date = as.Date(acq_date),
             datetime = format(strptime(acq_time, format="%H%M"), format = "%H:%M"),
             datetime = as.POSIXct(paste(acq_date, datetime), format = "%Y-%m-%d %H:%M"))
  }
  
  if (instrument == "MODIS"){
    FIRMS <- FIRMS %>%
      filter(confidence >= 30)
  }
  
  if (instrument == "VIIRS"){
    FIRMS <- FIRMS %>%
      filter(confidence != "L")
  }
  
  FIRMS <<- FIRMS
}


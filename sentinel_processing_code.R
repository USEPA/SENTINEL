
############################################################################################################ SECTION 1
#Read in Pkgs
require(readr)
require(tidyverse)
require(lubridate)
require(tibbletime)
library(quantreg)
require(splines2)  # replaces "splines" in R V3.4.3
require(splines)

require(plyr)
require(tools)
require(zoo)
require(devtools)
require(Matrix)
require(Cairo)
require(ggplot2)
require(dplyr)

#replace with your file path

# Load package - for "getBaseline" and related function
find_rtools()
load_all(
#  "C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/R Code/Shiny app/detrendr"
#)
#source( #be sure to use "getbaseline" from this and not detrendr!
#  "C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/R Code/Shiny app/getBaseline.R"
#)
#source( # If using this, need to edit the ScreenOffScale function so min is 0, sensits are lower-reading than Spod
#  "C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)//G-START Project_QAPP ID_0033295-QP-1-0/R Code/Shiny app/screeningFunctions.R" # edit max to 50
#)

############
  "C:/Users/ETHOMA/OneDrive - Environmental Protection Agency (EPA)//Eben_SENTINEL/Shiny app/detrendr"
)
source( #be sure to use "getbaseline" from this and not detrendr!
  "C:/Users/ETHOMA/OneDrive - Environmental Protection Agency (EPA)//Eben_SENTINEL/Shiny app/getBaseline.R"
)
source( # If using this, need to edit the ScreenOffScale function so min is 0, sensits are lower-reading than Spod
  "C:/Users/ETHOMA/OneDrive - Environmental Protection Agency (EPA)//Eben_SENTINEL/Shiny app/screeningFunctions.R" # edit max to 50
)

############################################################################################################
# USER INPUT BELOW:

# Define Nodes: change to your node S/N here.  The raw data files need to have the node in the filename.
nodes <-
  c( "105", "106", "1037", "1262")

# Define File path to Sensit raw data folder (replace with your file path)
data_folder_raw <-
  #"C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/MM R Code/GSTART/greensboro app MM/Processed_Greensboro_Data/Site01/raw SC data"
  "C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/Raw Data/site_01/"
# Define File path to Sensit folder to store data sets (replace with your file path) for 10 sec output
data_folder_processed_10 <-
  #"C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/MM R Code/GSTART/greensboro app MM/Processed_Greensboro_Data/Site01/10_sec/"
 "C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/Processed Data/10_sec/Site01/"
# Define File path to Sensit folder to store data sets (replace with your file path) for 5 min output
data_folder_processed_5 <-
  #"C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/MM R Code/GSTART/greensboro app MM/Processed_Greensboro_Data/Site01/5_min/"
  "C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/Processed Data/5_min/Site01/"

# Build time frame of dataset (complete)
#replace dates below with the date range to process (can be multiple days/entire dataset)
time_start = ymd('2021-10-26')
time_end = ymd('2022-10-17') #update this one only in shiny version to keep all files read in#
range_days = as.list(seq(
  floor_date(time_start, unit = "days"),
  floor_date(time_end, unit = "days"),
  by = '1 day'
))

# define lat / long and site / node
site <- "S01"
lat <- "36.07155"
long <- "-79.9192"

# site <- "S01A"
# lat <- "36.071835"
# long <- "-79.920215"

# site <- "S02"
# lat <- "36.074282"
# long <- "-79.919830"

# site <- "S03"
# lat <- "36.07733"
# long <- "-79.92113"
# ########################################################################################################

## begin loop here:
#################################################################################################################
#build final 10 sec object dataframe
final10secdf <- data.frame(data.frame(matrix(0, ncol = 32, nrow = 0)))# Read in Data and Parse by node
names(final10secdf)  <- length(c("serial.number", "timestamp", "RawPID_ppb(ppb)" , "RawPID_mV(mV)" , "Temp_(deg_c)" , "RH(%)" ,
  "Pressure(mbar)", "WS(mph)" , "WD(deg)" , "S1_temp(arb)" , "S1_Heat(0-255)" , "S1_Set(arb)" ,
   "Bat_volt(V)" , "Charge_Current_(mA)","Operate_Current_(mA)","Trig_Port_Stat","Trig_Active_Stat","Trig_Active_Flag" ,
  "Trig_Sample_Flag" ,"PID_mV(mV).baseline" ,"PID_ppb(ppb).baseline","BCPID_ppb(ppb)", "BCPID_mV(mV)", "RawPID_BCPID_ppb(ppb)" ,
  "RawPID_BCPID_mV(mV)","u.wind","v.wind", "time",  "date", "lat", "long", "site" ))

#build final 5 min object dataframe
final5mindf <- data.frame(data.frame(matrix(0, ncol = 25, nrow = 0)))# Read in Data and Parse by node
names(final5mindf) <- c("Serial.number","timeCut","rawPID_ppb","rawPID_mv", "bc.pid.ppb", "pid.sd", "MDL",
                          "temp","rh","pressure", "ws","wd", "s1temp", "s1heat",  "set","bat_volt",
                          "chg.current","opp.current","trigportstat","trigactivestat","trigactiveflag", "trigsampleflag", "lat", "long", "site" )
##################################################################################################################
# build 10 sec data frame
#read in data
#rename cols
for (dateI in range_days) {
  for (node in nodes) {
    filename <-
      paste0(data_folder_raw,
             "/SPOD_Data_Export_",
             node,
             "_",
             dateI,
             ".csv") #this node must match node above exactly!
    if (file.exists(filename)) {
      Data_sensit <- read_csv(
        filename,
        col_names = FALSE,
        col_types = cols(X2 = col_datetime(format = "%d-%b-%Y %H:%M:%S")),
        skip = 3,
      )
      Data_sensit <-
        Data_sensit[, c(2:19)] #limit to needed cols
      names(Data_sensit) <-
        c(
          "time",
          "RawPID_ppb(ppb)",
          "RawPID_mV(mV)",
          "Temp_(deg_c)",
          "RH(%)",
          "Pressure(mbar)",
          "WS(mph)",
          "WD(deg)",
          "S1_temp(arb)",
          "S1_Heat(0-255)",
          "S1_Set(arb)",
          "Bat_volt(V)",
          "Charge_Current_(mA)",
          "Operate_Current_(mA)",
          "Trig_Port_Stat",
          "Trig_Active_Stat",
          "Trig_Active_Flag",
          "Trig_Sample_Flag"
        ) #rename cols
      Data_sensit$serial.number <- paste0("SPOD", node)

      if (class(Data_sensit$`RawPID_ppb(ppb)`) != "numeric") {
        Data_sensit$`RawPID_ppb(ppb)` <-
          as.numeric(as.character(Data_sensit$`RawPID_ppb(ppb)`))
      }

      if (class(Data_sensit$`RH(%)`) != "numeric") {
        Data_sensit$`RH(%)` <- as.numeric(as.character(Data_sensit$`RH(%)`))
      }
      #if(length(which(!is.na(Data_sensit[,pid])))>0){
      # Data_sensit[,pid] <- screenOffScale(Data_sensit[,pid], Data_sensit$time) (not needed for sensit unless it appears to be railing)
      # if (length(which(!is.na(Data_sensit$`RawPID_ppb(ppb)`))) > 0) {
      #   # screen out large RH
      #   Data_sensit$`RawPID_ppb(ppb)` <-
      #     screenRH(Data_sensit$`RawPID_ppb(ppb)`,
      #              Data_sensit$time,
      #              Data_sensit$`RH(%)`)
      # }

      ############# Calculate and remove baseline for ppb and M
      Data_sensit_10 <-
        as.data.frame(Data_sensit) #create new df to use here
      Data_sensit_10$time <- as.POSIXct(Data_sensit_10$time)
      #mv space baseline correction # RawPID-BCmV mV (mV)
      Data_sensit_10[, 'PID_mV(mV).baseline'] <-
        getBaseline(Data_sensit_10[3], Data_sensit_10$time, df = 6)
      #ppb space baseline correction # RawPID-BCmV mV (mV)
      Data_sensit_10[, 'PID_ppb(ppb).baseline'] <-
        getBaseline(Data_sensit_10[2], Data_sensit_10$time, df = 6)
      # subtract baseline from raw for corrected dataset
      Data_sensit_10[, 'BCPID_ppb(ppb)'] <-
        (Data_sensit_10[, 'RawPID_ppb(ppb)'] - Data_sensit_10[, 'PID_ppb(ppb).baseline'])
      # subtract baseline from raw for corrected dataset
      Data_sensit_10[, 'BCPID_mV(mV)'] <-
        (Data_sensit_10[, 'RawPID_mV(mV)'] - Data_sensit_10[, 'PID_mV(mV).baseline'])
      # turn back into time tibble for merging
      Data_sensit_10 <-
        Data_sensit_10 %>% as_tbl_time(time)
      ############# Create Intermediate Processing file with additional calculations (runs on 1 day of data)
      # raw PID subtracted by background correction to the ppbv channel (non-standard BC, DF 24 default)
      Data_sensit_10[, 'RawPID_BCPID_ppb(ppb)'] <-
        Data_sensit_10[, 'RawPID_ppb(ppb)']  - Data_sensit_10[, 'BCPID_ppb(ppb)']
      # raw PID subtracted by background correction to the mv channel (standard BC, DF 24 default)
      Data_sensit_10[, 'RawPID_BCPID_mV(mV)'] <-
        Data_sensit_10[, 'RawPID_mV(mV)']  - Data_sensit_10[, 'BCPID_mV(mV)']
      # Calculate the u and v wind components
      Data_sensit_10[,'u.wind'] <- Data_sensit_10[, 'WS(mph)'] * sin(2 * pi * Data_sensit_10[, 'WD(deg)']/360)
      Data_sensit_10[,'v.wind'] <- Data_sensit_10[, 'WS(mph)'] * cos(2 * pi * Data_sensit_10[, 'WD(deg)']/360)

      Data_sensit_10$timestamp <- Data_sensit_10$time
      Data_sensit_10[, 'date'] <-
        as.Date(Data_sensit_10$time, format =  "%Y-%m-%d")
      Data_sensit_10[, 'time'] <-
        format(Data_sensit_10$time, "%H:%M:%S")
      #check, object created for R of all nodes/days
      Data_sensit_10 <- as.data.frame(Data_sensit_10)
      Data_sensit_10 <-
        Data_sensit_10[, c(19, 28, 2:18, 20:27, 1, 29)]
      Data_sensit_10$lat <- lat
      Data_sensit_10$long <- long
      Data_sensit_10$site <- site

      #round to even 10 sec limits
      timeBase <- "10 sec"
      timeBreaks <- seq(round(min(Data_sensit_10$timestamp, na.rm = T), "min"),
                        round(max(Data_sensit_10$timestamp, na.rm = T), "min"), timeBase)
      Data_sensit_10$timestamp <- cut(Data_sensit_10$timestamp, timeBreaks)
      #Write file
      write.csv(
        Data_sensit_10,
        file = paste0(
          data_folder_processed_10,
          "(10sec)_",
          dateI,
          "_",
          node, "DF6",
          ".csv"
        ), row.names=FALSE
      )


      ## save visual in project folder
      date_name <- as.character(Data_sensit_10$date[1])
      spod_name <- as.character(Data_sensit_10$serial.number[1])
      plotpath <- paste("C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/Processed Data/plots/",
                          spod_name,"_", date_name, ".png", sep = "")
      png(file = plotpath)
       x <- ggplot(Data_sensit_10, aes(x = as.POSIXct(Data_sensit_10$timestamp), y = `RawPID_ppb(ppb)`))+
        geom_line()+ geom_line(aes( y = `PID_ppb(ppb).baseline`), color = "red")
       print(x)
      dev.off()


      # add df to complete dataframe object
      final10secdf <- rbind(Data_sensit_10, final10secdf)
      final10secdf <- arrange(final10secdf, timestamp)
      #


      #
####### 5 minute analysis
      S <- Data_sensit_10[, c(1:19, 22, 23, 26, 27)]
      names(S) <-
        c(
          "SN",
          "time",
          "rawPID_ppb",
          "RawPID_mv",
          "temp",
          "rh",
          "pressure",
          "ws",
          "wd",
          "s1temp",
          "s1heat",
          "set",
          "bat_volt",
          "chg.current",
          "opp.current",
          "trigportstat",
          "trigactivestat",
          "trigactiveflag",
          "trigsampleflag",
          "BCPID_ppb",
          "BCPID_mv",
          "u.wind",
          "v.wind"
        )
      S$time <- as.POSIXct(S$time, format =  "%Y-%m-%d %H:%M:%S")
      # Aggregate to 5 minutes for 5 min data:
      # create 5 min col
      timeBase <- "5 min"
      timeBreaks <- seq(round(min(S$time, na.rm = T), "hour"),
                        round(max(S$time, na.rm = T), "hour"), timeBase)
      S$timeCut <- cut(S$time, timeBreaks)
      # calculate median of corrected.pid signal by node
      # for each node
      nodeMedian <- S %>%
        dplyr::group_by(SN) %>%
        dplyr::summarise(pid.median = quantile(S$BCPID_ppb, .5, na.rm = T))
      # merge pid median with df
      S_median <- merge(S, nodeMedian, by = "SN")
      # Plot corrected pid with threshold set at 4 times the median
      p <- ggplot(S_median, aes(y = BCPID_ppb, x = time, group = SN)) +
        geom_line() +
        geom_line(aes(y = 4 * pid.median), col = "red")
      p
      # define signal functions
      detectSignal <- function(pid, threshold) {
        length(which(pid > threshold)) #count pid values above threshbold (5 * pid.sd)
      }
      sdFunc <- function(y, q) {
        sd(y[y < q], na.rm = TRUE) # take sd of y where y
      }
      # define noise sd as threshold:
      noiseSD <-  S_median %>%
        dplyr::group_by(SN) %>%
        dplyr::summarise(pid.sd = sdFunc(BCPID_ppb, 4 * pid.median))
      # merge noise SD with df
      S_median_sd <- merge(S_median, noiseSD, by = "SN")


      # aggregate to 5 min, set detects to 5 times noise SD for that node for that day
      Data_sensit_5 <- S_median_sd %>%
        dplyr::group_by(timeCut) %>%
        dplyr::summarize(
          bc.pid.ppb = mean(BCPID_ppb),
          pid.sd = sd(BCPID_ppb, na.rm = TRUE),
          rawPID_ppb = mean(rawPID_ppb),
          rawPID_mv = mean(RawPID_mv),
          temp = mean(temp),
          rh = mean(rh),
          pressure = mean(pressure),
          u.wind = mean(u.wind),
          v.wind = mean(v.wind),
          s1temp = mean(s1temp),
          s1heat = mean(s1heat),
          set = mean(set),
          bat_volt = mean(bat_volt),
          chg.current = mean(chg.current),
          opp.current = mean(opp.current),
          trigportstat = length(which(trigportstat != 0)),
          trigactivestat = length(which(trigactivestat != 0)),
          trigactiveflag = length(which(trigactiveflag != 0)),
          trigsampleflag = length(which(trigsampleflag != 0))
        )
      # add daily median sd MDL column
      Data_sensit_5$MDL <- 3 * median(Data_sensit_5$pid.sd)
      #add serial number
      Data_sensit_5$Serial.number <- S_median_sd[1, 1]
      #revert wind back to ws and wd
      Data_sensit_5$wd <- atan2(-Data_sensit_5$u.wind, -Data_sensit_5$v.wind)*180/pi + 180
      Data_sensit_5$ws <- sqrt(Data_sensit_5$u.wind^2 + Data_sensit_5$v.wind^2)

      #reorganize cols
      # Data_sensit_5 <- Data_sensit_5[, c(24, 1, 6, 7, 2:5, 23, 8:12, 25, 26, 13:22)]
       Data_sensit_5 <- Data_sensit_5[, c(22, 1,4,5,2,3, 21, 6:10, 23:24,11:20 )]

      Data_sensit_5$lat <- lat
      Data_sensit_5$long <- long
      Data_sensit_5$site <- site
      #write to csv - this gets used in 24 hr plot package
      write.csv(
        Data_sensit_5,
        paste0(
          data_folder_processed_5,
          "5min_",
          dateI,
          "_",
          node,
          ".csv"
        ), row.names = FALSE
      )
      # add vector to a dataframe
      final5mindf <- rbind(Data_sensit_5, final5mindf)
     # final5mindf <- arrange(final5mindf, timestamp)

    }
  }
}


#View(final5mindf)
#################################################################################################################








############################################################################################################ SECTION 2
# create RDS object of  5 min data
setwd("C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/Processed Data/5_min")
path5 <-  "C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/Processed Data/5_min"
file_list <-
  list.files(
    path = path5,recursive = TRUE)
data_list <- vector("list", "length" = length(file_list))
for (i in seq_along(file_list)) {
  filename = file_list[[i]]
  Data_spod <-read_csv(filename)
  names(Data_spod) <- c( "SN",
                         "timeCut",
                         "rawPID_ppb",
                         "rawPID_mv",
                         "bcPID_ppb",
                         "pid.sd",
                         "MDL",
                         "temp",
                         "rh",
                         "pressure",
                         "u.wind",
                         "v.wind",
                         "wd",
                         "ws",
                         "s1temp",
                         "s1heat",
                         "set",
                         "bat_volt",
                         "chg.current",
                         "opp.current",
                         "trigportstat",
                         "trigactivestat",
                         "trigactiveflag",
                         "trigsampleflag",
                         "lat",
                         "long",
                         "site"
                        )
  # Data_spod$timeCut <-as.POSIXct(Data_spod$timeCut, tz = "America/New_York") #format DateTime
  Data_spod$day <-as.Date(Data_spod$timeCut, format = "%Y-%m-%d")
  data_list[[i]] <- Data_spod
}
spod_all <- do.call(rbind, data_list)
# spod_all <- spod_all %>%
#   mutate_if(is.numeric, round, digits = 1)
spod_all <- subset(spod_all, ws < 10)
spod_all$SN <- as.character(spod_all$SN)
spod_all$timeCut <- as.character(spod_all$timeCut)
spod_all$signal <- ifelse(spod_all$bcPID_ppb > spod_all$MDL, 1, 0)

## create blank QA file 
df <- spod_all[,c(1:3)]
write.csv(df, "C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/Processed Data/RDSfiles/QA_rdf1.csv")
### manual QA entry, read back in
QA <- read_csv("C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/Processed Data/RDSfiles/QA_rdf1.csv", 
               col_types = cols(timeCut = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                rawPID_ppb = col_skip()))
QA$QA[is.na(QA$QA)] <- 0
QA <- QA[!is.na(QA$timeCut),]

spod_all$timeCut <- as.POSIXct(spod_all$timeCut)
spod_all <- spod_all[!is.na(spod_all$timeCut),]
complete_spod <- inner_join(spod_all, QA, by = c('timeCut', "SN"))
View(QA)
View(complete_spod)

saveRDS(complete_spod, file = "C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/Processed Data/RDSfiles/5min_greensboro.rds")

################################################################################################################# 






################################################################################################################# SECTION 3
### make QA table for 5 min data

## QA Code:
# 0 = no events during this period
# 1 = canister collected
# 2 = calibration/cal check
# 3 = deployment
# 4 = maintenance
# 5 = malfunction
# 6 = wind obstruction

spod_all$QA <- ifelse(spod_all$site == "S03" &
                        spod_all$wd > 300 | spod_all$wd < 120, 6, 0)
spod_all$QA <- ifelse(spod_all$site == "S01" &
                        spod_all$day < "2022-11-01", 3, 0)
spod_all$QA <- ifelse(spod_all$SN == "106" &
                        spod_all$timeCut < "2022-06-30 19:45:00", 5, 0)
spod_all$QA <- ifelse(spod_all$SN == "105" &
                        spod_all$timeCut < "2022-07-11 11:35:00", 5, 0)
spod_all$QA <- ifelse(spod_all$SN == "105" &
                        spod_all$timeCut < "2022-07-21 09:17:00", 2, 0)
spod_all$QA <- ifelse(spod_all$SN == "1262" &
                        spod_all$timeCut < "2022-07-21 09:20:00", 2, 0)

spod_qa <- spod_all[, c(1,2,29,32)]

write.csv("GSTART_QA.csv", spod_qa)



#################################################################################################################


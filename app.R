##########################################################

#                 S  E  N  T  I  N  E  L                 #
#      Sensor Network Intelligent Emissions Locator      #

##########################################################

# M.K. MacDonald -//- ORD/CEMM/AMCD/SFSB -//- macdonald.megan@epa.gov

# Rev. 1.1: January

# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.

# Note this software is not yet in final form. Please direct any questions or bug reports to the email above.

##########################################################
options(install.packages.check.source = "no")

# install pkgs
library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(janitor)
library(shinydashboard)  
library(shinycssloaders)
library(rhandsontable)
library(data.table) 
library(stringr)  
library(rhandsontable)
library(leaflet)  
library(openair)
library(lattice)
library(plotly)  
library(tidyverse)
library(lubridate)  
library(ggpubr)

library(tinytex)
# tinytex::install_tinytex()  ## make sure this is run the first time running the app
library(knitr)
library(kableExtra)
library(tidyverse)
library(rmarkdown)

library(htmltools)
library(devtools)  
#install_github("davidcarslaw/openairmaps", force = TRUE)  ## make sure this is run the first time running the app
library(openairmaps)  

# baseline Functions
library(quantreg)  
library(splines2)  
library(splines)  
library(zoo)
source("getBaseline.R") ## found in app folder

options(scipen=999)  # turn off scientific notation in app 












########################################################### app starts here:

options(shiny.maxRequestSize=30*1024^2)


# User Interface Build

ui <- dashboardPage( ###################################################### build sidebar
  skin = "black", 
  dashboardHeader(title = "SENTINEL"),
  dashboardSidebar(
    sidebarMenu(
      tags$a(img(src="ngemlogo.png", width = 125,
                 style="display: block; margin-left: auto; margin-right: auto;"),
             href="https://www.epa.gov/air-research/next-generation-emission-measurement-ngem-research-fugitive-air-pollution"),
      menuItem(
        "Data Upload",
        tabName = "DataUpload",
        icon = icon("fas fa-turn-up")
      ),
      menuItem(
        "Data Check",
        tabName = "DataCheck",
        icon = icon("fas fa-check")
      ),
      menuItem( # make side bar menu items
        "Dashboard",
        tabName = "Dashboard",
        icon = icon("fas fa-house")
      ),
      menuItem( # make side bar menu items
        "Data Table",
        tabName = "DataTable",
        icon = icon("fas fa-list")
      ),
      menuItem(
        "QA Tables",
        tabName = "QA Tables",
        icon = icon("fas fa-wrench"),
        menuSubItem('Single Node',
                    tabName = 'singlenode',
                    icon = icon('fas fa-chevron-right')),
        menuSubItem('Collocated Nodes',
                    tabName = 'multinode',
                    icon = icon('fas fa-chevron-right'))
      ),
      menuItem(
        "About",
        tabName = "about",
        icon = icon("fas fa-heart")
      )
    )
  ),
  dashboardBody( ################################################## build main page of app
    
    # tags$head(tags$style(HTML('  /* body */
    #                             .content-wrapper, .right-side {
    #                             background-color: #FFFFFF;
    #                             }
    #                             '))),
    setBackgroundColor("ghostwhite"),
    
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 25px; }")) #change the font size to 20
    ),
    
    tabItems(
      # DATA UPLOAD PAGE  -------------------------------------------------------
      tabItem(tabName = "DataUpload", 
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: visible; content: 'Timestamp,Sensor ID, and Signal 1 must be identified to generate Table '; }"
              ),
              # h4("Upload one or more data files from any number of sensors of the same type in .csv format. 
              #    Files should all have the same structure but can be from different unit IDs. 
              #    Use column naming tool to indicate which columns are present. Sensor ID, Timestamp, and Signal are required. "),
              fluidRow(style = "background-color:ghostwhite;",
                       column(6, 
                              #upload files
                              fileInput("files",
                                        label = h4("Upload"),
                                        multiple = TRUE)
                       ),
                       column(6, 
                              br(),
                              br(),
                              downloadButton('saveQAfile',"Download Compiled Data"),
                              radioButtons(
                               "skip_val",
                               "Header lines to skip?",
                                choices = c(0,1,2,3),
                                selected = 0,
                                inline = TRUE
                              
                              )
                              
                       )
              ),
              br(),
              fluidRow(style = "background-color:ghostwhite;",
                       column(4, 
                              tabsetPanel(
                                #Required Cols
                                tabPanel(
                                  h4("ID/Time"),
                                  uiOutput("ID_column"),
                                  uiOutput("Time_column"),
                                  uiOutput("Time_Zone"),
                                  selectInput("Time_format", "Time Format", c("%d-%b-%Y %H:%M:%S","%Y-%m-%d %H:%M:%S","%m-%d-%Y %H:%M:%S","%d-%m-%Y %H:%M:%S","%m/%d/%y %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M"), selected = "%d-%b-%Y %H:%M:%S"),
                                  
                                ),
                                tabPanel(
                                  h4("Signal"),
                                  uiOutput("Signal_column"),
                                  selectInput("Signal_units", "Signal Units?", c("ppm", "ppb")) 
                                ),
                                tabPanel(
                                  h4("Met"),
                                  uiOutput("WS_column"),
                                  selectInput("WS_units", "WS Units?", c("mph" = 2.237, "m/s" = 1), selected = 1),
                                  uiOutput("WD_column"),
                                  #numericInput("WD_offset_val", label = h5("Add Wind Direction offset?"), value = 0)
                                ),
                                # Optional Cols
                                tabPanel(
                                  h4("Other"),
                                  uiOutput("Temp_column"),
                                  selectInput("Temp_units", "Temperature Units?", c("C", "F")),
                                  uiOutput("RH_column"),
                                  uiOutput("Lat_column"),
                                  uiOutput("Long_column"), 
                                  uiOutput("Can_column")
                                )
                              )
                       ),
                       
                       column(8, style = "background-color:ghostwhite;",
                              #display df
                              # fluidRow(column(align = "center", width = 12, DT::dataTableOutput("table", width = 700)))
                              rHandsontableOutput("upload_table")
                       )
                       
              )
              
      ),
      
      # DATA CHECK PAGE ---------------------------------------------------------
      tabItem(tabName = "DataCheck", 
              h3("Data Check"),
              br(),
              # summary table showing canisters/QA/day ranges of spod units and lat/long
              fluidRow(column(align = "center", width = 12, withSpinner(DT::dataTableOutput("summarytable", width = 900))))
              
              
      ),
      
      # DASHBOARD PAGE ----------------------------------------------------------
      tabItem(tabName = "Dashboard", 
              
              fluidRow(
                column(9, 
                       uiOutput("unitselect")
                       
                ),
                column(2, 
                       radioButtons("ws_select", label = "Remove low WS?", 
                                    choices = c("All Data", ">1 m/s"),
                                    selected = "All Data", inline=TRUE)
                ),
                column(1, 
                       downloadButton("report", "Export"),
                )
              ),
              fluidRow(  ## SDI row
                tabBox( ## signal map 
                  title = "Signal Map",side = "right",  ### Signal Map Box
                  tabPanel(h4("Graph"),
                           fluidRow(leafletOutput("polarmap", height = "550px") %>% withSpinner(color = "#0dc5c1"),  height = "550px"),
                           textOutput("latlongtext"),
                  ),
                  tabPanel(h4("Controls"), "Controls",
                           sliderInput(
                             "windfilterInput",
                             label = h4("Wind Speed Filter:"),
                             min = 0,
                             max = 12,
                             value = c(0,7)
                           ),
                           selectInput(
                             "statselect",
                             label = h4("Select Stat:"),
                             choices = list(
                               "Median" = "median",
                               "Weighted Mean" = "weighted.mean",
                               "Maximum" = "max", "nwr" = "nwr"
                             ),
                             selected = "median"
                           ),
                           h6("For less than 200 data points, selct 'nwr' "),
                           height = "500px"
                  ), height = "500px"
                ),
                tabBox(# SDI plots box
                  title = "SDI plots", side = "right",   ### SDI plots box
                  tabPanel(h4("SDI"),
                           fluidRow(
                             box(
                               selectInput(
                                 "statselectSDI",
                                 label = h4("Select SDI Stat:"),
                                 choices = list(
                                   "Median" = "median",
                                   "Weighted Mean" = "weighted.mean",
                                   "Mean" = "mean",
                                   "Maximum" = "max", "nwr" = "nwr"
                                 ),
                                 selected = "median"
                               ),
                               plotOutput("SDI")%>% withSpinner(color="#0dc5c1"),
                               height = "550px", width = "600px")), height = "550px", width = "600px"
                  ),
                  tabPanel(h4("Freq"), #frequency plot
                           fluidRow(
                             box(
                               selectInput(
                                 "statselectFREQ",
                                 label = h4("Select Frequency Stat:"),
                                 choices = list(
                                   "Median" = "median",
                                   "Weighted Mean" = "weighted.mean",
                                   "Mean" = "mean",
                                   "Maximum" = "max"
                                 ),
                                 selected = "median"
                               ),
                               plotOutput("FREQ") %>% withSpinner(color="#0dc5c1"),
                               height = "550px", width = "600px")), height = "550px", width = "600px"
                  ),
                  tabPanel(h4("Wind Rose"), #Wind Rose plot
                           fluidRow(
                             box(plotOutput("WR") %>% withSpinner(color = "#0dc5c1"),
                                 height = "550px", width = "600px")), height = "550px", width = "600px")),
                                 height = "550px", width = "600px"
              ),
              br(),
              br(),
              fluidRow( ### Time Series plots box
                tabBox(
                  title = "Time Series",side = "right",
                  tabPanel(h4("Baseline"), h4("Baseline Fit: Raw signal trace (5 minute values) plotted in black with baseline fit (df = 10) plotted in red"),
                           plotlyOutput("BCplot")),
                  tabPanel(h4("WD"), h4("Wind Direction: Baseline corrected signal trace (5 minute values) plotted in black with wind direction plotted in green (5 minute values)"),
                           plotlyOutput("windplot")),
                  tabPanel(h4("WS"), h4("Wind Speed: Baseline corrected signal trace (5 minute values) plotted in black with wind direction plotted in gray (5 minute values)"),
                           plotlyOutput("windspeedplot")),
                  tabPanel(h4("Calibrations"), h4("Calibrations: Baseline corrected signal trace (5 minute values) plotted in black with user-reported calibration periods plotted as points, if collected during this time frame"),
                           plotlyOutput("CALplot")),
                  tabPanel(h4("RH"), h4("RH: Baseline corrected signal trace (5 minute values) plotted in black with Relative Humidity (%) plotted in purple"),
                           plotlyOutput("RHplot")),
                  tabPanel(h4("Temp"), h4("Temperature: Baseline corrected signal trace (5 minute values) plotted in black with temperature (deg C) plotted in blue"),
                           plotlyOutput("Tplot")),
                  tabPanel(h4("Triggers"), h4("Canister Triggers: Baseline corrected signal trace (5 minute values) plotted in black with canister triggers plotted as points, if collected during this time frame"),
                           plotlyOutput("canplot")), 
                  width = 12
                ), width = 12
              ), 
              
      ),
      # DATA TABLE PAGE ---------------------------------------------------------
      tabItem( 
        tabName = "DataTable", 
        h4("Table Results"),
        br(),
        downloadButton('Download',"Download .CSV data"),
        br(),
        br(),
        DTOutput(outputId = "datatab", width = 1000)
      ),
      # CAL TABLE PAGE ----------------------------------------------------------
      tabItem(
        tabName = "singlenode",
        h2("QA Table for 1 Unit"),
        br(),
        uiOutput("calunitselect"),   
        uiOutput("singlenodestarttime"),
        uiOutput("singlenodeendtime"),
        br(),
        radioButtons("durationInput", h4("Select length of QA frame:"),
                     choices = c("1 min" = 60, "1 hour" = 3600, "1 day" = 86400),
                     selected = "1 min"),
        radioButtons("freqfile", h4("Select Frequency of sensor values:"),
                     choices = c("10 sec" = 10,
                                 "30 sec" = 30,
                                 "1 min" = 60),
                     selected = "10 sec"),
        downloadButton("singlenodereport", "Generate report"),
        box(tableOutput("draw_caltab"), width = 12)), 
      ### add small plotly graph???
      
      # COLLOCATED CAL TABLE PAGE --------------------------------------------
      tabItem(tabName = "multinode",
              h2("Sensor Agreement"),
              h5("QA Table for 2 co-located units during same time frame"),
              br(),
              br(),
              fluidRow(
                column(6, 
                       uiOutput("calunitselect1"),   
                       uiOutput("calunitselect2")
                       ),
                column(6,
                       uiOutput("multinodestarttime"),
                       uiOutput("multinodeendtime"),
                       downloadButton("multinodereport", "Generate report")
                       )
              ),
              box(tableOutput("draw_subcaltab"), width = 12),
              box(plotlyOutput("SensorAgreement_buildplot"), width = 12)
              
      ),
      
      # ABOUT PAGE --------------------------------------------------------------
      tabItem(tabName = "about",
              fluidRow( width = 12,
                        box( width = 12,
                             br(),
                             HTML("<h3> <b>SENTINEL:</b> An application for automated fenceline sensor data analysis </h3>"),
                             br(),
                             h4(div(em("Purpose:"))),
                             br(),
                             h4("There is growing interest in fenceline monitoring around chemical facilities. 
             Fenceline sensors used in these monitoring applications can collect large amounts 
             of concentration and meteorological data for extended time periods. The SENsor 
             InTellIgeNt Emissions Locator (SENTINEL) application helps users compile, process,
             and analyze data from fenceline sensors. This application delivers these 
             capabilities in a user-friendly interface that can combine and process daily data
             files from multi-sensor deployments, allowing users to gain insights from compiled
             sensor data over time. The SENTINEL app is one of the technologies developed under
             the Next Generation Emissions Measurements (NGEM) program. We awknowledge contributions
             from past and present contributors to this software: Halley Brantley, Yadong Xu,
             Wei Tang, and Gustavo Quieroz."),
                             br()
                        )
              ),
              
              
              fluidRow(
                box( width = 12,
                     h4("Version 1.0 (Aug 2024)"),
                     br(),
                     h4("Contact:"),
                     h4("macdonald.megan@epa.gov"),
                     br(),
                     #actionButton("pdf", "SENTINEL User Guide",class = "btn-success", class = "btn-lg", onclick = "window.open('SENTINEL Shiny App User Guide V1.pdf')"),
                     br()
                )
              )
              
      )
    )
  )
)

# End of UI build






# Start of server build

server <- function(input, output) {
  
  options(shiny.maxRequestSize=60*1024^4)
  
  # Data Upload Page Functions ----------------------------------------------
  
  
  # read in files
  test <- reactive({
    req(input$files)
    req(input$skip_val)
    inFile <- input$files
    skip_num <- as.numeric(input$skip_val)
    print(skip_num)
    if (is.null(inFile)){
      return(NULL)
    } else {
      numfiles = nrow(inFile)
      filelist = list()
      for (i in 1:numfiles)
      { # check for SENSIT SPod data which does not carry a sensor ID col. 
        #read in data file
        Data <- fread(input$files[[i, 'datapath']], skip = skip_num) # will have to edit to "skip to usable dat" somehow ...   EDITED##############
        #check for Sensit Connect data, which does not carry the Sensor Id col
        print(Data)
        filename <- input$files$name[[i]]
        print(filename)
        Data$spod_check <- ifelse(grepl("SPOD_Data_Export", filename, fixed = TRUE) == TRUE, str_match(filename, "SPOD_Data_Export_\\s*(.*?)\\s*_")[,2], "0" )
        print(str_match(filename, "SPOD_Data_Export_\\s*(.*?)\\s*_")[,2])
        # Roll up data
        filelist[[i]] <- Data
      }
      #do.call(rbind, filelist)
      #plyr:::rbind.fill(lapply(x,function(y){as.data.frame(t(filelist),stringsAsFactors=FALSE)}))
      data.table::rbindlist(filelist, fill = TRUE)
      
      
    }
  })
  
  ##creating dropdowns
  output$ID_column <- renderUI({
    req(input$files)
    test_df <- test()
    selectizeInput("ID_column",
                   "Sensor ID:",
                   c(names(test_df), "NA"), 
                   selected= "spod_check")
  })
  
  output$Time_column <- renderUI({
    req(input$files)
    test_df <- test()
    selectizeInput("Time_column",
                   "Date/Time stamp:",
                   c(names(test_df), "NA"), 
                   selected=ifelse(any(names(test_df) == 'Local Date Time'),'Local Date Time', 'NA'))
  })
  
  output$Time_Zone <- renderUI({
    req(input$files)
    selectizeInput("Time_Zone",
                   "Select Time Zone (choose local time for SPods):",
                   c( "UTC", "America/New_York", "America/Chicago", "America/Denver", "America/Phoenix", "America/Los_Angeles", "America/Anchorage"), 
                   selected="America/New_York")
  })
  
  
  output$Signal_column <- renderUI({
    req(input$files)
    test_df <- test()
    selectizeInput("Signal_column",
                   "Sensor Signal 1:",
                   c(names(test_df), "NA"), 
                   selected=ifelse(any(names(test_df) == 'pid1_PPB_Calc'),'pid1_PPB_Calc', 'NA'))
  })
  
  output$WS_column <- renderUI({
    req(input$files)
    test_df <- test()
    selectizeInput("WS_column",
                   "Wind Speed:",
                   c(names(test_df), "NA"), 
                   selected=ifelse(any(names(test_df) == 'ws_speed'),'ws_speed', 'NA'))
  })
  output$WD_column <- renderUI({
    req(input$files)
    test_df <- test()
    selectizeInput("WD_column",
                   "Wind Direction (deg):",
                   c(names(test_df), "NA"),  
                   selected=ifelse(any(names(test_df) == 'ws_direction'),'ws_direction', 'NA'))
  })
  
  output$Temp_column <- renderUI({
    req(input$files)
    test_df <- test()
    selectizeInput("Temp_column",
                   "Temperature:",
                   c(names(test_df), "NA"),  
                   selected=ifelse(any(names(test_df) == 'temp'),'temp', 'NA'))
  })
  
  output$RH_column <- renderUI({
    req(input$files)
    test_df <- test()
    selectizeInput("RH_column",
                   "Relative Humidity (%):",
                   c(names(test_df), "NA"), 
                   selected=ifelse(any(names(test_df) == 'rh_Humd'),'rh_Humd', 'NA'))
  })
  
  output$Lat_column <- renderUI({
    req(input$files)
    test_df <- test()
    selectizeInput("Lat_column",
                   "Latitude (decimal):",
                   c(names(test_df), "NA"), 
                   selected=ifelse(any(names(test_df) == 'lat'),'lat', 'NA'))
  })
   
  output$Long_column <- renderUI({
    req(input$files)
    test_df <- test()
    selectizeInput("Long_column",
                   "Longitude (decimal):",
                   c(names(test_df), "NA"), 
                   selected=ifelse(any(names(test_df) == 'long'),'long', 'NA'))
  })
  
  output$Can_column <- renderUI({
    req(input$files)
    test_df <- test()
    selectizeInput("Can_column",
                   "Active Canister trigger:",
                   c(names(test_df), "NA"), 
                   selected=ifelse(any(names(test_df) == 'trig.trig_activeFlag'),'trig.trig_activeFlag', 'NA'))
  })
  
  
  
  # build DF to recieve new col names
  df_new <- reactive({
    req(input$files)
    
    old_signal_name <- input$Signal_column
    old_sensor_id_name <- input$ID_column
    old_WS_name <- input$WS_column
    old_WD_name <- input$WD_column
    old_Temp_name <- input$Temp_column
    old_RH_name <- input$RH_column
    old_Lat_name <- input$Lat_column
    old_Long_name <- input$Long_column
    old_Can_name <- input$Can_column
    old_Time_name <- input$Time_column
    
    df_1 <- test()
    
    names(df_1)[names(df_1) == old_signal_name] <- 'Signal_1'
    names(df_1)[names(df_1) == old_sensor_id_name] <- 'Sensor_ID'
    names(df_1)[names(df_1) == old_WS_name] <- 'WS'
    names(df_1)[names(df_1) == old_WD_name] <- 'WD'
    names(df_1)[names(df_1) == old_Temp_name] <- 'Temp'
    names(df_1)[names(df_1) == old_RH_name] <- 'RH'
    names(df_1)[names(df_1) == old_Lat_name] <- 'Lat'
    names(df_1)[names(df_1) == old_Long_name] <- 'Long'
    names(df_1)[names(df_1) == old_Can_name] <- 'Canister'
    names(df_1)[names(df_1) == old_Time_name] <- 'Timestamp'
    
    df_1 <- as.data.frame(df_1)
    print(df_1)
    return(df_1)
  })
  
  # export CSV file of aggregated data - make a "daily files download" option ?? add to data check page?
  output$saveQAfile <- downloadHandler(
    filename = function() {
      QA_df <- test_df_new()
      paste0("Data_Export_", Sys.Date(), "_QA.csv", sep="")
    },
    content = function(file) {
      readr::write_csv(hot_to_r(input$upload_table), file)
    }
  )
  
  
  # Upload Table 
  output$upload_table <- renderRHandsontable({
    req(input$Time_column)  ##### this is causing red warning messages, find a way to suppress these?? also this is very slow...try with normal output table??
    all_QA_static <- df_new()
    all_QA_static[is.na(all_QA_static)] = " "
    all_QA_static$QA <- "None"
    all_QA_static$Timestamp <- as.character(all_QA_static$Timestamp)

    flags <- c("None","Calibration","Interferance","Maintenance","Malfunction","Other","WD_Interference", "WD_Error")
    rhandsontable(all_QA_static, width = 850, height = 550)%>%
      hot_col(col = "QA", type = "dropdown", source = flags)%>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)%>%
      hot_cols(columnSorting = TRUE)
  })
  
  
  ####### Data Processing, Cleaning, and Aggregating 
  data_5min <- reactive({
    WSunit <- input$WS_units
    TimeZone <- input$Time_Zone
    DF = hot_to_r(input$upload_table)
    DF <- as.data.frame(DF)
    DF$wsunit <- WSunit
    time_format <- input$Time_format
    ##################### Data prep and cleaning
    #formats <- c( "%d-%b-%Y %H:%M:%S","%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%m/%d/%Y %H:%M:%S" )
    formats <- time_format
    DF$Timestamp <- stringr::str_replace_all(DF$Timestamp, " AM", "") #strip out any AM/PM
    DF$Timestamp <- stringr::str_replace_all(DF$Timestamp, " PM", "") #strip out any AM/PM
    print(DF$Timestamp)
    DF$Timestamp <- lubridate::parse_date_time(DF$Timestamp, formats, tz = TimeZone)
    #DF$Timestamp <- as.POSIXct(DF$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    #force cols to numeric if they exist ; create NA versions if they dont exist so QA can run 
    DF <- DF %>% mutate(across(matches('Signal_1|WS|WD|Temp|RH|Canister|Lat|Long'), as.numeric))
    if(!'WS' %in% names(DF)) DF <- DF %>% mutate(WS = NA)
    if(!'WD' %in% names(DF)) DF <- DF %>% mutate(WD = NA)
    if(!'Signal_1' %in% names(DF)) DF <- DF %>% mutate(Signal_1 = NA)
    if(!'Lat' %in% names(DF)) DF <- DF %>% mutate(Lat = NA)
    if(!'Long' %in% names(DF)) DF <- DF %>% mutate(Long = NA)
    if(!'Canister' %in% names(DF)) DF <- DF %>% mutate(Canister = NA)
    if(!'Temp' %in% names(DF)) DF <- DF %>% mutate(Temp = NA)
    if(!'RH' %in% names(DF)) DF <- DF %>% mutate(RH = NA)

    DF <- as.data.frame(DF)
    # apply wind speed correction to m/s
    DF$WS_mps <- as.numeric(DF$WS) / as.numeric(DF$wsunit,2)
    
    # add U and V for WD averaging 
    DF$u <- DF$WS_mps * sin(2 * pi * DF$WD/360)
    DF$v <- DF$WS_mps * cos(2 * pi * DF$WD/360)
    # Check for lat and long // round to prevent slightly off coordinates // return 0 to NA for mapping
    if(!'Lat' %in% names(DF)) DF <- DF %>% add_column(Lat = 0)
    if(!'Long' %in% names(DF)) DF <- DF %>% add_column(Long = 0)
    DF$Lat <- round(DF$Lat, 3)
    DF$Long <- round(DF$Long, 3)
    DF$Lat[is.na(DF$Lat)] <- 0
    DF$Long[is.na(DF$Long)] <- 0
    
    ##################### Complete Auto QA scan
    ##### look for QA col, if not there, add it
    if(!'QA' %in% names(DF)) DF <- DF %>% add_column(QA = "None")
    # check for repeated wind/PID vals
    DF$QA <- ifelse(rep(rle(DF$WS)$lengths,
                        times = rle(DF$WS)$lengths) * sign(DF$WS) > 30, "WS_repeat", DF$QA ) # flag 10
    DF$QA <- ifelse(rep(rle(DF$WD)$lengths,
                        times = rle(DF$WD)$lengths) * sign(DF$WD) > 30, "WD_repeat", DF$QA ) # flag 11
    DF$QA <- ifelse(rep(rle(DF$Signal_1)$lengths,
                        times = rle(DF$Signal_1)$lengths) * sign(DF$Signal_1) > 30, "Sig_repeat", DF$QA ) # flag 12
    # check for illogical wind vals
    DF$QA <- ifelse(DF$WS > 40,"WS_offscale",DF$QA ) # flag 11
    DF$QA <- ifelse(DF$WD > 360 | DF$WD < 0,"WD_offscale",DF$QA ) # flag 12
    # check for missing data
    DF$QA <- ifelse(is.na(DF$Signal_1),"Missing_Signal",DF$QA ) # flag 13
    ######################## end of AutoQA Flagging
    # Baseline Correction (default to 10)
    DF <- DF[!is.na(DF$Timestamp),]
    DF$bc <- (DF$Signal_1 - getBaseline(DF$Signal_1, DF$Timestamp, df = 10)) 
    print(DF)
    
    # roll up to 5 min
    timeBase <- "5 min"
    timeBreaks <- seq(round(min(DF$Timestamp, na.rm = T), "min"),
                      round(max(DF$Timestamp, na.rm = T), "min"), timeBase)
    DF$timecut <- cut(DF$Timestamp, timeBreaks)
    
    # average vals to 5 min
    Data_5 <- DF %>%
      dplyr::group_by(Sensor_ID, timecut) %>%
      dplyr::summarize(
        signal_1 = mean(Signal_1, na.rm = TRUE),
        bc_signal_1 = mean(bc, na.rm = TRUE),
        ws = mean(WS_mps, na.rm = TRUE),
        Temp = mean(Temp, na.rm = TRUE),
        RH = mean(RH, na.rm = TRUE),
        U =  mean(u, na.rm = TRUE),
        V =  mean(v, na.rm = TRUE),
        QA = paste(unique(QA), collapse = ', '),
        Lat =  unique(Lat, na.rm = TRUE),
        Long =  unique(Long, na.rm = TRUE),
        Canister = paste(unique(Canister), collapse = ', ')
      )
    #calc WD based on U and V (leave WS as a scalar calc)
    Data_5$wd <- atan2(-Data_5$U, -Data_5$V)*180/pi + 180
    print(Data_5)
    #print(sum(is.na(Data_5$timecut)))
    return(Data_5)
  })
  
  
  
  
  
  
  # Data Check Page Functions -----------------------------------------------
  
  
  
  
  #display data (summary table showing 5 min data)
  output$summarytable <- DT::renderDataTable({
    req(input$files)
    TimeZone <- input$Time_Zone
    df <- as.data.frame(data_5min())
    df$timecut <- as.POSIXct(df$timecut, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    #df$timecut <- lubridate::parse_date_time(df$timecut,orders = "ymd HMS", tz = TimeZone)
    summary <- df %>%
      dplyr::group_by(Sensor_ID) %>%
      dplyr::summarise(
        Start_Time = min(timecut, na.rm = T),
        End_Time = max(timecut, na.rm = T),
        Lat = unique(Lat),
        Long = unique(Long),
        Count = n(), 
        QA =paste(unique(QA), collapse = ', '),
        Canister = paste(unique(Canister), collapse = ', ')
      )
    summary$Start_Time <- as.character(summary$Start_Time)
    summary$End_Time <- as.character(summary$End_Time)
    
    datatable(summary,
              selection = 'single',
              options = list(autoWidth = TRUE,
                             scrollX = TRUE,
                             searching = FALSE,
                             lengthChange = FALSE),
              rownames= FALSE)
  })
  
  
  
  #### add export options here: daily 5 min processed files or one large file?
  
  
  

# Dashboard Page Functions ------------------------------------------------

  
  
  # build in option to exclude low wind speed data from analysis 
  data_5min_highws <- reactive({
    req(data_5min())
    all <- data_5min()
    data_5min_highws <- subset(all, all$ws >= 1)
  })
  
  # select between SPods for analysis 
  output$unitselect <- renderUI({
    choice <-  unique(data_5min()$Sensor_ID)
    selectInput("unitselect",h4("Select unit to display:"), choices = choice, selected = choice[1])
  })
  
  # select active SPod data set (> 1 min or normal)
  data_5min_active <- reactive({
    if (input$ws_select == "All Data")
      data_5min()
    else if (input$ws_select == ">1 m/s")
      data_5min_highws()
    # else
    #   stop("Unexpected dataset")
  })
  
  
  
  
  # Leaflet polar map
  output$polarmap <- renderLeaflet({
    req(input$unitselect)
    req(input$statselect)
    unitinput <- input$unitselect
    print(unitinput)
    statinput <- input$statselect
    req(data_5min_active())
    data_all_5 <- as.data.frame(data_5min_active()) 
    data_all_5_1 <- subset(data_all_5,
                           data_all_5$ws >= input$windfilterInput[1] & 
                             data_all_5$ws <= input$windfilterInput[2]&
                             data_all_5$QA == "None") 
    
    output$latlongtext <- renderText({ "Note: Basemap only displayed when lat/long data detected"})
    polarMap(data_all_5_1,
             latitude = 'Lat',
             longitude = 'Long',
             pollutant = 'bc_signal_1',
             statistic = statinput,
             provider = "Esri.WorldImagery",
             key = TRUE,
             #limits = "fixed",
             # iconWidth = 450, iconHeight = 650,
             # fig.width = 5, fig.height = 5,
             par.settings=list(fontsize=list(text=19),
                               add.line = list(col = "white"),
                               axis.line = list(col = "white"),
                               axis.text = list(col = 'white'),
                               add.text = list(col = 'white'),
                               layout.widths = list(left.padding = 3, right.padding = 0, axis.key.padding = 0)
             ))
  })
  #str(trellis.par.get(), max.level = 1)
  ######################################################## SDI plots and Wind Roses
  output$FREQ <- renderPlot({
    req(input$statselectFREQ)
    req(input$unitselect)
    req(data_5min_active())
    unitinput <- input$unitselect
   # print(unitinput)
    data_all_5 <- as.data.frame(data_5min_active())
    data_all_5_1 <- subset(data_all_5,
                           data_all_5$Sensor_ID == input$unitselect &
                             data_all_5$QA == "None")
    statFREQ <- input$statselectFREQ
    trellis.par.set(theme = col.whitebg()) # make background transparent 
    
    
    polarFreq(data_all_5_1, pollutant = "bc_signal_1",fontsize = 18,
              statistic = statFREQ, main = NULL, key.position = "right", 
              par.settings = col.whitebg())
  })
  
  SDI_build <- reactive({# SDI plot
    req(data_5min_active())
    req(input$statselectSDI) 
    data_all_5 <- as.data.frame(data_5min_active())
    data_all_5_1 <- subset(data_all_5,
                           data_all_5$Sensor_ID == input$unitselect &
                             data_all_5$QA == "None")
    statSDI <- input$statselectSDI
    
    trellis.par.set(theme = col.whitebg()) # make background transparent 
    
    polarPlot(data_all_5_1, pollutant = "bc_signal_1",  fontsize = 25,
              statistic = statSDI, main = NULL, key.position = "right", 
              par.settings = col.whitebg())
    
   
    
  })
  output$SDI <- renderPlot({
    SDI_build()
  })
  WR_build <- reactive({# Wind Rose plot
    req(data_5min_active())
    data_all_5 <- as.data.frame(data_5min_active())
    data_all_5_1 <- subset(data_all_5,
                           data_all_5$Sensor_ID == input$unitselect &
                             data_all_5$QA == "None")
    
    #trellis.par.set(background = list(col="green")) # make background transparent 
    
    trellis.par.set(theme = col.whitebg()) # make background transparent 
    
    windRose(data_all_5_1,  fontsize = 18, paddle = F, cols = "hue",
             main = NULL, key.position = "right",
             par.settings=list(par.sub.text=list(cex=0.8),
                               col.whitebg()))
    
  })
  output$WR <- renderPlot({
    WR_build()
  })
  ######################################################## time series outputs
  BCplot_build <- reactive({# background Correction plot
    req(data_5min_active())
    data_all_5 <- as.data.frame(data_5min_active())
    data_all_5_1 <- subset(data_all_5,
                           data_all_5$Sensor_ID == input$unitselect &
                             data_all_5$QA == "None")
    p <-
      plot_ly(
        data_all_5_1,
        x = ~ timecut,
        y = ~ signal_1,
        type = "scatter", name = "Raw Signal",
        hovertext = ~ paste0("time: ", data_all_5_1$timecut, "<br>", "WD: ", round(data_all_5_1$wd,2)),
        hoverinfo = "text",
        mode = "lines",  showlegend = T, connectgaps = FALSE, line = list(color = "black")) %>%
      layout(showlegend = T,
             yaxis = list(title = paste0 ("5-min Signal (",input$Signal_units, ")"), showgrid = FALSE, showline = TRUE, mirror=TRUE),
             legend = list(
               orientation = "h",
               x = 0.3,
               y = -0.5
             ),
             xaxis = list(type = 'Date', tickformat = "%m/%d/%y %H:%M", showgrid = FALSE, showline = TRUE, mirror=TRUE),
             scene = list(xaxis = list(showgrid = F, showline = TRUE, mirror=TRUE),
                          yaxis = list(showgrid = F, showline = TRUE, mirror=TRUE))
      )
    p <- p %>% add_trace(y = data_all_5_1$signal_1 - data_all_5_1$bc_signal_1, name = 'Baseline', mode = 'lines', connectgaps = FALSE, line = list(color = "red"))
    p
  })
  
  output$BCplot <- renderPlotly({ # baseline correction plot
    BCplot_build()
  })
  
  WDplot_build <- reactive({    # Wind Direction plot
    req(data_5min_active())
    data_all_5 <- as.data.frame(data_5min_active())
    data_all_5_1 <- subset(data_all_5,
                           data_all_5$Sensor_ID == input$unitselect &
                             data_all_5$QA == "None")
    
    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "5-min Signal (ppb)",
      showgrid = FALSE,
      showline = TRUE, mirror=TRUE)
    w <-
      plot_ly(
        data_all_5_1,
        x = ~timecut,
        y = ~wd,
        type = "scatter", name = "Wind Direction",
        hovertext = ~ paste0("time: ", data_all_5_1$timecut),
        hoverinfo = "text",
        mode = "markers",  showlegend = T, marker = list(color = "green")) %>%
      layout(showlegend = T,
             legend = list(
               orientation = "h",
               x = 0.3,
               y = -0.5
             ),
             xaxis = list(type = 'Date', tickformat = "%m/%d/%y %H:%M", showgrid = FALSE, mirror=TRUE),
             scene = list(xaxis = list(showgrid = F,showline = TRUE, mirror=TRUE),
                          yaxis = list(showgrid = F, showline = TRUE, mirror=TRUE))
      )
    w <- w %>% add_trace(y = data_all_5_1$bc_signal_1, name = 'Signal', yaxis = "y2",type = 'scatter', mode = 'lines', connectgaps = FALSE, line = list(color = "black"), marker = list(color = 'black', opacity=0))
    w <- w %>% layout(
      yaxis2 = ay,
      xaxis = list(title="Date", showgrid = FALSE, showline = TRUE, mirror=TRUE),
      yaxis = list(title= list(text = "Wind Direction (Deg.)", font = list(color = 'darkgreen')), tickfont = list(color = 'darkgreen'), showgrid = FALSE, showline = TRUE, mirror=TRUE),
      margin = list(l = 50, t = 50, b =50, r = 100, pad = 20))
    w
  })
  
  output$windplot <- renderPlotly({ # wind direction plot
    WDplot_build()
  })
  

  WSplot_build <- reactive({    # Wind Direction plot
    req(data_5min_active())
    data_all_5 <- as.data.frame(data_5min_active())
    data_all_5_1 <- subset(data_all_5,
                           data_all_5$Sensor_ID == input$unitselect &
                             data_all_5$QA == "None")
    
    ay <- list(
      tickfont = list(color = "black",size = 20),
      overlaying = "y",
      side = "right",
      title = "5-min Signal (ppb)",
      showgrid = FALSE,
      showline = TRUE, mirror=TRUE,
      titlefont = list(size = 20))
    w <-
      plot_ly(
        data_all_5_1,
        x = ~timecut,
        y = ~ws,
        type = "scatter", name = "Wind Speed",
        hovertext = ~ paste0("time: ", data_all_5_1$timecut),
        hoverinfo = "text",
        mode = "markers",  showlegend = T, marker = list(color = "gray")) %>%
      layout(showlegend = F,
             legend = list(
               orientation = "h",
               x = 0.3,
               y = -0.5
             ),
             xaxis = list(type = 'Date', tickformat = "%m/%d/%y\n %H:%M", showgrid = FALSE, mirror=TRUE,list(titlefont = list(size = 20),tickfont = list(size = 20))),
             scene = list(xaxis = list(showgrid = F,showline = TRUE, mirror=TRUE),
                          yaxis = list(showgrid = F, showline = TRUE, mirror=TRUE))
      )
    w <- w %>% add_trace(y = data_all_5_1$bc_signal_1, name = 'Signal', yaxis = "y2",type = 'scatter', mode = 'lines', connectgaps = FALSE, line = list(color = "black"), marker = list(color = 'black', opacity=0))
    w <- w %>% layout(
      yaxis2 = ay,
      xaxis = list(title=list(text = "Date", font = list(color = 'black', size = 20)), tickfont = list(color = 'black', size = 20), showgrid = FALSE, showline = TRUE, mirror=TRUE,list(titlefont = list(size = 20),tickfont = list(size = 20))),
      yaxis = list(title= list(text = "Wind Speed (m/s)", font = list(color = 'gray', size = 20)), tickfont = list(color = 'gray', size = 20), showgrid = FALSE, showline = TRUE, mirror=TRUE,list(titlefont = list(size = 20))),
      margin = list(l = 50, t = 50, b =50, r = 100, pad = 20))
    w
  })
  
  output$windspeedplot <- renderPlotly({ # wind direction plot
    WSplot_build()
  })
 
  
  CTplot_build <- reactive({# Canister plot
    req(data_5min_active())
    data_all_5 <- as.data.frame(data_5min_active())
    data_all_5_1 <- subset(data_all_5,
                           data_all_5$Sensor_ID == input$unitselect)
    
    data_all_5_1$Canister <- as.character(data_all_5_1$Canister)
    data_all_5_1$port1 <- ifelse(grepl("1", data_all_5_1$Canister), -1, NA)
    data_all_5_1$port2 <- ifelse(grepl("2", data_all_5_1$Canister), -1.4, NA)
    data_all_5_1$port3 <- ifelse(grepl("4", data_all_5_1$Canister), -1.8, NA)
    data_all_5_1$port4 <- ifelse(grepl("8", data_all_5_1$Canister), -2.2, NA)
    
    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "5-min Signal (ppb)",
      showgrid = FALSE,
      showline = TRUE, mirror=TRUE)
    
    w <-
      plot_ly(
        data_all_5_1,
        x = ~timecut,
        y = ~port1,
        type = "scatter", name = "port1",
        hovertext = ~ paste0("time: ", data_all_5_1$timecut, "<br>", "WD: ", round(data_all_5_1$wd,2)),
        hoverinfo = "text",
        mode = "markers",  showlegend = T, marker = list(color = "darkgreen", size = 10, opacity = 0.8)) %>%
      layout(showlegend = T,
             yaxis = list(title = " ", range = c(-4,6), tickfont = list(color = "white"),
                          zeroline = FALSE, showgrid = FALSE, showline = TRUE, mirror=TRUE),
             legend = list(
               orientation = "h",
               x = 0.3,
               y = -0.5
             ),
             xaxis = list(type = 'Date', tickformat = "%m/%d/%y %H:%M", showgrid = FALSE,showline = TRUE, mirror=TRUE),
             scene = list(xaxis = list(showgrid = F, showline = TRUE, mirror=TRUE),
                          yaxis = list(showgrid = F, showline = TRUE, mirror=TRUE))
      )
    w <- w %>% add_trace( data_all_5_1,
                          x = ~timecut,
                          y = ~port2,
                          type = "scatter", name = "port2",
                          hovertext = ~ paste0("time: ", data_all_5_1$timecut, "<br>", "WD: ", round(data_all_5_1$wd,2)),
                          hoverinfo = "text",
                          mode = "markers",  showlegend = T, marker = list(color = "darkcyan", size = 10, opacity = 0.8))
    w <- w %>% add_trace( data_all_5_1,
                          x = ~timecut,
                          y = ~port3,
                          hovertext = ~ paste0("time: ", data_all_5_1$timecut, "<br>", "WD: ", round(data_all_5_1$wd,2)),
                          hoverinfo = "text",
                          type = "scatter", name = "port3",
                          mode = "markers",  showlegend = T, marker = list(color = "mediumblue", size = 10, opacity = 0.8))
    w <- w %>% add_trace( data_all_5_1,
                          x = ~timecut,
                          y = ~port4,
                          hovertext = ~ paste0("time: ", data_all_5_1$timecut, "<br>", "WD: ", round(data_all_5_1$wd,2)),
                          hoverinfo = "text",
                          type = "scatter", name = "port4",
                          mode = "markers",  showlegend = T, marker = list(color = "purple4", size = 10, opacity = 0.8))
    w <- w %>% add_trace(y = data_all_5_1$bc_signal_1, name = 'Signal', yaxis = "y2",type = 'scatter', mode = 'lines', connectgaps = FALSE, line = list(color = "black"), marker = list(color = 'black', opacity=0))
    w <- w %>% layout(
      yaxis2 = ay,
      xaxis = list(title="Date", showgrid = FALSE, mirror=TRUE),
      yaxis = list(title=" ", showgrid = FALSE, mirror=TRUE),
      margin = list(l = 50, t = 50, b =50, r = 100, pad = 20))
    
    w
  })
  output$canplot <- renderPlotly({ # Canister plot
    CTplot_build()
  })
  
  RHplot_build <- reactive({    # Relative Humidity plot
    req(data_5min_active())
    data_all_5 <- as.data.frame(data_5min_active())
    data_all_5_1 <- subset(data_all_5,
                           data_all_5$Sensor_ID == input$unitselect &
                             data_all_5$QA == "None")
    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = paste0 ("5-min Signal (",input$Signal_units, ")"), 
      showgrid = FALSE, mirror=TRUE)
    w <-
      plot_ly(
        data_all_5_1,
        x = ~timecut,
        y = ~RH,
        type = "scatter", name = "Relative Humidity",
        hovertext = ~ paste0("time: ", data_all_5_1$timecut),
        hoverinfo = "text",
        mode = "markers",  showlegend = T, marker = list(color = "purple")) %>%
      layout(showlegend = T,
             legend = list(
               orientation = "h",
               x = 0.3,
               y = -0.5
             ),
             xaxis = list(type = 'Date', tickformat = "%m/%d/%y %H:%M", showgrid = FALSE),
             scene = list(xaxis = list(showgrid = F, showline = TRUE, mirror=TRUE),
                          yaxis = list(showgrid = F, showline = TRUE, mirror=TRUE))
      )
    w <- w %>% add_trace(y = data_all_5_1$bc_signal_1, name = 'Signal', yaxis = "y2",type = 'scatter', mode = 'lines', connectgaps = FALSE, line = list(color = "black"), marker = list(color = 'black', opacity=0))
    w <- w %>% layout(
      yaxis2 = ay,
      xaxis = list(title="Date", showgrid = FALSE, showline = TRUE, mirror=TRUE),
      yaxis = list(title= list(text = "Relative Humididty (%)", font = list(color = 'purple')), tickfont = list(color = 'purple'), showgrid = FALSE, showline = TRUE, mirror=TRUE),
      margin = list(l = 50, t = 50, b =50, r = 100, pad = 20))
    w
  })
  
  output$RHplot <- renderPlotly({ # relative humidity plot
    RHplot_build()
  })
  
  Tplot_build <- reactive({    # Temperature plot
    req(data_5min_active())
    data_all_5 <- as.data.frame(data_5min_active())
    data_all_5_1 <- subset(data_all_5,
                           data_all_5$Sensor_ID == input$unitselect &
                             data_all_5$QA == "None")
    
    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = paste0 ("5-min Signal (",input$Signal_units, ")"),
      showgrid = FALSE, showline = TRUE, mirror=TRUE)
    w <-
      plot_ly(
        data_all_5_1,
        x = ~timecut,
        y = ~Temp,
        type = "scatter", name = "Temperature",
        hovertext = ~ paste0("time: ", data_all_5_1$timecut),
        hoverinfo = "text",
        mode = "markers",  showlegend = T, marker = list(color = "blue")) %>%
      layout(showlegend = T,
             legend = list(
               orientation = "h",
               x = 0.3,
               y = -0.5
             ),
             xaxis = list(type = 'Date', tickformat = "%m/%d/%y %H:%M", showgrid = FALSE),
             scene = list(xaxis = list(showgrid = F),
                          yaxis = list(showgrid = F))
      )
    w <- w %>% add_trace(y = data_all_5_1$bc_signal_1, name = 'Signal', yaxis = "y2",type = 'scatter', mode = 'lines', connectgaps = FALSE, line = list(color = "black"), marker = list(color = 'black', opacity=0))
    w <- w %>% layout(
      yaxis2 = ay,
      xaxis = list(title="Date", showgrid = FALSE, showline = TRUE, mirror=TRUE),
      yaxis = list(title= list(text = "Temperature (C)", font = list(color = 'blue')), tickfont = list(color = 'blue'), showgrid = FALSE, showline = TRUE, mirror=TRUE),
      margin = list(l = 50, t = 50, b =50, r = 100, pad = 20))
    w
  })
  
  output$Tplot <- renderPlotly({ # temperature plot
    Tplot_build()
  })
  
  CALplot_build <- reactive({ # calibrations plot
    req(data_5min_active())
    data_all_5 <- as.data.frame(data_5min_active())
    data_all_5_1 <- subset(data_all_5,
                           data_all_5$Sensor_ID == input$unitselect )
    
    data_all_5_1$QA <- as.character(data_all_5_1$QA)
    data_all_5_1$Calibration <- ifelse(grepl("Calibration", data_all_5_1$QA), 1, NA)   
    
    
    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = paste0 ("5-min Signal (",input$Signal_units, ")"),
      showgrid = FALSE, showline = TRUE, mirror=TRUE)
    w <-
      plot_ly(
        data_all_5_1,
        x = ~timecut,
        y = ~Calibration,
        type = "scatter", name = "Calibration",
        hovertext = ~ paste0("time: ", data_all_5_1$timecut),
        hoverinfo = "text",
        mode = "markers",  showlegend = T, marker = list(color = "orange", size = 10, opacity = 0.8)) %>%
      layout(showlegend = T,
             yaxis = list(title = " ", range = c(-4,6), tickfont = list(color = "white"),
                          zeroline = FALSE, showgrid = FALSE, showline = TRUE, mirror=TRUE),
             legend = list(
               orientation = "h",
               x = 0.3,
               y = -0.5
             ),
             xaxis = list(type = 'Date', tickformat = "%m/%d/%y %H:%M", showgrid = FALSE, showline = TRUE, mirror=TRUE),
             scene = list(xaxis = list(showgrid = F),
                          yaxis = list(showgrid = F))
      )
    
    w <- w %>% add_trace(y = data_all_5_1$bc_signal_1, name = 'Signal', yaxis = "y2",type = 'scatter', mode = 'lines', connectgaps = FALSE, line = list(color = "black"), marker = list(color = 'black', opacity=0))
    w <- w %>% layout(
      yaxis2 = ay,
      xaxis = list(title="Date", showgrid = FALSE, showline = TRUE, mirror=TRUE),
      yaxis = list(title="Calibration", showgrid = FALSE, showline = TRUE, mirror=TRUE),
      margin = list(l = 50, t = 50, b =50, r = 100, pad = 20))
    
    w
  })
  
  output$CALplot <- renderPlotly({ # Calibrations plot
    CALplot_build()
  })
  
  ####################################################### Report Output Option
  output$report <- downloadHandler( # calls Sentinel_Report.RMD to build doc
    filename = "Sentinel_Report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "Sentinel_Report.Rmd") #.html
      file.copy("Sentinel_Report.Rmd", tempReport, overwrite = TRUE)
      params <- list(plotBC = BCplot_build(),
                     plotWD = WDplot_build(),
                     plotWS = WSplot_build(),
                     plotCT = CTplot_build(),
                     plotRH = RHplot_build(),
                     plotT = Tplot_build(),
                     plotCAL = CALplot_build(),
                     WR = WR_build(),
                     SDI = SDI_build(),
                     SN = input$data_5min_active,
                     OutputDate = Sys.Date())
      rmarkdown::render(tempReport, output_file = file,
                        params = params,output_format = "html_document",
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
  
  
  
  
  
  
  
  # Data Table Page Functions ------------------------------------------------
  
  output$datatab <-  DT::renderDataTable({
    all_5_tab <-  data_5min()
    all_5_tab <- all_5_tab %>%
      mutate_if(is.numeric, round, digits = 2)
    DT::datatable(all_5_tab,filter = 'top',
                  options = list(scrollX = TRUE)) 
  })
  
  output$Download <-
    downloadHandler(
      filename = function () {
        paste("SENTINELData_5min.csv", sep = "")
      },
      content = function(file) {
        write.csv(data_5min(), file)
      }
    )
  
  
  
  
  
  
  # Single Node Calibration Page Functions -----------------------------------
  
  # select between SPods for analysis 
  output$calunitselect <- renderUI({
    choice <-  unique(df_new()$Sensor_ID)
    selectInput("calunitselect",h4("Select unit:"), choices = choice, selected = choice[1])
  })
  

  
  
  # build single node df
  getcaldata <- reactive({ # make data for QA table, from base dataset entered in data_upload (df_new)
    req(input$calunitselect)
    req(input$freqfile)
    req(input$Time_Zone)
    req(input$Time_format)
    time_format <- input$Time_format
    TimeZone <- input$Time_Zone
    duration <- as.numeric(input$durationInput)
    frequency_sec <- as.numeric(input$freqfile)
    start_time <- as.POSIXct(input$singlenodestarttime, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    end_time <- as.POSIXct(input$singlenodeendtime, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    req(input$Time_column) 
    DF <- df_new()
    DF$Timestamp <- as.character(DF$Timestamp)
    formats <- time_format
    DF$Timestamp <- lubridate::parse_date_time(DF$Timestamp, formats, tz = TimeZone)
    DF$Timestamp <- as.POSIXct(DF$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    DF_unit <-
      DF %>%
      filter(Sensor_ID == input$calunitselect)
    # fix lat/long capitalization issue....
    names(DF_unit)[names(DF_unit) == 'lat'] <- 'Lat'
    names(DF_unit)[names(DF_unit) == 'long'] <- 'Long'
    #force cols to numeric if they exist ; create NA versions if they dont exist so QA can run 
    DF_unit <- DF_unit %>% mutate(across(matches('Signal_1|WS|WD|Temp|RH|Canister'), as.numeric))
    if(!'WS' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(WS = NA)
    if(!'WD' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(WD = NA)
    if(!'Signal_1' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(Signal_1 = NA)
    if(!'Canister' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(Canister = NA)
    if(!'Temp' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(Temp = NA)
    if(!'RH' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(RH = NA)
    DF_unit <- as.data.frame(DF_unit)
    #print(getBaseline(DF_unit$Signal_1, DF_unit$Timestamp, df = 10))
    DF_unit$BC <- DF_unit$Signal_1 - getBaseline(DF_unit$Signal_1, DF_unit$Timestamp, df = 10)
    filtered <-
      DF_unit %>%
      filter(Timestamp >= start_time & Timestamp <= end_time)
    filtered$u <- filtered$WS * sin(2 * pi * filtered$WD/360)
    filtered$v <- filtered$WS * cos(2 * pi * filtered$WD/360)
    filtered$WD <- atan2(-1 *filtered$u, -1 *filtered$v)*180/pi + 180
    filtered_sum <- filtered %>%
      select(where(is.numeric)) %>%
      select(-any_of(c("Lat", "Long","u","v"))) %>%  
      pivot_longer(cols = everything()) %>%
      group_by(name) %>%
      dplyr::summarise(mean = mean(value, na.rm = TRUE), std = sd(value, na.rm = TRUE),
                med = median(value, na.rm = TRUE), max = max(value, na.rm = TRUE), min = min(value, na.rm = TRUE),
                comp = (sum(!is.na(value))/(duration/frequency_sec))*100)
    print(filtered_sum)
    return(filtered_sum)
  })
  
  

  
  #Start time object
  singlenodetimestampstart <- reactive({ # create start time
    req(input$Time_Zone)
    TimeZone <- input$Time_Zone
    req(input$Time_column)  
    req(input$calunitselect)
    req(input$Time_format)
    time_format <- input$Time_format
    DF <- df_new()
    DF_unit <-
      DF %>%
      filter(Sensor_ID == input$calunitselect) 
    DF_unit$Timestamp <- as.character(DF_unit$Timestamp)
    formats <- time_format
    DF_unit$Timestamp <- lubridate::parse_date_time(DF_unit$Timestamp, formats, tz = TimeZone)
    DF_unit$Timestamp <- as.POSIXct(DF_unit$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    DF_unit <- arrange(DF_unit, Timestamp)
    singlenodetimestampstart <- as.character(DF_unit$Timestamp[[1]])
  })
  output$singlenodestarttime <- renderUI({
    textInput(inputId = "singlenodestarttime", label = "Start Time:", value = singlenodetimestampstart())
  })

  
# End time object 
  singlenodetimestampend <- reactive({ # create end time
    req(input$Time_Zone)
    TimeZone <- input$Time_Zone
    req(input$Time_column)  
    req(input$calunitselect)
    req(input$Time_format)
    time_format <- input$Time_format
    DF <- df_new()
    DF_unit <-
      DF %>%
      filter(Sensor_ID == input$calunitselect) 
    DF_unit$Timestamp <- as.character(DF_unit$Timestamp)
    formats <- time_format
    DF_unit$Timestamp <- lubridate::parse_date_time(DF_unit$Timestamp, formats, tz = TimeZone)
    DF_unit$Timestamp <- as.POSIXct(DF_unit$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    DF_unit <- arrange(DF_unit, Timestamp)
    singlenodetimestampend <- as.character(DF_unit$Timestamp[[2]])
  })
  output$singlenodeendtime <- renderUI({
    textInput(inputId = "singlenodeendtime", label = "End Time:", value = singlenodetimestampend())
  })

  
  # develop table in app 
  draw_caltab <- function(){ # create output for RMD  
    req(getcaldata())
     req(input$singlenodestarttime)
     req(input$singlenodeendtime)
     start_time <- as.character(input$singlenodestarttime, tz = "America/New_York")
     end_time <- as.character(input$singlenodeendtime, tz = "America/New_York")
    caltab <- getcaldata()
    caltab <- as.data.frame(caltab)
    #print(caltab)
    names(caltab) <- c("Variable", "Mean", "SD", "Median", "Max", "Min", "% Complete")
    caltab %>%
      kbl(escape = F, caption = paste0(start_time, " to ", end_time), digits = 2, table.attr = "style='width:20%;'")%>%
      kable_classic(full_width = F, html_font = "Cambria") %>%
      kable_styling(latex_options = "HOLD_position")
  }
  output$draw_caltab <- function() { # use function above to render table in UI in app
    draw_caltab()
  }
  
  
  
  start.end.time_1 <- function(){ # create output for RMD
    req(getcaldata())
    req(input$singlenodestarttime)
    req(input$singlenodeendtime)
    req(input$Time_Zone)
    TimeZone <- input$Time_Zone
    start_time <- as.POSIXct(input$singlenodestarttime, tz = TimeZone)
    end_time <- as.POSIXct(input$singlenodeendtime, tz = TimeZone)
    start.end.time <- paste0(substr(start_time, start = 11, stop = 23), " to",
                             substr(end_time, start = 11, stop = 23))
    start.end.time
  }
  
  SN_1 <- function(){ # create output for RMD
    req(input$calunitselect)
    SN <- input$calunitselect
    return(SN)
  }

  date_1 <- function(){ # create output for RMD
    req(input$singlenodestarttime)
    req(input$Time_Zone)
    TimeZone <- input$Time_Zone
    start_time <- as.POSIXct(input$singlenodestarttime, tz = TimeZone)
    date <- paste0(substr(start_time, start = 1, stop = 10))
    date
  }

  QATableID_1 <- function(){ # create output for RMD
    req(input$singlenodestarttime)
    req(input$singlenodeendtime)
    req(input$Time_Zone)
    TimeZone <- input$Time_Zone
    start_time <- as.POSIXct(input$singlenodestarttime, tz = TimeZone)
    end_time <- as.POSIXct(input$singlenodeendtime, tz = TimeZone)
    start.end.time <- start.end.time_1()
    Date <- date_1()
    Serial.number <- SN_1()
    QA.table.ID <- paste0(Serial.number,"_", Date,"_", paste0(substr(start_time, start = 12, stop = 19), "-",
                                                              substr(end_time, start = 12, stop = 19)))
    QATableID <- gsub("[[:punct:]]", "_", QA.table.ID)
    QATableID
  }

  output$singlenodereport <- downloadHandler(
    filename = "single_node_QA_Table.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "single_node_QA_Table.Rmd") #.html
      file.copy("single_node_QA_Table.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        draw_caltab = draw_caltab,
        SN_1 = SN_1(),
        date_1 = date_1(),
        start.end.time_1 = start.end.time_1(),
        OutputDate = Sys.Date(),
        RCodeVersion = "Version 1.0",
        QATableID_1 = QATableID_1(),
        Analyst = "_________________________________"
      )

      rmarkdown::render(tempReport, output_file = file,
                        params = params,output_format = "html_document",
                        envir = new.env(parent = globalenv())
      )
    }
  )




  
  
  
  # Multi Node Calibration Page Functions -----------------------------------
  
  # select between SPods for analysis 
  output$calunitselect1 <- renderUI({
    choice <-  unique(df_new()$Sensor_ID)
    selectInput("calunitselect1",h4("Select unit:"), choices = choice, selected = choice[1])
  })
  
  # select between SPods for analysis 
  output$calunitselect2 <- renderUI({
    choice <-  unique(df_new()$Sensor_ID)
    selectInput("calunitselect2",h4("Select unit:"), choices = choice, selected = choice[2])
  })
  
  # make df for cal data unit 1
  getcaldata1_base <- reactive({ # make data for QA table, from base dataset entered in data_upload (df_new)
    req(input$calunitselect1)
    req(input$Time_Zone)
    req(input$Time_format)
    time_format <- input$Time_format
    TimeZone <- input$Time_Zone
    start_time2 <- as.POSIXct(input$multinodestarttime, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    end_time2 <- as.POSIXct(input$multinodeendtime, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    req(input$Time_column) 
    DF <- df_new()
    DF$Timestamp <- as.character(DF$Timestamp)
    formats <- time_format
    DF$Timestamp <- lubridate::parse_date_time(DF$Timestamp, formats, tz = TimeZone)
    DF$Timestamp <- as.POSIXct(DF$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    DF_unit <-
      DF %>%
      filter(Sensor_ID == input$calunitselect1)
    # fix lat/long capitalization issue....
    names(DF_unit)[names(DF_unit) == 'lat'] <- 'Lat'
    names(DF_unit)[names(DF_unit) == 'long'] <- 'Long'
    #force cols to numeric if they exist ; create NA versions if they dont exist so QA can run 
    DF_unit <- DF_unit %>% mutate(across(matches('Signal_1|WS|WD|Temp|RH|Canister'), as.numeric))
    if(!'WS' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(WS = NA)
    if(!'WD' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(WD = NA)
    if(!'Signal_1' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(Signal_1 = NA)
    if(!'Canister' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(Canister = NA)
    if(!'Temp' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(Temp = NA)
    if(!'RH' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(RH = NA)
    DF_unit <- as.data.frame(DF_unit)
    DF_unit$BC <- DF_unit$Signal_1 - getBaseline(DF_unit$Signal_1, DF_unit$Timestamp, df = 10)
    
    filtered <-
      DF_unit %>%
      filter(Timestamp >= start_time2 & Timestamp <= end_time2)
    filtered$u <- filtered$WS * sin(2 * pi * filtered$WD/360)
    filtered$v <- filtered$WS * cos(2 * pi * filtered$WD/360)
    filtered$WD <- atan2(-1 *filtered$u, -1 *filtered$v)*180/pi + 180
    #filtered$Signal_BC <- filtered$Signal_1 - getBaseline(filtered$Signal_1, filtered$Timestamp, df = 10)
    filtered$unit <- as.character(input$calunitselect1)
    
    print(filtered)
    return(filtered)
  })
  
  
  # make df for cal data unit 1
  getcaldata1 <- reactive({ # make data for QA table, from base dataset entered in data_upload (df_new)
    filtered1 <- as.data.frame(getcaldata1_base())
    filtered_sum <- filtered1 %>%
      select(where(is.numeric)) %>%
      select(-any_of(c("Lat", "Long","u","v"))) %>%  
      pivot_longer(cols = everything()) %>%
      group_by(name) %>%
      dplyr::summarise(mean = mean(value, na.rm = TRUE), std = sd(value, na.rm = TRUE),
                       med = median(value, na.rm = TRUE), max = max(value, na.rm = TRUE), min = min(value, na.rm = TRUE),
                       n = (sum(!is.na(value))))
    return(filtered_sum)
  })
  
  
  
  
  multinodetimestampstart <- reactive({
    req(input$Time_Zone)
    TimeZone <- input$Time_Zone
    req(input$Time_column)  
    req(input$calunitselect1)
    req(input$Time_format)
    time_format <- input$Time_format
    DF <- df_new()
    DF_unit <-
      DF %>%
      filter(Sensor_ID == input$calunitselect1) 
    DF_unit$Timestamp <- as.character(DF_unit$Timestamp)
    formats <- time_format
    DF_unit$Timestamp <- lubridate::parse_date_time(DF_unit$Timestamp, formats, tz = TimeZone)
    DF_unit$Timestamp <- as.POSIXct(DF_unit$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    DF_unit <- arrange(DF_unit, Timestamp)
    multinodetimestampstart <- as.character(DF_unit$Timestamp[[1]])
  })
  output$multinodestarttime <- renderUI({
    textInput(inputId = "multinodestarttime", label = "Start Time:", value = multinodetimestampstart())
  })
  
  
  
  multinodetimestampend <- reactive({
    req(input$Time_Zone)
    TimeZone <- input$Time_Zone
    req(input$Time_column)  
    req(input$calunitselect1)
    req(input$Time_format)
    time_format <- input$Time_format
    DF <- df_new()
    DF_unit <-
      DF %>%
      filter(Sensor_ID == input$calunitselect1) 
    DF_unit$Timestamp <- as.character(DF_unit$Timestamp)
    formats <- time_format
    DF_unit$Timestamp <- lubridate::parse_date_time(DF_unit$Timestamp, formats, tz = TimeZone)
    DF_unit$Timestamp <- as.POSIXct(DF_unit$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    DF_unit <- arrange(DF_unit, Timestamp)
    multinodetimestampstart <- as.character(DF_unit$Timestamp[[2]])
    
  })
  output$multinodeendtime <- renderUI({
    textInput(inputId = "multinodeendtime", label = "End Time:", value = multinodetimestampend())
  })
  
  
  # make df for cal data unit 2
  getcaldata2_base <- reactive({
    req(input$calunitselect2)
    req(input$Time_Zone)
    req(input$Time_format)
    time_format <- input$Time_format
    TimeZone <- input$Time_Zone
    start_time2 <- as.POSIXct(input$multinodestarttime, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    end_time2 <- as.POSIXct(input$multinodeendtime, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    req(input$Time_column) 
    DF <- df_new()
    #print(DF$Sensor_ID)
    DF$Timestamp <- as.character(DF$Timestamp)
    formats <- time_format
    DF$Timestamp <- lubridate::parse_date_time(DF$Timestamp, formats, tz = TimeZone)
    DF$Timestamp <- as.POSIXct(DF$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = TimeZone)
    DF_unit <-
      DF %>%
      filter(Sensor_ID == input$calunitselect2)
    # fix lat/long capitalization issue....
    names(DF_unit)[names(DF_unit) == 'lat'] <- 'Lat'
    names(DF_unit)[names(DF_unit) == 'long'] <- 'Long'
    #force cols to numeric if they exist ; create NA versions if they dont exist so QA can run 
    DF_unit <- DF_unit %>% mutate(across(matches('Signal_1|WS|WD|Temp|RH|Canister'), as.numeric))
    if(!'WS' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(WS = NA)
    if(!'WD' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(WD = NA)
    if(!'Signal_1' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(Signal_1 = NA)
    if(!'Canister' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(Canister = NA)
    if(!'Temp' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(Temp = NA)
    if(!'RH' %in% names(DF_unit)) DF_unit <- DF_unit %>% mutate(RH = NA)
    DF_unit <- as.data.frame(DF_unit)
    DF_unit$BC <- DF_unit$Signal_1 - getBaseline(DF_unit$Signal_1, DF_unit$Timestamp, df = 10)
    
    filtered <-
      DF_unit %>%
      filter(Timestamp >= start_time2 & Timestamp <= end_time2)
    filtered$u <- filtered$WS * sin(2 * pi * filtered$WD/360)
    filtered$v <- filtered$WS * cos(2 * pi * filtered$WD/360)
    filtered$WD <- atan2(-1 *filtered$u, -1 *filtered$v)*180/pi + 180
    #filtered$Signal_BC <- filtered$Signal_1 - getBaseline(filtered$Signal_1, filtered$Timestamp, df = 10)
    filtered$unit <- as.character(input$calunitselect2)
    print(filtered)
    return(filtered)
  })  
  
  
  
  
  # make df for cal data unit 2
  getcaldata2 <- reactive({ # make data for QA table, from base dataset entered in data_upload (df_new)
    filtered2<- as.data.frame(getcaldata2_base())
    filtered_sum <- filtered2 %>%
      select(where(is.numeric)) %>%
      select(-any_of(c("Lat", "Long","u","v"))) %>%  
      pivot_longer(cols = everything()) %>%
      group_by(name) %>%
      dplyr::summarise(mean = mean(value, na.rm = TRUE), std = sd(value, na.rm = TRUE),
                       med = median(value, na.rm = TRUE), max = max(value, na.rm = TRUE), min = min(value, na.rm = TRUE),
                       n = (sum(!is.na(value))))
    return(filtered_sum)
  })
  
  
  draw_subcaltab <- function(){ # create output for RMD
    req(getcaldata1())
    req(getcaldata2())
    req(input$Time_Zone)
    TimeZone <- input$Time_Zone
    req(input$multinodestarttime)
    req(input$multinodeendtime)
    start_time_table <- as.POSIXct(input$multinodestarttime, tz = TimeZone)
    end_time_table <- as.POSIXct(input$multinodeendtime, tz = TimeZone)
    req(input$calunitselect1)
    req(input$calunitselect2)
    unit1 <- input$calunitselect1 
    unit2 <- input$calunitselect2
    
    caldata1 <- as.data.frame(getcaldata1())
    caldata2 <- as.data.frame(getcaldata2())
    
    # remove any rows that ended up completely NA
    caldata1[sapply(caldata1, is.infinite)] <- NA
    caldata1[caldata1 == "NaN"] <- NA
    caldata1_sub <- caldata1 %>% filter(if_any(c(mean, std, med, max,min), ~ !is.na(.)))
    
    caldata2[sapply(caldata2, is.infinite)] <- NA
    caldata2[caldata2 == "NaN"] <- NA
    caldata2_sub <- caldata2 %>% filter(if_any(c(mean, std, med, max,min), ~ !is.na(.)))
    
    #limit to rows present in both table
    caldata2_sub <- caldata2_sub[rownames(caldata1_sub),] 
    
    #xxsub <- round(caldata1_sub - caldata2_sub,2)
    tab <- merge(caldata1_sub, caldata2_sub, by = "name")
    tab <- tab[,c(1,2,8,3,9,4,10,5,11,6,12,7,13)]
    tab <- as.data.frame(tab)
    colnames(tab) <- c("Variable", 
                       str_sub(unit1,-4,-1), str_sub(unit2,-4,-1),
                       str_sub(unit1,-4,-1), str_sub(unit2,-4,-1),
                       str_sub(unit1,-4,-1), str_sub(unit2,-4,-1),
                       str_sub(unit1,-4,-1), str_sub(unit2,-4,-1),
                       str_sub(unit1,-4,-1), str_sub(unit2,-4,-1),
                       str_sub(unit1,-4,-1), str_sub(unit2,-4,-1) )
    
    tab %>%
      kbl(escape = F, caption = paste0(start_time_table, " to ", end_time_table), digits = 2, table.attr = "style='width:20%;'") %>%
      kable_classic(full_width = F, html_font = "Cambria") %>%
      kable_styling(latex_options = "HOLD_position")   %>% 
      add_header_above(header = c(" " = 1, "Mean" = 2,"St. Dev." = 2, "Median" = 2,"Maximum" = 2,"Minimum" = 2,"Count" = 2))%>%
      column_spec(1, border_right=T) %>%
      column_spec(3, border_right=T) %>%
      column_spec(5, border_right=T) %>%
      column_spec(7, border_right=T) %>%
      column_spec(9, border_right=T) %>%
      column_spec(11, border_right=T)
    
  }
  
  # table of sub caldata 1 - cal data 2
  output$draw_subcaltab <- function() { # create output for app
    draw_subcaltab()
  }
  
  
  
  
  
  
  
  
  
  start.end.time2 <- function(){ # create output for RMD
    req(getcaldata1())
    req(input$multinodestarttime)
    req(input$multinodeendtime)
    req(input$Time_Zone)
    TimeZone <- input$Time_Zone
    start_time <- as.POSIXct(input$multinodestarttime, tz = TimeZone)
    end_time <- as.POSIXct(input$multinodeendtime, tz = TimeZone)
    start.end.time <- paste0(substr(start_time, start = 11, stop = 23), " to",
                             substr(end_time, start = 11, stop = 23))
    start.end.time
  }
  
  SN1 <- function(){ # create output for RMD
    req(input$calunitselect1)
    SN1 <- input$calunitselect1
    return(SN1)
  }
  
  SN2 <- function(){ # create output for RMD
    req(input$calunitselect2)
    SN2 <- input$calunitselect2
    return(SN2)
  }
  
  date2 <- function(){ # create output for RMD
    req(input$multinodestarttime)
    req(input$Time_Zone)
    TimeZone <- input$Time_Zone
    start_time <- as.POSIXct(input$multinodestarttime, tz = TimeZone)
    date <- paste0(substr(start_time, start = 1, stop = 10))
    date
  }
  
  
  QATableID2 <- function(){ # create output for RMD
    req(input$Time_Zone)
    TimeZone <- input$Time_Zone
    start_time <- as.POSIXct(input$multinodestarttime, tz = TimeZone)
    end_time <- as.POSIXct(input$multinodeendtime, tz = TimeZone)
    start.end.time <- start.end.time2()
    Date <- date2()
    Serial.number1 <- SN1()
    Serial.number2 <- SN2()
    QA.table.ID <- paste0(Serial.number1,Serial.number2,"_", Date,"_", paste0(substr(start_time, start = 12, stop = 19), "-",
                                                                              substr(end_time, start = 12, stop = 19)))
    QATableID <- gsub("[[:punct:]]", "_", QA.table.ID)
    QATableID
  }
  
  
  
  output$multinodereport <- downloadHandler(
    filename = "multi_node_QA_Table.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "multi_node_QA_Table.Rmd") #.html
      file.copy("multi_node_QA_Table.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        draw_subcaltab = draw_subcaltab,
        SN1 = SN1(),
        SN2 = SN2(),
        date = date2(),
        start.end.time = start.end.time2(),
        OutputDate = Sys.Date(),
        RCodeVersion = "Version 1.2",
        QATableID = QATableID2(),
        Analyst = "_________________________________"
      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,output_format = "html_document",
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  

  SensorAgreement_build <- reactive({# sensor agreement plot


    # req(input$Signal_units)
    # req(input$calunit1select)
    
    req(getcaldata1_base())
    cal1 <- as.data.frame(getcaldata1_base())
    cal1 <- cal1 %>%
      select(Signal_BC, Timestamp, unit)
    req(getcaldata2_base())
    cal2 <- as.data.frame(getcaldata2_base())
    cal2 <- cal2 %>%
      select(Signal_BC, Timestamp, unit)
    df <- rbind(cal1,cal2)
    print(df)
    p <-
      plot_ly(
        cal1,
        x = ~ Timestamp,
        y = ~ Signal_BC,
        color = ~unit,
        type = "scatter",
        mode = "lines",
        showlegend = F,
        line = list(color = "darkgreen"), connectgaps = FALSE) %>%
      layout(
             yaxis = list(title = paste0( " Corrected Signal (",input$Signal_units, ")"), showgrid = FALSE, showline = TRUE, mirror=TRUE, tickfont = list(size = 22)),
             xaxis = list( showgrid = FALSE, showline = TRUE, mirror=TRUE, showticklabels = FALSE, tickfont = list(size = 22)),
             scene = list(xaxis = list(showgrid = F, showline = TRUE, mirror=TRUE),
                          yaxis = list(showgrid = F, showline = TRUE, mirror=TRUE),
                          showlegend= FALSE)
      )
    p <- p %>% layout(showlegend = FALSE)
    
   q <-
      plot_ly(
        cal2,
        x = ~ Timestamp,
        y = ~ Signal_BC,
        color = ~unit,
        type = "scatter",
        mode = "lines",
        showlegend = F,
        line = list(color = "navy"),  connectgaps = FALSE) %>%
      layout(
             yaxis = list(title = paste0(" Corrected Signal (",input$Signal_units, ")"), showgrid = FALSE, showline = TRUE, mirror=TRUE, tickfont = list(size = 22)),
             xaxis = list(type = 'Date', tickformat = "%m/%d/%y\n %H:%M", showgrid = FALSE, showline = TRUE, mirror=TRUE, tickfont = list(size = 22)),
             scene = list(xaxis = list(showgrid = F, showline = TRUE, mirror=TRUE),
                          yaxis = list(showgrid = F, showline = TRUE, mirror=TRUE),
                          showlegend= FALSE)
      ) 
   q <- q %>% layout(showlegend = FALSE)
    #q
   subplot(p,q, nrows = 2)

    
  })
  
  output$SensorAgreement_buildplot <- renderPlotly({ # baseline correction plot
    SensorAgreement_build()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui, server)   



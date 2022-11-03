# M. MacDonald ORISE, macdonald.megan@epa.gov
# App processing for Sensit SPods at 1 site:
## Rev. 1.0: 1 collocated pair of SPods supported
### Notes:
# First process data and create 10sec and 5 min output files using xx.r code
# Comparison tables can be made with xx.r code from 10 sec files


####################### Megan To-DO: #############################

## add input for raw files to 10sec

#### Step 2: auto QA
## add automatic QA into code
## add calibration QA table, connect in RDB

#### step 6: update documentation:
## make updated user manual in RMD and add to about page
## update architexture diagram
## Gayles App QAPP

## future ideas :) :
## 3-D signal viewer of a single sensor
## 3-D signal viewer of a whole area w/ multiple sites in plotly so user can interact


###################################################################


# df <- readRDS("C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/Processed Data/RDSfiles/5min_greensboro.rds")
# View(df)

##########################################################

	require(dplyr)
	require(leaflet)
	require(forcats)
	require(tidyr)
	require(collapsibleTree)
	require(cowplot)
	require(data.table)
	require(DT)
	require(ggpubr)
	require(grid)
	require(gridExtra)
	require(hexbin)
	require(kableExtra)
	require(latticeExtra)
	require(plotly)
	require(quantreg)
	require(shinycssloaders)
	require(shinydashboard)
	require(splines2)
	require(tibbletime)
	require(openair)
	require(openairmaps)

# require(shiny)
# require(readr)
# require(data.table)
# require(leaflet)
# require(shinydashboard)
# require(collapsibleTree)
# require(shinycssloaders)
# require(DT)
# require(openair)
# require(plotly)
# require(ggplot2)
# require(hexbin)
# require(DT)
# require(tidyverse)
# require(lubridate)
# require(tibbletime)
# require(grid)
# require(cowplot)
# require(latticeExtra)
# require(ggpubr)
### for RMD??
# require(tinytex)
# tinytex::install_tinytex()  ## make sure this is run the first time running the app
# require(knitr)
# require(kableExtra)
# require(gridExtra)
# require(cowplot)
# require(devtools)
#install_github("davidcarslaw/openairmaps")
# require(openairmaps)
# require(quantreg)
# require(splines2)  # replaces "splines" in R V3.4.3
# require(splines)
# # Load package - for "getBaseline" and related function
# find_rtools()
# load_all(
# #  "C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/R Code/Shiny app/detrendr"
# "detrendr"
# )


source( #be sure to use "getbaseline" from this and not detrendr!
#  "C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/R Code/Shiny app/getBaseline.R"
"getBaseline.R"
)
source( # If using this, need to edit the ScreenOffScale function so min is 0, sensits are lower-reading than Spod
#  "C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)//G-START Project_QAPP ID_0033295-QP-1-0/R Code/Shiny app/screeningFunctions.R" # edit max to 50
"screeningFunctions.R"
)
##########################################################

# set working directory
#setwd("C:/Users/mmacdo01/OneDrive - Environmental Protection Agency (EPA)/G-START Project_QAPP ID_0033295-QP-1-0/R Code/Shiny app")

# code version
version <- "1.3_2022"

########################################################### app starts here:

ui <- dashboardPage( ###################################################### build sidebar
  skin = "black", # choose panel color for app
  dashboardHeader(title = "SENTINEL"),
  dashboardSidebar(
    sidebarMenu(
      tags$a(img(src="ngemlogo.png", width = 125,
                 style="display: block; margin-left: auto; margin-right: auto;"),
             href="https://www.epa.gov/air-research/next-generation-emission-measurement-ngem-research-fugitive-air-pollution"),
      menuItem(
        "Data Upload",
        tabName = "dataupload",
        icon = icon("level-up", lib = "glyphicon")
      ),
      menuItem( # make side bar menu items
        "Dashboard",
        tabName = "home",
        icon = icon("home", lib = "glyphicon")
      ),
      menuItem(
        "Map",
        tabName = "sitemap",
        icon = icon("globe", lib = "glyphicon")
      ),
      menuItem(
        "Calibrations",
        tabName = "calibrations",
        icon = icon("wrench", lib = "glyphicon"),
        menuSubItem('Single Node',
                    tabName = 'singlenode',
                    icon = icon('chevron-right', lib = "glyphicon")),
        menuSubItem('Multiple Nodes',
                    tabName = 'multinode',
                    icon = icon('chevron-right', lib = "glyphicon"))
      ),
      menuItem(
        "Report Generator",
        tabName = "reportgenerator",
        icon = icon("picture", lib = "glyphicon")
      ),
      menuItem(
        "All Data (Table)",
        tabName = "alldatatab",
        icon = icon("th-list", lib = "glyphicon")
      ),
      menuItem(
        "All Data (Graphs)",
        tabName = "alldata",
        icon = icon("time", lib = "glyphicon"),
        dateRangeInput( # add date range for all 5 min files / plotly explorer
          "dateRange",
          label = 'Choose Date Range:',
          start = as.Date('2021-11-01') ,
          end = as.Date('2022-07-01')
        ),
        menuSubItem('Plotly Explorer',
                    tabName = 'plotlyexplorer',
                    icon = icon('chevron-right', lib = "glyphicon")),
        menuSubItem('SDI Plots',
                    tabName = 'sdiplots',
                    icon = icon('chevron-right', lib = "glyphicon"))
      ),
      menuItem(
        "About",
        tabName = "about",
        icon = icon("heart", lib = "glyphicon")
      )
    )
  ),
  dashboardBody( ################################################## build main page of app
    tabItems(###################################################### build DASHBOARD page
      tabItem(tabName = "home",
              fluidRow(
                h2("SPod Dashboard"),
                box(
                  uiOutput("checkboxspod0"),
                  height = 300,
                  width = 3
                  ),
                box(
                  uiOutput("checkboxspod"),
                  height = 300,
                  width = 3
                ),
                box(
                  sliderInput(
                    "windfilterInput",
                    "Wind Speed Filter:",
                    min = 0,
                    max = 15,
                    value = 5
                  ),
                  sliderInput(
                    "winddirRange",
                    "Wind Direction Range:",
                    min = 0,
                    max = 360,
                    value = c(0, 360)
                  ),
                  height = 300,
                  width = 6
                )),
                leafletOutput("polarmap", width = "100%", height = "700px") %>% withSpinner(color = "#0dc5c1"),
              fluidRow(box(plotOutput("dashtable"), width = 12))
      ),
      tabItem(tabName = "dataupload", ############################ build DATA UPLOAD page
      h2("Data Upload"),
      br(),
      h4(
        "Upload pre-processed 5-min .rds file using the processing script: "
      ),
      br(),
      fileInput("fileRDS",
                label = "Upload .RDS file here",
                multiple = FALSE),
      br(),
      DTOutput(outputId = "contents5")%>% withSpinner(color = "#0dc5c1")
      
      ),
      tabItem(tabName = "sitemap", ############################ build SITE MAP page
              h2("Site Map"),
              br(),
              br(),
              leafletOutput("mymap",width = "100%", height = "700px")),

      tabItem(tabName = "singlenode", ######################### build SINGLE CALIBRATION page
              h2("Calibration QA Table for 1 Unit"),
              br(),
              h4("File must have the following naming convention: "),
              div("SPOD_Data_Export_1181_2022-07-10.csv ", style = "color:blue; text-align:center; font-size:20px;"),
              h4("Where SPOD_Data_Export_ is automatically generated by Sensit Connect, 1181 is the Sensor ID, and 2022-07-10 is the date of the data collection. These values will be parsed by the processing code, so it is important to name the files in this way exactly."),
              br(),
              fileInput("file1",
                        label= h4("Upload 10 second raw .CSV file here"),
                        multiple = FALSE),
              textInput("singlenodestarttime", label = h4("Start Time input"), value = "2022-07-03 12:00:00"),  #, value = "2022-01-01 12:00:00"
              textInput("singlenodeendtime", label = h4("End Time input"), value = "2022-07-03 12:59:59"),  #, value = "2022-01-01 12:59:59"
              radioButtons("durationInput", h4("Select # of 10 second periods to QA: (6 = 1 min, 360 = 1 hour)"),
                           choices = c("6", "360"),
                           selected = "6"),
              downloadButton("singlenodereport", "Generate pdf report"),
              box(plotOutput("draw_caltab"), width = 12)),
      tabItem(tabName = "multinode", ######################### build MULTI CALIBRATION page
              h2("Calibration QA Table for 2 Collocated Units"),
              br(),
              br(),
              h4("File must have the following naming convention: "),
              div("SPOD_Data_Export_1181_2022-07-10.csv ", style = "color:blue; text-align:center; font-size:20px;"),
              h4("Where SPOD_Data_Export_ is automatically generated by Sensit Connect, 1181 is the Sensor ID, and 2022-07-10 is the date of the data collection. These values will be parsed by the processing code, so it is important to name the files in this way exactly."),
              br(),
              fileInput("file1multi",
                        label= h4("Upload 10 second processed .CSV file here"),
                        multiple = FALSE),
              fileInput("file2multi",
                        label= h4("Upload 10 second processed .CSV file here"),
                        multiple = FALSE),
              textInput("multinodestarttime", label = h4("Start Time input"), value = "2022-07-03 14:00:00"),
              textInput("multinodeendtime" , label = h4("End Time input"), value = "2022-07-03 15:59:59"),
              downloadButton("multinodereport", "Generate pdf report"),
              box(plotOutput("draw_subcaltab"), width = 12)
      ),
      tabItem(################################################ build REPORTING page
        tabName = "reportgenerator",
        fluidRow(box(
          dateRangeInput( # add date range for range of 5 min files
            "dateRangereport",
            label = 'Choose Date Range for report:',
            start = as.Date('2022-07-01') ,
            end = as.Date('2022-07-03'),
          ),
          textInput("nodereport", label = "Enter Node:", value = "SPOD1181"),
          downloadButton("exportpdf", "Report pdf"), height = 200),
          box(htmlOutput("sensorinfo", style="color:black;font-size:12px"),height = 200)),
        fluidRow(box(plotOutput("statstab"), width = 12)),
        fluidRow(box(
          plotOutput("baselineplot") %>% withSpinner(color = "#0dc5c1"),
          width = 12
        )),
        fluidRow(box(plotOutput("MDLplot")%>% withSpinner(color = "#0dc5c1"), width = 12)),
        fluidRow(box(plotOutput("WSWDplot")%>% withSpinner(color = "#0dc5c1"), width = 12)),
        fluidRow(box(plotOutput("RHtempplot")%>% withSpinner(color = "#0dc5c1"), width = 12)),
        fluidRow(box(plotOutput("densityplot")%>% withSpinner(color = "#0dc5c1"), width = 12)),
        fluidRow(box(plotOutput("SDIall")%>% withSpinner(color = "#0dc5c1"), width = 6),
                 box(plotOutput("windrose")%>% withSpinner(color = "#0dc5c1"), width = 6))
      ),
      tabItem(####################################################### build PLOTLY EXPLORER page
        tabName = "plotlyexplorer",  
        h2("Graphic Results"),
        fluidRow(box( radioButtons(
          "plotlysite",
          "Select Site:",
          choices = c("S01", "S01A", "S02", "S03"),     ##### fix this hardcoding 
          selected = " ",
          inline = T))),
        fluidRow(
          box(
            title = "  ",
            width = 6,
            selectInput(
              inputId = "variable1",
              label = "Choose variable to graph with Plotly (top graph):",
              choices = c(
                "Raw PID (ppb)" = "rawPID_ppb",
                "Raw PID (mv)" = "rawPID_mV",
                "ppb PID baseline corrected" = "bcPID_ppb",
                "detects" = "detects",
                "PID SD (daily)" = "pid.sd",
                "MDL (Daily)" = "MDL",
                "Temp (c)" = "temp",
                "Rh (%)" = "rh",
                "Pressure (mbar)" = "pressure",
                "Wind Speed (mph)" = "ws",
                "Wind Direction (deg)" = "wd",
                "S1 temp" = "s1temp",
                "S1 heat (0-255)" = "s1heat",
                "S1 set" = "set",
                "Battery Voltage (V)" = "bat_volt",
                "Charge current (ma)" = "chg.current",
                "Opperating Current (ma)" = "opp.current",
                "Trig Port Stat" = "trigportstat",
                "Trig Active Stat" = "trigactivestat",
                "Trig Active Flag" = "trigactiveflag",
                "Trig Sample Flag" = "trigsampleflag"
              )
            )
          ),
          box(
            title = "  ",
            width = 6,
            selectInput(
              inputId = "variable",
              label = "Choose variable to graph with Plotly (bottom graph):",
              choices = c(
                "Raw PID (ppb)" = "rawPID_ppb",
                "Raw PID (mv)" = "rawPID_mV",
                "ppb PID baseline corrected" = "bcPID_ppb",
                "detects" = "detects",
                "PID SD (daily)" = "pid.sd",
                "MDL (Daily)" = "MDL",
                "Temp (c)" = "temp",
                "Rh (%)" = "rh",
                "Pressure (mbar)" = "pressure",
                "Wind Speed (mph)" = "ws",
                "Wind Direction (deg)" = "wd",
                "S1 temp" = "s1temp",
                "S1 heat (0-255)" = "s1heat",
                "S1 set" = "set",
                "Battery Voltage (V)" = "bat_volt",
                "Charge current (ma)" = "chg.current",
                "Opperating Current (ma)" = "opp.current",
                "Trig Port Stat" = "trigportstat",
                "Trig Active Stat" = "trigactivestat",
                "Trig Active Flag" = "trigactiveflag",
                "Trig Sample Flag" = "trigsampleflag"
              )
            )
          )),
        box(title = "Plotly Data Explorer:", width = 12,
            plotlyOutput("plots"))),
      tabItem(############################################################# build SDI page
        tabName = "sdiplots", # make SDI data plots page
        h2("SDI Plots"),
        radioButtons("statInput", "Select Statistic:",
                     choices = c("median", "mean"),
                     selected = "median"),
        radioButtons("wsInput", "Select Wind SPeed lower limit:",
                     choices = c(0, 1, 2, 3, 4), inline=T,
                     selected = 0),
        fluidRow(
          textInput("siteSDI", label = h2("Input site"), value = "S01"),
          textInput("SDISN1", label = h2("Input unit 1"), value = "SPOD1181"),
          
          box(title = paste0( "SPOD 1"," - SDI plots (All, Below MDL, Above MDL)"), width = 4,
              plotOutput("SDIall1") %>% withSpinner(color="#0dc5c1")
          ),
          box(title = " ", width = 4,
              plotOutput("SDIbelowMDL1")%>% withSpinner(color="#0dc5c1")
          ),
          box(title = " ", width = 4,
              plotOutput("SDIaboveMDL1")%>% withSpinner(color="#0dc5c1")
          )),
        fluidRow(
          textInput("SDISN2", label = h2("Input unit 2"), value = "SPOD1261"),
          
          box(title = paste0( "SPOD 2"," - SDI plots (All, Below MDL, Above MDL)"), width = 4,
              plotOutput("SDIall2") %>% withSpinner(color="#0dc5c1")
          ),
          box(title = " ", width = 4,
              plotOutput("SDIbelowMDL2")%>% withSpinner(color="#0dc5c1")
          ),
          box(title = " ", width = 4,
              plotOutput("SDIaboveMDL2")%>% withSpinner(color="#0dc5c1")
          )
        )),
      tabItem( ##################################################### build DATA TABLE page
        tabName = "alldatatab", # make data table page
        h2("Table Results"),
        dateRangeInput(
          "dateRangetab",
          label = 'Choose Date Range:',
          start = as.Date('2021-11-01') ,
          end = as.Date('2021-11-04')
        ),
        DTOutput(outputId = "table_all"),

      ),
      tabItem( ####################################################### build ABOUT page
        tabName = "about",
        fluidRow(
          box( h3("Version 1.0 (September 2022)"),
               br(),
               h3("Megan MacDonald (ORISE)"),
               br(),
               h3("macdonald.megan@epa.gov"),
               actionButton(inputId='ab1', label="APP USER GUIDE",
                            icon = icon("th"),
                            onclick ="window.open('U R L     H E R E', '_blank')")),   ## megan add this
          box(img(src = "pic2.jpg", height = 300, width = 350))
        ),
        h3(" Web Resources:"),
        actionButton(inputId='ab1', label="Learn More about reading SDI plots",
                     icon = icon("th"),
                     onclick ="window.open('https://bookdown.org/david_carslaw/openair/', '_blank')"),
        actionButton(inputId='ab1', label="Learn More about Sensit SPods",
                     icon = icon("th"),
                     onclick ="window.open('https://gasleaksensors.com/products/sensit-spod-voc-emissions-air-pollutant-monitor/', '_blank')"),
        h3(" EPA Resources:"),
        actionButton(inputId='ab1', label="Fenceline Monitoring video",
                     icon = icon("th"),
                     onclick ="window.open('https://www.youtube.com/watch?v=jUTQrVVTYNg', '_blank')"),
        actionButton(inputId='ab1', label="Fenceline Sensors video",
                     icon = icon("th"),
                     onclick ="window.open('https://www.youtube.com/watch?v=ACFm8-WhMRU', '_blank')")
      )
    )
  )
)




server <- function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2) # extend file allowance 
  ######################################################### Data input RDS
  spod_all_5min <- reactive({
    req(input$fileRDS)
    df <- readRDS(input$fileRDS$datapath)
    df <- subset(df, df$QA == 0)
    return(df)
    })
  
  ######################################################### RDS output table
  
  output$contents5 <-  DT::renderDataTable({
    req(spod_all_5min())
    x <- as.data.frame(spod_all_5min())
    y <- x %>%
      mutate(Serial.number = factor(SN)) %>%
      group_by(site, Serial.number) %>%  
      dplyr::summarize(Count = n())
    DT::datatable(y)
  })
  
  ######################################################### dashboard reactive selection output
  output$checkboxspod0 <- renderUI({
    choice <-  unique(spod_all_5min()$site)
    checkboxGroupInput("checkboxspod0","Select Site:", choices = choice, selected = choice[1])
    
  })
  ######################################################### dashboard reactive selection output
  output$checkboxspod <- renderUI({
    choice <-  unique(spod_all_5min()$SN)
    checkboxGroupInput("checkboxspod","Select Units", choices = choice, selected = choice[1])

  })
  ######################################################### POLARMAP
  output$polarmap <- renderLeaflet({
  spodinput <- input$spodInput
  spodinput <- input$siteInput
    req(spod_all_5min())
    spod_all_5 <- as.data.frame(spod_all_5min())
   spod_all_5_1 <- subset(spod_all_5,
                          spod_all_5$site == input$checkboxspod0 &
                            spod_all_5$SN == input$checkboxspod &
                            spod_all_5$ws <= input$windfilterInput &
                            spod_all_5$wd > input$winddirRange[1] &
                            spod_all_5$wd < input$winddirRange[2])
   polarMap(spod_all_5_1,
           latitude = "lat",
           longitude = "long",
           pollutant = 'bcPID_ppb',
           type = "site",
           statistic = "median",
           provider = "Esri.WorldImagery",
           key = TRUE,
           iconWidth = 300, iconHeight = 300,
           fig.width = 5, fig.height = 5,
           par.settings=list(fontsize=list(text=25)))
  })
  #########################################################
  ######################################################### Dashboard Table

  output$dashtable <- renderPlot({
    req(spod_all_5min())
    spod_all_5 <- as.data.frame(spod_all_5min())
    xx <- spod_all_5 %>%
      dplyr::group_by(site, spod_all_5$SN ) %>%
      dplyr::summarize(
        lat = mean(lat, na.rm = T),
        long = mean(long, na.rm = T),
        count = length(bcPID_ppb),
        corrected_max_signal = round(max(bcPID_ppb, na.rm = T),2),
        corrected_min_signal = round(min(bcPID_ppb, na.rm = T),2),
        corrected_med_signal = round(median(bcPID_ppb, na.rm = T),2),
        corrected_sd_signal = round(sd(bcPID_ppb, na.rm = T),2),
        mean_MDL = round(mean(MDL, na.rm = T),2)
      )
    colnames(xx) <-  c("Site", "Serial Number","Latitude", "Longitude","5-min Datapoints", "Max. Signal","Min. Signal","Med. Signal","SD Signal",
                       "Mean MDL")
    main.title <- paste0("SPod Stats, ", min(spod_all_5$day, na.rm = T), " to ", max(spod_all_5$day, na.rm = T))
    ggtexttable(xx,rows = NULL, theme = ttheme("blank")) %>%
      tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
      tab_add_hline(at.row = 10, row.side = "bottom", linewidth = 2)%>%
      tab_add_title(text = main.title, face = "bold", padding = unit(0.2, "line"))%>%
      tab_add_footnote(text = paste0("Table generated on: ", date()), color = " dark grey")
  })
  #########################################################
  ######################################################### leaflet site map
  
  output$mymap <- renderLeaflet({ # site map
    req(spod_all_5min())
    spod_all_5 <- as.data.frame(spod_all_5min())
    t <- spod_all_5  %>%
      dplyr::group_by(site) %>%
      dplyr::summarize(
        lat = mean(lat, na.rm = T),
        long = mean(long, na.rm = T),
        count = length(bcPID_ppb),
        corrected_max_signal = max(bcPID_ppb, na.rm = T),
        corrected_min_signal = min(bcPID_ppb, na.rm = T))
    
    leaflet() %>% setView(lng = t$long[1], lat = t$lat[1], zoom = 12)%>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = t$long[1], lat = t$lat[1], zoom = 16) %>%
      addProviderTiles(providers$Stamen.TonerLines,
                       options = providerTileOptions(opacity = 0.35)) %>%
      addProviderTiles(providers$Stamen.TonerLabels) %>%
      addCircleMarkers(
        t$long[1],
        t$lat[1],
        popup = paste0(
          "Site 1",
          "\n",
          "count = ",
          t$count[1],
          "\n",
          ", max ppb = ",
          round(t$corrected_max_signal[1], 1)
        ),
        label = "Site 1"
      ) %>%
      addCircleMarkers(
        t$long[2],
        t$lat[2],
        popup = paste0(
          "Site 1A, ",
          "count = ",
          t$count[2],
          ", max ppb = ",
          round(t$corrected_max_signal[2], 1)
        ),
        label = "Site 1A"
      ) %>%
      addCircleMarkers(
        t$long[3],
        t$lat[3],
        popup = paste0(
          "Site 2, ",
          "count = ",
          t$count[3],
          ", max ppb = ",
          round(t$corrected_max_signal[3], 1)
        ),
        label = "Site 2"
      ) %>%
      addCircleMarkers(
        t$long[4],
        t$lat[4],
        popup = paste0(
          "Site 3, ",
          "count = ",
          t$count[4],
          ", max ppb = ",
          round(t$corrected_max_signal[4], 1)
        ),
        label = "Site 3"
      )
  })
  #########################################################
  ######################################################### SINGLE CALIBRATION PAGE
  getcaldata <- reactive({
    req(input$singlenodestarttime)
    req(input$singlenodeendtime)
    req(input$durationInput)
    req(input$file1)
    duration <- input$durationInput
    duration_sec <- as.numeric(duration)
    start_time <- as.POSIXct(input$singlenodestarttime, tz = "America/New_York")
    end_time <- as.POSIXct(input$singlenodeendtime, tz = "America/New_York")

    
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    } else {
      numfiles = nrow(inFile)
      filelist_10 = list()
      for (i in 1:numfiles)
      {
        dat_10 <- ######## add read in from raw data file 
          fread(
            input$file1[[i, 'datapath']], select = c(2:19), skip = 2, 
            fill = TRUE
          )
        
        Data_sensit <- dat_10
          # dat_10[, c(2:19)] #limit to needed cols
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
        ######################################################################################## TESTING HERE !!!!
        a <- input$file1$name[[i]]
        node <- str_match(a, "Export_\\s*(.*?)\\s*_20")
        ID <- node[,2]
        Data_sensit$serial.number <- paste0("SPOD", ID)  
        if (class(Data_sensit$`RawPID_ppb(ppb)`) != "numeric") {
          Data_sensit$`RawPID_ppb(ppb)` <-
            as.numeric(as.character(Data_sensit$`RawPID_ppb(ppb)`))
        }
        Data_sensit$time <- as.POSIXct(Data_sensit$time,format = "%d-%b-%Y %H:%M:%S", tz = "America/New_York")
        # pid <- Data_sensit$`RawPID_ppb(ppb)`
        if (class(Data_sensit$`RawPID_ppb(ppb)`) != "numeric") {
          Data_sensit$`RawPID_ppb(ppb)` <-
            as.numeric(as.character(Data_sensit$`RawPID_ppb(ppb)`))
        }
        # rh <- paste(node,  "RH(%)", sep=".")
        if (class(Data_sensit$`RH(%)`) != "numeric") {
          Data_sensit$`RH(%)` <- as.numeric(as.character(Data_sensit$`RH(%)`))
        }
        #if(length(which(!is.na(Data_sensit[,pid])))>0){
        # Data_sensit[,pid] <- screenOffScale(Data_sensit[,pid], Data_sensit$time) (not needed for sensit unless it appears to be railing)
        if (length(which(!is.na(Data_sensit$`RawPID_ppb(ppb)`))) > 0) {
          # screen out large RH
          Data_sensit$`RawPID_ppb(ppb)` <-
            screenRH(Data_sensit$`RawPID_ppb(ppb)`,
                     Data_sensit$time,
                     Data_sensit$`RH(%)`)
        }
        ############# Calculate and remove baseline for ppb and M
        Data_sensit_10 <-
          as.data.frame(Data_sensit) #create new df to use here
        Data_sensit_10$time <- as.POSIXct(Data_sensit_10$time)
        #mv space baseline correction # RawPID-BCmV mV (mV)
        Data_sensit_10[, 'PID_mV(mV).baseline'] <-
          getBaseline(Data_sensit_10[3], Data_sensit_10$time, df = 24)
        #ppb space baseline correction # RawPID-BCmV mV (mV)
        Data_sensit_10[, 'PID_ppb(ppb).baseline'] <-
          getBaseline(Data_sensit_10[2], Data_sensit_10$time, df = 24)
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
        Data_sensit_10 <- as.data.frame(Data_sensit_10)
      }
    }
    filtered0 <-
      Data_sensit_10 %>%
      filter(timestamp >= start_time &  timestamp <= end_time)
    filtered <- filtered0[,c(3,4,22,23,5:19)] #change to filtered version
    print(filtered)
    #no exponent notation
    options(scipen=999)
    #fix trig flag cols
    filtered[c("Trig_Port_Stat", "Trig_Sample_Flag", "Trig_Active_Stat", "Trig_Active_Flag")][is.na(filtered[c("Trig_Port_Stat", "Trig_Sample_Flag", "Trig_Active_Stat", "Trig_Active_Flag")])] <- 0
    # build QA df for output file
    Mean <- lapply(filtered, mean)
    Median <- lapply(filtered, median)
    StdDev <- lapply(filtered, sd)
    Min <- lapply(filtered, min)
    Max <- lapply(filtered, max)
    N <- lapply(filtered,function(x) sum(!is.na(x)))
    DataComp <- lapply(filtered,function(x) (sum(!is.na(x))/duration_sec)*100) #8640 for 1 hr
    #Round vals for table
    Mean <- round(as.numeric(Mean), 1)
    Median <- round(as.numeric(Median), 1)
    StdDev <- round(as.numeric(StdDev), 1)
    Min <- round(as.numeric(Min), 1)
    Max <- round(as.numeric(Max), 1)
    DataComp <- round(as.numeric(DataComp), 1)
    xx <- cbind(Mean, Median, StdDev, Min, Max, DataComp)
    rownames(xx) <-  c("RawPIDppb","RawPIDMV", "BCPIDppb","BCPIDMV","Tempdegc","RH", "Pressure","WS","WD","S1temp","S1Heat",
                       "S1Setarb","BatvoltV","ChargeCurrentmA","OperateCurrentmA", "TrigPortStat","TrigActiveStat","TrigActiveFlag","TrigSampleFlag")
    xx
  })

  output$draw_caltab <- renderPlot({ # create output for app
    req(getcaldata())
    xx <- getcaldata()
    ggtexttable(xx, theme = ttheme("blank")) %>%
      tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
      tab_add_hline(at.row = 20, row.side = "bottom", linewidth = 2)%>%
      tab_add_title(text = "test", face = "bold", padding = unit(0.2, "line"))
  })

  draw_caltab <- function(){ # create output for RMD
    req(getcaldata())
    req(input$singlenodestarttime)
    req(input$singlenodeendtime)
    start_time <- as.POSIXct(input$singlenodestarttime, tz = "America/New_York")
    end_time <- as.POSIXct(input$singlenodeendtime, tz = "America/New_York")
    xx <- getcaldata()
    xx <- as.data.frame(xx)
    xx$Mean[1] = cell_spec(xx$Mean[1], color = ifelse(xx$Mean[1] < 200, "black", "red"))
    xx$Mean[2] = cell_spec(xx$Mean[2], color = ifelse(xx$Mean[2] < 300, "black", "red"))
    xx$Mean[3] = cell_spec(xx$Mean[3], color = ifelse(xx$Mean[3] > -1, "black", "red"))
    xx$Mean[4] = cell_spec(xx$Mean[4], color = ifelse(xx$Mean[4] > -1 , "black", "red"))
    # xx$StdDev[1] = cell_spec(xx$StdDev[1], color = ifelse(xx$StdDev[1] < 20 | xx$StdDev[1] == "NA", "black", "red"))
    # xx$StdDev[2] = cell_spec(xx$StdDev[2], color = ifelse(xx$StdDev[2] < 30 | xx$StdDev[1] == "NA", "black", "red"))
    # xx$StdDev[3] = cell_spec(xx$StdDev[3], color = ifelse(xx$StdDev[3] > 5 | xx$StdDev[1] == "NA" , "red", "black"))
    # xx$StdDev[4] = cell_spec(xx$StdDev[4], color = ifelse(xx$StdDev[4] > 5 | xx$StdDev[1] == "NA" , "red", "black"))
    xx$Min[1] = cell_spec(xx$Min[1], color = ifelse(xx$Min[1] > 5 , "red", "black"))
    xx$Min[2] = cell_spec(xx$Min[2], color = ifelse(xx$Min[2] > 10 , "red", "black"))
    xx$Min[3] = cell_spec(xx$Min[3], color = ifelse(xx$Min[3] < -2 , "red", "black"))
    xx$Min[4] = cell_spec(xx$Min[4], color = ifelse(xx$Min[4] < -2 , "red", "black"))
    xx$Max[1] = cell_spec(xx$Max[1], color = ifelse(xx$Max[1] > 200 , "red", "black"))
    xx$Max[2] = cell_spec(xx$Max[2], color = ifelse(xx$Max[2] > 400 , "red", "black"))
    xx$Max[3] = cell_spec(xx$Max[3], color = ifelse(xx$Max[3] > 200 , "red", "black"))
    xx$Max[4] = cell_spec(xx$Max[4], color = ifelse(xx$Max[4] > 400 , "red", "black"))
    xx$DataComp = cell_spec(xx$DataComp, color = ifelse(xx$DataComp != 100 , "red", "black"))

    xx <- xx[c("Mean","Median","StdDev", "Min", "Max","DataComp")]

    xx %>%
      kbl(escape = F, caption = paste0(start_time, " to ", end_time), digits = 2, table.attr = "style='width:20%;'")%>%
      kable_classic(full_width = F, html_font = "Cambria") %>%
      kable_styling(latex_options = "HOLD_position")
  }

  start.end.time_1 <- function(){ # create output for RMD
    req(getcaldata())
    req(input$singlenodestarttime)
    req(input$singlenodeendtime)
    start_time <- as.POSIXct(input$singlenodestarttime, tz = "America/New_York")
    end_time <- as.POSIXct(input$singlenodeendtime, tz = "America/New_York")
    start.end.time <- paste0(substr(start_time, start = 11, stop = 23), " to",
                             substr(end_time, start = 11, stop = 23))
    start.end.time
  }

  SN_1 <- function(){ # create output for RMD
    req(input$file1)
      inFile <- input$file1
      if (is.null(inFile)){
        return(NULL)
      } else {
        numfiles = nrow(inFile)
        filelist_10 = list()
        for (i in 1:numfiles)
        {
          Data_sensit <- fread(input$file1[[i, 'datapath']], fill = TRUE)}}
      a <- input$file1$name[[i]]
      node <- str_match(a, "Export_\\s*(.*?)\\s*_20")
      ID <- node[,2]
      SN <- paste0("SPOD", ID)
    return(SN)
  }

  date_1 <- function(){ # create output for RMD
    req(input$singlenodestarttime)
    start_time <- as.POSIXct(input$singlenodestarttime, tz = "America/New_York")
    date <- paste0(substr(start_time, start = 1, stop = 10))
    date
  }

  QATableID_1 <- function(){ # create output for RMD
    req(input$singlenodestarttime)
    req(input$singlenodeendtime)
    start_time <- as.POSIXct(input$singlenodestarttime, tz = "America/New_York")
    end_time <- as.POSIXct(input$singlenodeendtime, tz = "America/New_York")
    start.end.time <- start.end.time_1()
    Date <- date_1()
    Serial.number <- SN_1()
    QA.table.ID <- paste0(Serial.number,"_", Date,"_", paste0(substr(start_time, start = 12, stop = 19), "-",
                                                              substr(end_time, start = 12, stop = 19)))
    print(QA.table.ID)
    QATableID <- gsub("[[:punct:]]", "_", QA.table.ID)
    print(QATableID)
    QATableID
  }

  output$singlenodereport <- downloadHandler(
    filename = "single_node_QA_Table.pdf",
    content = function(file) {
      res <- rmarkdown::render(
        "single_node_QA_Table.Rmd",
        params = list(
          draw_caltab = draw_caltab,
          SN_1 = SN_1(),
          date_1 = date_1(),
          start.end.time_1 = start.end.time_1(),
          OutputDate = Sys.Date(),
          RCodeVersion = "Version 1.2",
          QATableID_1 = QATableID_1(),
          Analyst = "_________________________________"
        )
      )
      file.rename(res, file)
    }
  )
  #########################################################
  ######################################################### MULTI NODE  CALIBRATION

  # get cal data 1
  getcaldata1 <- reactive({
    req(input$multinodestarttime)
    req(input$multinodeendtime)
    req(input$file1multi)
      start_time <- as.POSIXct(input$multinodestarttime, tz = "America/New_York")
      end_time <- as.POSIXct(input$multinodeendtime, tz = "America/New_York")
      inFile <- input$file1multi
      if (is.null(inFile)) {
        return(NULL)
      } else {
        numfiles = nrow(inFile)
        filelist_10 = list()
        for (i in 1:numfiles)
        {
          dat_10 <- ######## add read in from raw data file 
            fread(
              input$file1multi[[i, 'datapath']], select = c(2:19), skip = 2, 
              fill = TRUE
            )
          
          Data_sensit <- dat_10
          # dat_10[, c(2:19)] #limit to needed cols
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
          ######################################################################################## TESTING HERE !!!!
          a <- input$file1multi$name[[i]]
          node <- str_match(a, "Export_\\s*(.*?)\\s*_20")
          ID <- node[,2]
          Data_sensit$serial.number <- paste0("SPOD", ID)  
          if (class(Data_sensit$`RawPID_ppb(ppb)`) != "numeric") {
            Data_sensit$`RawPID_ppb(ppb)` <-
              as.numeric(as.character(Data_sensit$`RawPID_ppb(ppb)`))
          }
          Data_sensit$time <- as.POSIXct(Data_sensit$time,format = "%d-%b-%Y %H:%M:%S", tz = "America/New_York")
          # pid <- Data_sensit$`RawPID_ppb(ppb)`
          if (class(Data_sensit$`RawPID_ppb(ppb)`) != "numeric") {
            Data_sensit$`RawPID_ppb(ppb)` <-
              as.numeric(as.character(Data_sensit$`RawPID_ppb(ppb)`))
          }
          # rh <- paste(node,  "RH(%)", sep=".")
          if (class(Data_sensit$`RH(%)`) != "numeric") {
            Data_sensit$`RH(%)` <- as.numeric(as.character(Data_sensit$`RH(%)`))
          }
          #if(length(which(!is.na(Data_sensit[,pid])))>0){
          # Data_sensit[,pid] <- screenOffScale(Data_sensit[,pid], Data_sensit$time) (not needed for sensit unless it appears to be railing)
          if (length(which(!is.na(Data_sensit$`RawPID_ppb(ppb)`))) > 0) {
            # screen out large RH
            Data_sensit$`RawPID_ppb(ppb)` <-
              screenRH(Data_sensit$`RawPID_ppb(ppb)`,
                       Data_sensit$time,
                       Data_sensit$`RH(%)`)
          }
          ############# Calculate and remove baseline for ppb and M
          Data_sensit_10 <-
            as.data.frame(Data_sensit) #create new df to use here
          Data_sensit_10$time <- as.POSIXct(Data_sensit_10$time)
          #mv space baseline correction # RawPID-BCmV mV (mV)
          Data_sensit_10[, 'PID_mV(mV).baseline'] <-
            getBaseline(Data_sensit_10[3], Data_sensit_10$time, df = 24)
          #ppb space baseline correction # RawPID-BCmV mV (mV)
          Data_sensit_10[, 'PID_ppb(ppb).baseline'] <-
            getBaseline(Data_sensit_10[2], Data_sensit_10$time, df = 24)
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
          Data_sensit_10 <- as.data.frame(Data_sensit_10)
        }
      }
      filtered0 <-
        Data_sensit_10 %>%
        filter(timestamp >= start_time &  timestamp <= end_time)
      filtered <- filtered0[,c(3,4,22,23,5:19)] #change to filtered version
      print(filtered)
      #no exponent notation
      options(scipen=999)
      #fix trig flag cols
      filtered[c("Trig_Port_Stat", "Trig_Sample_Flag", "Trig_Active_Stat", "Trig_Active_Flag")][is.na(filtered[c("Trig_Port_Stat", "Trig_Sample_Flag", "Trig_Active_Stat", "Trig_Active_Flag")])] <- 0
      # build QA df for output file
      Mean <- lapply(filtered, mean)
      Median <- lapply(filtered, median)
      StdDev <- lapply(filtered, sd)
      Min <- lapply(filtered, min)
      Max <- lapply(filtered, max)
      N <- lapply(filtered,function(x) sum(!is.na(x)))
      DataComp <- lapply(filtered,function(x) (sum(!is.na(x))/360)*100) #8640 for 1 hr
      #Round vals for table
      Mean <- round(as.numeric(Mean), 1)
      Median <- round(as.numeric(Median), 1)
      StdDev <- round(as.numeric(StdDev), 1)
      Min <- round(as.numeric(Min), 1)
      Max <- round(as.numeric(Max), 1)
      DataComp <- round(as.numeric(DataComp), 1)
      xx <- cbind(Mean, Median, StdDev, Min, Max, DataComp)
      rownames(xx) <-  c("RawPIDppb","RawPIDMV", "BCPIDppb","BCPIDMV","Tempdegc","RH", "Pressure","WS","WD","S1temp","S1Heat",
                         "S1Setarb","BatvoltV","ChargeCurrentmA","OperateCurrentmA", "TrigPortStat","TrigActiveStat","TrigActiveFlag","TrigSampleFlag")
      xx
    })


  # make table for node 2 input
  getcaldata2 <- reactive({
    req(input$multinodestarttime)
    req(input$multinodeendtime)
    req(input$file2multi)
    start_time <- as.POSIXct(input$multinodestarttime, tz = "America/New_York")
    end_time <- as.POSIXct(input$multinodeendtime, tz = "America/New_York")
    inFile <- input$file2multi
    if (is.null(inFile)) {
      return(NULL)
    } else {
      numfiles = nrow(inFile)
      filelist_10 = list()
      for (i in 1:numfiles)
      {
        dat_10 <- ######## add read in from raw data file 
          fread(
            input$file2multi[[i, 'datapath']], select = c(2:19), skip = 2, 
            fill = TRUE
          )
        
        Data_sensit <- dat_10
        # dat_10[, c(2:19)] #limit to needed cols
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
        ######################################################################################## TESTING HERE !!!!
        a <- input$file2multi$name[[i]]
        node <- str_match(a, "Export_\\s*(.*?)\\s*_20")
        ID <- node[,2]
        Data_sensit$serial.number <- paste0("SPOD", ID)  
        if (class(Data_sensit$`RawPID_ppb(ppb)`) != "numeric") {
          Data_sensit$`RawPID_ppb(ppb)` <-
            as.numeric(as.character(Data_sensit$`RawPID_ppb(ppb)`))
        }
        Data_sensit$time <- as.POSIXct(Data_sensit$time,format = "%d-%b-%Y %H:%M:%S", tz = "America/New_York")
        # pid <- Data_sensit$`RawPID_ppb(ppb)`
        if (class(Data_sensit$`RawPID_ppb(ppb)`) != "numeric") {
          Data_sensit$`RawPID_ppb(ppb)` <-
            as.numeric(as.character(Data_sensit$`RawPID_ppb(ppb)`))
        }
        # rh <- paste(node,  "RH(%)", sep=".")
        if (class(Data_sensit$`RH(%)`) != "numeric") {
          Data_sensit$`RH(%)` <- as.numeric(as.character(Data_sensit$`RH(%)`))
        }
        #if(length(which(!is.na(Data_sensit[,pid])))>0){
        # Data_sensit[,pid] <- screenOffScale(Data_sensit[,pid], Data_sensit$time) (not needed for sensit unless it appears to be railing)
        if (length(which(!is.na(Data_sensit$`RawPID_ppb(ppb)`))) > 0) {
          # screen out large RH
          Data_sensit$`RawPID_ppb(ppb)` <-
            screenRH(Data_sensit$`RawPID_ppb(ppb)`,
                     Data_sensit$time,
                     Data_sensit$`RH(%)`)
        }
        ############# Calculate and remove baseline for ppb and M
        Data_sensit_10 <-
          as.data.frame(Data_sensit) #create new df to use here
        Data_sensit_10$time <- as.POSIXct(Data_sensit_10$time)
        #mv space baseline correction # RawPID-BCmV mV (mV)
        Data_sensit_10[, 'PID_mV(mV).baseline'] <-
          getBaseline(Data_sensit_10[3], Data_sensit_10$time, df = 24)
        #ppb space baseline correction # RawPID-BCmV mV (mV)
        Data_sensit_10[, 'PID_ppb(ppb).baseline'] <-
          getBaseline(Data_sensit_10[2], Data_sensit_10$time, df = 24)
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
        Data_sensit_10 <- as.data.frame(Data_sensit_10)
      }
    }
    filtered0 <-
      Data_sensit_10 %>%
      filter(timestamp >= start_time &  timestamp <= end_time)
    filtered <- filtered0[,c(3,4,22,23,5:19)] #change to filtered version
    print(filtered)
    #no exponent notation
    options(scipen=999)
    #fix trig flag cols
    filtered[c("Trig_Port_Stat", "Trig_Sample_Flag", "Trig_Active_Stat", "Trig_Active_Flag")][is.na(filtered[c("Trig_Port_Stat", "Trig_Sample_Flag", "Trig_Active_Stat", "Trig_Active_Flag")])] <- 0
    # build QA df for output file
    Mean <- lapply(filtered, mean)
    Median <- lapply(filtered, median)
    StdDev <- lapply(filtered, sd)
    Min <- lapply(filtered, min)
    Max <- lapply(filtered, max)
    N <- lapply(filtered,function(x) sum(!is.na(x)))
    DataComp <- lapply(filtered,function(x) (sum(!is.na(x))/360)*100) #8640 for 1 hr
    #Round vals for table
    Mean <- round(as.numeric(Mean), 1)
    Median <- round(as.numeric(Median), 1)
    StdDev <- round(as.numeric(StdDev), 1)
    Min <- round(as.numeric(Min), 1)
    Max <- round(as.numeric(Max), 1)
    DataComp <- round(as.numeric(DataComp), 1)
    xx <- cbind(Mean, Median, StdDev, Min, Max, DataComp)
    rownames(xx) <-  c("RawPIDppb","RawPIDMV", "BCPIDppb","BCPIDMV","Tempdegc","RH", "Pressure","WS","WD","S1temp","S1Heat",
                       "S1Setarb","BatvoltV","ChargeCurrentmA","OperateCurrentmA", "TrigPortStat","TrigActiveStat","TrigActiveFlag","TrigSampleFlag")
    xx
  })


  # table of sub caldata 1 - cal data 2
  output$draw_subcaltab <- renderPlot({ # create output for app
    req(getcaldata1())
    req(getcaldata2())
    xx1 <- getcaldata1()
    xx2 <- getcaldata2()
    xxsub <- round(xx1 - xx2,2)
    ggtexttable(xxsub, theme = ttheme("blank")) %>%
      tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
      tab_add_hline(at.row = 20, row.side = "bottom", linewidth = 2)%>%
      tab_add_title(text = "test", face = "bold", padding = unit(0.2, "line"))
  })

  draw_subcaltab <- function(){ # create output for RMD
    req(getcaldata1())
    req(getcaldata2())
    req(input$multinodestarttime)
    req(input$multinodeendtime)
    start_time <- as.POSIXct(input$multinodestarttime, tz = "America/New_York")
    end_time <- as.POSIXct(input$multinodeendtime, tz = "America/New_York")
    xx1 <- getcaldata1()
    xx2 <- getcaldata2()
    xxsub <- xx1 - xx2
    xxsub %>%
      kbl(caption = paste0(start_time, " to ", end_time), digits = 2, table.attr = "style='width:20%;'") %>%
      kable_classic(full_width = F, html_font = "Cambria") %>%
      kable_styling(latex_options = "HOLD_position")
  }

  start.end.time2 <- function(){ # create output for RMD
    req(getcaldata1())
    start_time <- as.POSIXct(input$multinodestarttime, tz = "America/New_York")
    end_time <- as.POSIXct(input$multinodeendtime, tz = "America/New_York")
    start.end.time <- paste0(substr(start_time, start = 11, stop = 23), " to",
                             substr(end_time, start = 11, stop = 23))
    start.end.time
  }

  SN1 <- function(){ # create output for RMD
    req(input$file1multi)
    inFile <- input$file1multi
    if (is.null(inFile)){
      return(NULL)
    } else {
      numfiles = nrow(inFile)
      filelist_10 = list()
      for (i in 1:numfiles)
      {
        Data_sensit <- fread(input$file1multi[[i, 'datapath']], fill = TRUE)}}
    a <- input$file1multi$name[[i]]
    node <- str_match(a, "Export_\\s*(.*?)\\s*_20")
    ID <- node[,2]
    SN <- paste0("SPOD", ID)
    return(SN)
  }

  SN2 <- function(){ # create output for RMD
    req(input$file2multi)
    inFile <- input$file2multi
    if (is.null(inFile)){
      return(NULL)
    } else {
      numfiles = nrow(inFile)
      filelist_10 = list()
      for (i in 1:numfiles)
      {
        Data_sensit <- fread(input$file2multi[[i, 'datapath']], fill = TRUE)}}
    a <- input$file2multi$name[[i]]
    node <- str_match(a, "Export_\\s*(.*?)\\s*_20")
    ID <- node[,2]
    SN <- paste0("SPOD", ID)
    return(SN)
  }

  date2 <- function(){ # create output for RMD
    start_time <- as.POSIXct(input$multinodestarttime, tz = "America/New_York")
    date <- paste0(substr(start_time, start = 1, stop = 10))
    date
  }

  QATableID2 <- function(){ # create output for RMD
    start_time <- as.POSIXct(input$multinodestarttime, tz = "America/New_York")
    end_time <- as.POSIXct(input$multinodeendtime, tz = "America/New_York")
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
    filename = "multi_node_QA_Table.pdf",
    content = function(file) {
      res <- rmarkdown::render(
        "multi_node_QA_Table.Rmd",
        params = list(
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
      )
      file.rename(res, file)
    }
  )

  #########################################################
  ######################################################### daily report builder
  vals <- reactiveValues(p1=NULL, p2 = NULL, p3 = NULL, p4 = NULL,
                         p5 = NULL, p6 = NULL, p7 = NULL, p8 = NULL) # build empty df for final report 

  spod_reportdf <- reactive({
    # complete dataset build
    req(spod_all_5min())
    spod_all_5 <- as.data.frame(spod_all_5min())
    df <- spod_all_5 %>%
      filter(day >= input$dateRangereport[1] &
               day <= input$dateRangereport[2],
             SN == input$nodereport)
    df$timeCut <- as.POSIXct(df$timeCut)
    df
  })

  # daily outputs
  output$sensorinfo <- renderUI({
    l0 <- ("SPod Info:")
    l1 <- paste0("Site: ", spod_reportdf()[[1, 29]])
    l2 <- paste0("latitude: (", spod_reportdf()[[1, 27]], ")")
    l3 <- paste0("longitude: (", spod_reportdf()[[1, 28]], ")")
    l4 <- paste0("Sensor ID: ", spod_reportdf()[[1, 1]])
    l5 <-
      paste0(
        'Date Range: ',
        min(spod_reportdf()$timeCut, na.rm = T),
        " to ",
        max(spod_reportdf()$timeCut, na.rm = T)
      )
    l6 <- paste0("Code Version: ", version)
    l7 <- paste0("Access Date: ", Sys.Date())
    l8 <- paste0("Collocation ID: ", spod_reportdf()[[1, 30]])
    t <-  HTML(paste("<b>", l0, l1, l2, l3, l4, l5, l6, l7, l8, sep = '<br/>'))
    text <- paste("SENTINEL SPod Report:",
                  l1,l2,l3,l4,l5,l6,l7,l8, sep = "\n")
    vals$p0 <- textGrob(text)
    t
  })

  output$statstab <- renderPlot({
    X1 <- spod_reportdf()[, c(3, 5, 10, 11, 12, 15:26)]
    #no exponent notation
    options(scipen = 999)
    #fix trig flag cols
    X1[c("trigportstat",
         "trigsampleflag",
         "trigactivestat",
         "trigactiveflag")][is.na(X1[c("trigportstat",
                                       "trigsampleflag",
                                       "trigactivestat",
                                       "trigactiveflag")])] <- 0
    list(colnames(X1))
    # build QA df for output file
    Mean <- lapply(na.omit(X1), mean)
    Median <- lapply(na.omit(X1), median)
    Std.Dev <- lapply(na.omit(X1), sd)
    Min <- lapply(na.omit(X1), min)
    Max <- lapply(na.omit(X1), max)
    #Round vals for table
    Mean <- round(as.numeric(Mean), 1)
    Median <- round(as.numeric(Median), 1)
    Std.Dev <- round(as.numeric(Std.Dev), 1)
    Min <- round(as.numeric(Min), 1)
    Max <- round(as.numeric(Max), 1)
    xx <- cbind(Mean, Median, Std.Dev, Min, Max)
    rownames(xx) <-
      c(
        "RawPID(ppb)",
        "BCPID(ppb)",
        "Temp(deg_c)",
        "RH(%)",
        "Pressure(mbar)",
        "WS(mph)",
        "WD(deg)",
        "S1_temp(arb)",
        "S1_Heat(0-255)",
        "S1_Set(arb)",
        "Bat_volt(V)",
        "Charge Current(mA)",
        "Operate Current(mA)",
        "Trig Port Stat",
        "Trig Active Stat",
        "Trig Active Flag",
        "Trig Sample Flag"
      )
    main.title <-
      (paste0(
        "sensor = ",
        spod_reportdf()[[1, 1]],
        ", date = ",
        min(spod_reportdf()$timeCut, na.rm = T),
        " to ",
        max(spod_reportdf()$timeCut, na.rm = T)
      ))
    vals$p1 <- ggtexttable(xx, theme = ttheme("blank")) %>%
      tab_add_hline(at.row = 1:2,
                    row.side = "top",
                    linewidth = 2) %>%
      tab_add_hline(at.row = 18,
                    row.side = "bottom",
                    linewidth = 2) %>%
     tab_add_title(text = main.title, face = "bold", padding = unit(0.2, "line"), size = 11)
    vals$p1
  })

  output$RHtempplot <- renderPlot({
    my.settings <- list(par.main.text = list(
      font = 1,
      just = "left",
      cex = 0.8,
      x = grid::unit(5, "mm")
    ))
    obj1 <-
      xyplot(
        `rh` ~ (timeCut + lubridate::hours(5)),
        spod_reportdf(),
        xlab = "Time",
        type = "l",
        par.settings = my.settings,
        main = bquote(atop(
          textstyle(
            Temperature ~ and ~ Relative ~ Humidity ~ (5 ~ min) ~  ~  ~  ~  ~  ~  ~
              ~  ~  ~  ~  ~  ~  ~  ~ "                                                         "
          ),
          textstyle(
            "site= " ~ .(spod_reportdf()[[1, 29]]) * ", lat/long= ("  ~ .(spod_reportdf()[[1, 27]]) *  " , "  ~ .(spod_reportdf()[[1, 28]]) *  "), sensor= " ~ .(spod_reportdf()[[1, 1]]) * ', date= ' * .(as.character(paste0(
              min(spod_reportdf()$timeCut, na.rm = T),
              " to ",
              max(spod_reportdf()$timeCut, na.rm = T)
            )))
          )
        ))
      )
    obj2 <-
      xyplot(
        `temp` ~ (timeCut + lubridate::hours(5)),
        spod_reportdf(),
        type = "l",
        xlab = "time"
      )
    vals$p2 <- doubleYScale(obj1, obj2, add.ylab2 = TRUE)
    vals$p2
  })
  output$MDLplot <- renderPlot({
    vals$p3 <- ggplot(spod_reportdf(), aes(x = as.POSIXct(timeCut))) +
      geom_line(aes(y = `bcPID_ppb`), color = "black") +
      geom_line(aes(y = `MDL`), color = "red") + theme_bw() + labs(
        y = "Corrected PID (ppb)",
        x = "Time",
        title = "Baseline Corrected PID (5 min) and MDL (5 min) ",
        subtitle = paste0(
          "site= ",
          spod_reportdf()[[1, 29]],
          ", lat/long= (",
          spod_reportdf()[[1, 27]],
          " , ",
          spod_reportdf()[[1, 28]],
          "), sensor= ",
          spod_reportdf()[[1, 1]],
          ', date= ',
          paste0(
            min(spod_reportdf()$timeCut, na.rm = T),
            " to ",
            max(spod_reportdf()$timeCut, na.rm = T)
          )
        )
      ) + theme(plot.title=element_text(size=10))+
          theme(plot.subtitle=element_text(size=9))
    vals$p3
  })
  output$baselineplot <- renderPlot({
    vals$p4 <-  ggplot(spod_reportdf(), aes(x = as.POSIXct(timeCut))) +
      geom_line(aes(y = (`rawPID_ppb` - `bcPID_ppb`)), color = "red") +
      geom_line(aes(y = `rawPID_ppb`), color = "black") + theme_bw() +
      labs(
        y = "PID (ppb)",
        x = "Time",
        title = "Raw PID data and Baseline (5 min)",
        subtitle = paste0(
          "site= ",
          spod_reportdf()[[1, 29]],
          ", lat/long= (",
          spod_reportdf()[[1, 27]],
          " , ",
          spod_reportdf()[[1, 28]],
          "), sensor= ",
          spod_reportdf()[[1, 1]],
          ', date= ',
          paste0(
            min(spod_reportdf()$timeCut, na.rm = T),
            " to ",
            max(spod_reportdf()$timeCut, na.rm = T)
          )
        )
      )+ theme(plot.title=element_text(size=10))+
      theme(plot.subtitle=element_text(size=9))
    vals$p4
  })
  output$WSWDplot <- renderPlot({
    x <-
      ggplot(spod_reportdf(), aes(x = as.POSIXct(timeCut), y = `ws`)) +
      geom_point(size = 0.6,
                 stroke = 0,
                 shape = 16) + theme_bw() + labs(
                   title = "Wind Speed and Wind Direction (5 min)",
                   subtitle = paste0(
                     "site= ",
                     spod_reportdf()[[1, 29]],
                     ", lat/long= (",
                     spod_reportdf()[[1, 27]],
                     " , ",
                     spod_reportdf()[[1, 28]],
                     "), sensor= ",
                     spod_reportdf()[[1, 1]],
                     ', date= ',
                     paste0(
                       min(spod_reportdf()$timeCut, na.rm = T),
                       " to ",
                       max(spod_reportdf()$timeCut, na.rm = T)
                     )
                   )
                 ) +
      xlab("")+ theme(plot.title=element_text(size=10))+
      theme(plot.subtitle=element_text(size=9))
    y <-
      ggplot(spod_reportdf(), aes(x = as.POSIXct(timeCut), y = `wd`)) +
      geom_point(size = 0.6,
                 stroke = 0,
                 shape = 16) + theme_bw() + xlab("Time")
   vals$p5 <-  plot_grid(
      x,
      y,
      align = "hv",
      ncol = 1,
      rel_heights = c(1 / 2, 1 / 2)
    )
   vals$p5
  })
  output$densityplot <- renderPlot({
  vals$p6 <-  ggplot(subset(spod_reportdf(), `bcPID_ppb` > `MDL`),
           aes(x = `wd`, y = `ws`)) +
      geom_hex() + theme_bw() + labs(
        title = "Data Density of points above MDL (5 min)",
        subtitle = paste0(
          "site= ",
          spod_reportdf()[[1, 29]],
          ", lat/long= (",
          spod_reportdf()[[1, 27]],
          " , ",
          spod_reportdf()[[1, 28]],
          "), sensor= ",
          spod_reportdf()[[1, 1]],
          ', date= ',
          paste0(
            min(spod_reportdf()$timeCut, na.rm = T),
            " to ",
            max(spod_reportdf()$timeCut, na.rm = T)
          )
        )
      )+ theme(plot.title=element_text(size=10))+
    theme(plot.subtitle=element_text(size=9)) +
      xlab("Wind Direction (deg)") + ylab("Wind Speed (m/s)")  + scale_x_continuous(breaks = seq(0, 360, by = 90))
  vals$p6
  })
  output$windrose <- renderPlot({
    my.settings <- list(par.main.text = list(
      font = 1,
      just = "left",
      cex = 0.9,
      x = grid::unit(5, "mm")
    ))
   w <-  windRose(
      spod_reportdf(),
      paddle = FALSE,
      breaks = c(0, 1, 2, 3, 4, 7, 10),
      cols = c("grey90", "grey80", "grey60", "grey40", "grey20", "black"),
      par.settings = my.settings,
      main = paste0(
        "SDI, all 5 min data, date range= ",
        paste0(
          min(spod_reportdf()$timeCut, na.rm = T),
          " to ",
          max(spod_reportdf()$timeCut, na.rm = T)
        ),
        "\n",
        "site= ",
        spod_reportdf()[[1, 29]],
        ", lat/long= (",
        spod_reportdf()[[1, 27]],
        " , ",
        spod_reportdf()[[1, 28]],
        "), sensor= ",
        spod_reportdf()[[1, 1]]
      ),
      key.position = "right"
    )
   r <- w$plot
   vals$p7 <- r
  })
  output$SDIall <- renderPlot({
    my.settings <- list(par.main.text = list(
      font = 1,
      just = "left",
      cex = 0.9,
      x = grid::unit(5, "mm")
    ))
   p <-  polarPlot(
      spod_reportdf(),
      pollutant = "bcPID_ppb",
      statistic = "mean",
      key.position = "right",
      par.settings = my.settings,
      main = paste0(
        "SDI, all 5 min data, date range= ",
        paste0(
          min(spod_reportdf()$timeCut, na.rm = T),
          " to ",
          max(spod_reportdf()$timeCut, na.rm = T)
        ),
        "\n",
        "site= ",
        spod_reportdf()[[1, 29]],
        ", lat/long= (",
        spod_reportdf()[[1, 27]],
        " , ",
        spod_reportdf()[[1, 28]],
        "), sensor= ",
        spod_reportdf()[[1, 1]]
      )
    )
   r <- p$plot
   vals$p8 <- r
  })

  output$exportpdf = downloadHandler(
    filename = function() {"plots.pdf"},
    content = function(file) {
      pdf(file, onefile = TRUE)
     grid.arrange(vals$p0)
     grid.arrange(vals$p1)
     grid.arrange(vals$p4, vals$p3)
     grid.arrange(vals$p2, vals$p5)
     grid.arrange(vals$p6)
     grid.arrange(vals$p7, vals$p8, ncol = 1, nrow = 2)

      dev.off()
    }
  )
  #########################################################
  ######################################################### PLOTLY EXPLORER PAGE
  output$plots <- renderPlotly({
    req(input$variable1)
    req(input$variable)
    req(input$plotlysite)
    req(spod_all_5min())
    spod_all_5 <- as.data.frame(spod_all_5min())
    plotly_dat <-
      subset(spod_all_5, spod_all_5$site == input$plotlysite)
    p <-
      plot_ly(
        plotly_dat,
        x = ~ timeCut,
        y = ~ get(input$variable1),
        color = ~ SN,
        colors = "Dark2", type = "scatter",
        mode = "markers",  showlegend = T) %>%
      layout(showlegend = T,
             yaxis = list(title = paste(" ", input$variable1)),
             legend = list(
               orientation = "h",
               x = 0.3,
               y = -0.2
             ),
             xaxis = list(type = 'date', tickformat = "%m/%d/%y")
      )
    q <-
      plot_ly(
        plotly_dat,
        x = ~ timeCut,
        y = ~ get(input$variable),
        color = ~ SN,
        colors = "Dark2", type = "scatter",
        mode = "markers",  showlegend = F) %>%
      layout(
        yaxis = list(title = paste(" ", input$variable)),
        xaxis = list(type = 'date', tickformat = "%m/%d/%y")
      )
    subplot(ggplotly(p),
            ggplotly(q),
            nrows = 2,
            shareX = TRUE)
  })
  #########################################################
  ######################################################### SDI plots
  spod_comp_1 <- reactive({  # Node 1
    req(input$siteSDI)
    req(input$SDISN1)  
    req(spod_all_5min())
    spod_all_5 <- as.data.frame(spod_all_5min())
    spod_all_5 %>% #spod_all %>%
      filter(day >= input$dateRange[1] & day <= input$dateRange[2],
             site == input$siteSDI, ws >= input$wsInput,
             SN == input$SDISN1)
  })
 
  spod_comp_2 <- reactive({ # Node 2
    req(input$SDISN2)
    req(input$siteSDI)
    req(spod_all_5min())
    spod_all_5 <- as.data.frame(spod_all_5min())
    spod_all_5 %>% #spod_all %>%
      filter(day >= input$dateRange[1] & day <= input$dateRange[2],
             site == input$siteSDI, ws >= input$wsInput,
             SN == input$SDISN2)
  })
  
  # SDI plots:
  output$SDIall1 <- renderPlot({
    stat <- input$statInput
    polarPlot(spod_comp_1(), pollutant = "bcPID_ppb",fontsize = 20,
              statistic = stat, main = NULL, key.position = "right", upper = 8, ws.int = 1,
              key.footer = NULL,limits = c(0, 50)) #,breaks = c(0, 50, 40, 60, 80, 1000))
  })
  output$SDIaboveMDL1 <- renderPlot({
    stat <- input$statInput
    polarPlot(subset(spod_comp_1(), bcPID_ppb > MDL), pollutant = "bcPID_ppb",  fontsize = 20,
              statistic = stat, main = NULL, key.position = "right", upper = 8,ws.int = 1,
              key.footer = NULL,limits = c(0, 50)) #,breaks = c(0, 20, 40, 60, 80, 1000))
  })
  output$SDIbelowMDL1 <- renderPlot({
    stat <- input$statInput
    polarPlot(subset(spod_comp_1(), bcPID_ppb < MDL), pollutant = "bcPID_ppb",  fontsize = 20,
              statistic = stat, main = NULL, key.position = "right", upper = 8,ws.int = 1,
              key.footer = NULL,limits = c(0, 50)) #,breaks = c(0, 20, 40, 60, 80, 1000))
  })
  output$SDIall2 <- renderPlot({
    stat <- input$statInput
    polarPlot(spod_comp_2(), pollutant = "bcPID_ppb",fontsize = 20,
              statistic = stat, main = NULL, key.position = "right", upper = 8,ws.int = 1,
              key.footer = NULL,limits = c(0, 50)) #,breaks = c(0, 20, 40, 60, 80, 1000))
  })
  output$SDIaboveMDL2 <- renderPlot({
    stat <- input$statInput
    polarPlot(subset(spod_comp_2(), bcPID_ppb > MDL), pollutant = "bcPID_ppb",  fontsize = 20,
              statistic = stat, main = NULL, key.position = "right", upper = 8,ws.int = 1,
              key.footer = NULL,limits = c(0, 50)) #,breaks = c(0, 20, 40, 60, 80, 1000))
  })
  output$SDIbelowMDL2 <- renderPlot({
    stat <- input$statInput
    polarPlot(subset(spod_comp_2(), bcPID_ppb < MDL), pollutant = "bcPID_ppb",  fontsize = 20,
              statistic = stat, main = NULL, key.position = "right", upper = 8, ws.int = 1,
              key.footer = NULL,limits = c(0, 50)) #,breaks = c(0, 20, 40, 60, 80, 1000))
  })
  #########################################################
  ######################################################### table output   
  output$table_all <-  DT::renderDataTable({
    spod_all_5_tab <-  spod_all_5min() 
    spod_all_5_tab <- spod_all_5_tab %>% 
      mutate_if(is.numeric, round, digits = 2)
    DT::datatable(spod_all_5_tab %>% filter(day >= input$dateRangetab[1] & day <= input$dateRangetab[2])) %>% formatDate(2, "toLocaleString")
  })
  
  #########################################################

}


shinyApp(ui, server)







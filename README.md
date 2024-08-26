# SENTINEL: Fenceline Sensor Data Analysis, Quality Assurance, & Visualization

## Disclaimer: 
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government. 

## Purpose:
There is growing interest in fenceline monitoring around chemical facilities. Fenceline sensors used in these monitoring applications can collect large amounts of concentration and meteorological data for extended time periods. The SENsor InTellIgeNt Emissions Locator (SENTINEL) application helps users compile, process, and analyze data from any fenceline sensor with a frequency of data reporting less than 5 minutes. This application delivers these capabilities in a user-friendly interface that can combine and process daily data files from multi-sensor deployments, allowing users to gain insights from compiled sensor data over time. The SENTINEL app is one of the technologies developed under the Next Generation Emissions Measurements (NGEM) program.

## Features:
The SENTINEL application is developed within the R Shiny framework and features are supported by several common R packages. Version 1.1 of SENTINEL prompts the user to upload any number sensor files from similar units as .csv files. The user will map the columns to a stardard analysis framework in the interface. The user has the option to apply user-specified quality assurance (QA) flags (i.e., calibrations, maintenance) to the raw data in the QA screen. All uploaded data is then processed to a standard 5-minute form and automatic QA flags are applied by the software. A dashboard is generated for the user that contains interactive times series plots and geospatial representations that incorporate sensor signal data, meteorological data, user-input calibration information, and automatic canister trigger recordings if applicable. This dashboard can be output to a HTML page for printing as a record. The user can also view the data in tabular form and perform basic search and sort operations on the processed 5-minute data. Quality assurance for this type of data is particularly useful to the user to confirm that sensors are working as expected over time. SENTINEL can populate QA tables for a single sensor or a collocated pair of sensors that contain summary statistics on multiple data quality indicators. Any values that are outside of a determined range will be flagged to alert the user. The user can output the processed data as a .csv file and the QA tables as HTML files. Finally, the app contains a page with useful links and resources to NGEM fenceline sensor research.

## More Info:
[SPod & SENTINEL Tools and Training Webinar](https://www.epa.gov/research-states/sensor-pods-volatile-organic-compound-fenceline-monitoring-and-data-analysis)


[Science Inventory Page](https://cfpub.epa.gov/si/si_public_record_report.cfm?Lab=CEMM&dirEntryId=359582)

## Contact:
Contact Megan MacDonald with any feedback or suggestions (macdonald.megan@epa.gov)

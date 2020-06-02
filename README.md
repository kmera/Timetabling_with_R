### Table of Contents

1. [Installation](#installation)
2. [Project Motivation](#motivation)
3. [File Descriptions](#files)
4. [Results](#results)

## Installation <a name="installation"></a>

The libaries needed to run the code, using R version 4.* are:

* shiny
* shinythemes
* dplyr
* readr
* openxlsx
* stringr
* DT
* data.table
* ggplot2
* shinyjs
* shinyauthr
* shinydashboard
* shinyalert
* forcats
* sodium
* tibble
* rdrop2

## Project Motivation<a name="motivation"></a>

When a University wants to define the professors schedule, the coordination team usually use a spreadsheet, but they have a really hard time trying to do that using tools that aren't suitable for that kind of tasks, so in order to support them with a specific tool, a Web app (using shiny) was developed which includes some features requested by the coordination team.

## File Descriptions <a name="files"></a>

The R file (app.R) includes the following:
user and password.
`Information` tab, with basic presentation of the app and the developer.
`Setting` tab, in this space the configuration will be done. The user can select the professor name, career, level, subject, schedule (day and time) and so on.
`Main Table` tab, shows all the paramenters configured by the user.
`Duplicates` tab, when a duplicate is detected it will be appear here. 
`Reports` tab, some graphical reports according to the configuration.

## Results<a name="results"></a>

After the cofiguration has finished, the user can download the table in the `Main Table` tab. Also, to avoid loosing the information, the app will upload automatically the main table, as an Excel file, to dropbox, so in that way the main table is always backed up and can be loaded to continue working. 


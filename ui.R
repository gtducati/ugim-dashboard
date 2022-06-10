#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyalert)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(DT)


# Options to check capacity by Facility or Tool
toolFacility_options = list("Facility" = 1, 
                    "Tool" = 2)

# Options for left plot type
week_options = list("Bar" = 1,
                    "Line" = 2)

# Options for right plot type
summary_options = list("Box-Whisker" = 1,
                       "Bar" = 2,
                       "Line" = 3)

stat_options = list(
    "Mean" = 1,
    "Median" = 2,
    "Max" = 3,
    "Min" = 4
)



# Define UI for application that draws a histogram
shinyUI(

  
  navbarPage(
    "Facility/Instrument Dashboard",
    tabPanel(
      "Capacity Analysis",
    useShinyjs(),
    useShinyalert(),
    
    fluidRow(
        column(
            8,
            # Sidebar with a slider input for defining high workload
            fluidRow(
                # h4("Select Calendar Range"),
                column(
                    2,
                    fileInput(
                        inputId = 'datafile',
                        label = "Choose CSV file",
                        accept = c(".csv")
                    )
                    
                ),
                column(
                    3,
                    
                    
                    radioButtons(
                      "toolFacility",
                      "Analyze by",
                      choices = toolFacility_options,
                      selected = 1,
                      inline = T
                    ),
                    
                    selectInput(
                        "Facility",
                        "Facility",
                        multiple = T,
                        choices = NULL
                    ),
                    
                ),
                column(
                    4,
                    dateRangeInput(
                        inputId = "dates",
                        label = "Date Range",
                        start = floor_date(now(), "week"),
                        end = now()
                    ),
                    checkboxInput(
                        inputId = "lastDayComplete",
                        label = "Zero out the remaining hours through the last day of the selected time range.",
                        # title = "Setting this to TRUE will fill the hours of the last day to zero if there are no entries. This impacts the statistics that are computed.",
                        value = F
                    )
                    
                ),
                column(
                    3,
                    checkboxInput(inputId = "showToolOccupation",
                                  label = "Show tool as occupied and disregard the number of users in a given hour",
                                  value = F)
                )
                
            )
        ),
        column(
            4,
            h3("Quick Views"),
            

            fluidRow(
                column(
                    1,
                    actionButton(
                        inputId = "Backward",
                        label = NULL,
                        title = "Go back 1 week",
                        icon = icon("step-backward")
                    )
                ),
                column(
                    1,
                    actionButton(
                        inputId = "Forward",
                        label = NULL,
                        title = "Go forward 1 week",
                        icon = icon("step-forward")
                    )
                ),

                column(2,
                       actionButton(inputId = "LastWeek",
                                    label = "Last Week")),
                column(2,
                       actionButton(inputId = "ThisWeek",
                                    label = "This Week")),
                column(
                  3,
                  actionButton(
                    inputId = "buttonToolUse",
                    label = "Occupancy",
                    icon = icon("chart-bar")
                  )
                ),
                
                column(
                  2,
                  actionButton(
                    inputId = "buttonTrainingChart",
                    label = "New Users",
                    icon = icon("chart-bar")
                  )
                )
                
            )
        )
        
        
    ),
    
    
    fluidRow(
      plotOutput("plot3", height = "700px")
    ),
    
    fluidRow(
        column(
            8,
            uiOutput("WeeklyPlot"),
            
            
            fluidRow(
                # column(4,            
                #        numericInput("height",
                #                     label = "Plot Height",
                #                     value = 400,
                #                     min = 400,
                #                     step = 100),
                #        downloadButton('downloadPlot1', 'Download Plot'),
                #        align = "right"),
                
                column(
                    4,
                    radioButtons(
                        "weekOptions",
                        "Plot Type",
                        choices = week_options,
                        selected = 1,
                        inline = T
                    )
                )
            )
            
        ),
        
        column(
            4,
            plotOutput("plot2", height = "700px"),
            
            # column(4,
            #        downloadButton('downloadPlot2', 'Download Plot'),
            #        align = "right"),
            
            column(
                4,
                radioButtons(
                    "summaryOptions",
                    "Plot Type",
                    choices = summary_options,
                    selected = 1,
                    inline = F
                )
                
            ),
            
            column(
                4,
                radioButtons(
                    "statOptions",
                    "Statistic",
                    choices = stat_options,
                    selected = 1,
                    inline = F
                )
                
            )
            
            
        )
    ),
 
    
    fluidRow(
      DT::dataTableOutput('tableUniqueUsers')
    )
    
    )
))

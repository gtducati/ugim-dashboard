#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggrepel)

# Source for color palette
source("environment.R", echo = T)

# Allows for large files to be uploaded
options(shiny.maxRequestSize = 100 * 1024 ^ 2)


# Define Server Logic
shinyServer(function(input, output, session) {
  
  numberOfWeeks <- function(start, end){
    interval(start, end) / weeks(1)
  }
  
  
  # Hide the Training/First Log-in and Total User Plot
  hide("plot3")
  
  plotHeight <- reactive({
    nweeks <- numberOfWeeks(input$dates[1], input$dates[2])
    
    paste0(200 * ifelse(nweeks < 2, 2, nweeks), "px")
  })
  
  # Event Observers ----
  observeEvent(input$buttonTrainingChart, {
    toggle("plot3")
  })
  
  observeEvent(input$buttonToolUse, {
    toggle("plot1")
    toggle("plot2")
    toggle("downloadPlot1")
    toggle("weekOptions")
    toggle("downloadPlot2")
    toggle("summaryOptions")
    toggle("statOptions")
  })
  

  
  
  #---- Updates the dropdown for Facilities or Tools ----
  updateDropDown <- function(infile) {
    facilities <- read_delim(infile$datapath,
                             delim = ";",
                             trim_ws = TRUE)
    
    
    
    if (input$toolFacility == 1 &
        "Facility" %in% colnames(facilities)) {
      facilities <- facilities %>%
        select(Facility) %>%
        distinct(Facility) %>%
        arrange(Facility) %>%
        rename(Tool = Facility)
    } else if ("Tool ID" %in% colnames(facilities)) {
      facilities <- facilities %>%
        select(`Tool ID`) %>%
        distinct(`Tool ID`) %>%
        arrange(`Tool ID`) %>%
        rename(Tool = `Tool ID`)
    }
    
    updateSelectInput(
      session,
      "Facility",
      label = ifelse(input$toolFacility == 1, "Facility", "Tool"),
      choices = facilities$Tool,
      selected = ""
    )
  }
  
  
  observeEvent(input$datafile, {
    updateDropDown(input$datafile)
  })
  
  
  observeEvent(input$toolFacility, {
    updateDropDown(input$datafile)
  })
  
  
  
  
  #---- This function creates the main plot of each week and occupancy per hour ----
  # This is sloppily written since it accesses global variables while passing local variables.
  getHourlyHeadCount <-
    function(toolCharges,
             FacName,
             Cap,
             STARTDATE,
             ENDDATE) {
      p <- toolCharges %>%
        ggplot(aes(x = HOUR, y = NUsers)) +
        scale_fill_manual(values = sl$HexCode) +
        scale_color_manual(values = s$HexCode) +
        gtheme +
        guides(
          fill = guide_legend(nrow = 1, label.position = "bottom"),
          color = guide_legend(nrow = 1, label.position = "bottom")
        ) +
        theme(
          axis.text.x = element_text(size = 10),
          axis.title.x.bottom = element_text(margin = margin(t = 12)),
          axis.text.y = element_text(size = 10),
          axis.title.y.left = element_text(margin = margin(r = 12,
                                                           l = 12)),
          axis.title.y.right = element_text(margin = margin(r = 12,
                                                            l = 12)),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = margin(t = 24, b = 24),
          plot.caption = element_text(size = 12),
          plot.caption.position = "plot",
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 10, angle = 0),
          legend.key.size = unit(12, "pt"),
          legend.position = "bottom",
          legend.text = element_text(
            size = 10,
            margin = margin(r = 0.1,
                            l = 0.1,
                            unit = "pt")
          ),
          legend.margin = margin(t = 0),
          legend.direction = "horizontal"
        ) +
        labs(
          title = paste0("Hourly Occupancy: ", STARTDATE, " to ", ENDDATE),
          x = "Hour of the Day (24-hour)",
          y = "Number of Occupants",
          fill = NULL,
          color = NULL,
          caption = Cap
        )
      scale_x_continuous(breaks = seq(0, 23, by = 2))
      
      
      # Toggle Chart Type: Bar or Line
      if (input$weekOptions == 1)  {
        # if (input$toggleCompare == 1) {
        #     p <- p + scale_fill_manual(values = s$HexCode) +
        #         geom_bar(stat = "identity",
        #                  position = "dodge",
        #                  aes(fill = factor(WEEK)))
        # } else {
        p <-
          p + geom_bar(stat = "identity", aes(fill = factor(HOUR)))
        # }
      }
      
      if (input$weekOptions == 2)  {
        # if (input$toggleCompare == 1) {
        #     p <- p + geom_line(aes(color = factor(WEEK)))
        # } else {
        p <- p + geom_line()
        # }
        
      }
      
      # if (input$toggleCompare == 1) {
      #     p <- p + facet_grid(~ factor(DAY, levels = dayLevel)) +
      #         labs(title = "Historical Comparison of Hourly Occupancy")
      # } else {
      p <- p + facet_grid(factor(WEEK)  ~
                            factor(DAY, levels = dayLevel)) +
        scale_y_continuous(sec.axis = sec_axis( ~ . * 0,
                                                name = "Week of Sunday",
                                                breaks = NULL))
      # }
      
      p # Show plot
    }
  
  #---- Summary plot of the selected date range ----
  # This is sloppily written since it accesses global variables while passing local variables.
  getAvgHourlyHeadCountTrunc <-
    function(toolCharges,
             FacName,
             Cap,
             STARTDATE,
             ENDDATE,
             showOccupiedOnly) {
      # Core data is transferred to t
      t <- toolCharges
      
      # Dynamically setting title
      type <- "Distributed" # Title for Boxploting
      
      # Changing the title depending on statistic that is selected
      if (input$summaryOptions != 1) {
        if (input$statOptions == 1)   {
          # mean
          t <- t %>%
            group_by(GROUP, DAYTYPE, HOUR) %>%
            summarise(NUsers = mean(NUsers)) %>%
            ungroup()
          type <- "Mean" # title for mean
        } else if (input$statOptions == 2)  {
          # median
          t <- t %>%
            group_by(GROUP, DAYTYPE, HOUR) %>%
            summarise(NUsers = median(NUsers)) %>%
            ungroup()
          type <- "Median" # title for median
        } else if (input$statOptions == 3)  {
          # max
          t <- t %>%
            group_by(GROUP, DAYTYPE, HOUR) %>%
            summarise(NUsers = max(NUsers)) %>%
            ungroup()
          type <- "Maximum" # title for max
        } else if (input$statOptions == 4)  {
          # min
          t <- t %>%
            group_by(GROUP, DAYTYPE, HOUR) %>%
            summarise(NUsers = min(NUsers)) %>%
            ungroup()
          type <- "Minimum"  # title for min
        }
      }
      
      if (showOccupiedOnly) {
        t <- t %>% mutate(NUsers = round(NUsers * 100, 0))
      }
      
      # Begin plotting options
      p <- t %>% ggplot() +
        scale_fill_manual(values = sl$HexCode) +
        scale_color_manual(values = s$HexCode) +
        gtheme +
        guides(
          fill = guide_legend(nrow = 1, label.position = "bottom"),
          color = guide_legend(nrow = 1, label.position = "bottom")
        ) +
        facet_grid(cols = vars(DAYTYPE)) +
        theme(
          title = element_text(size = 16),
          axis.text.x = element_text(size = 10),
          axis.title.x.bottom = element_text(margin = margin(t = 12)),
          axis.text.y = element_text(size = 10),
          axis.title.y.left = element_text(margin = margin(r = 12, l = 12)),
          axis.title.y.right = element_text(margin = margin(r = 12, l = 12)),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = margin(t = 24, b = 24, r = 24),
          plot.caption = element_text(size = 12),
          plot.caption.position = "plot",
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 11),
          legend.key.size = unit(12, "pt"),
          legend.position = "bottom",
          legend.text = element_text(
            size = 10,
            margin = margin(r = 0.1,
                            l = 0.1,
                            unit = "pt")
          ),
          legend.margin = margin(t = 0),
          legend.direction = "horizontal"
        ) +
        labs(
          title = paste0("Hourly ", type , " Occupancy: ",
                         STARTDATE,
                         " to ",
                         ENDDATE),
          x = "Hour of the Day (24-hour)",
          y = "Number of Occupants",
          fill = NULL,
          color = NULL,
          caption = Cap
        ) +
        scale_y_continuous(breaks = c(0:40)) +
        scale_x_continuous(breaks = seq(0, 23, by = 2))
      
      
      # Toggle Plot Type depending on selection
      if (input$summaryOptions == 1) {
        # BOX-WHISKER PLOT
        # Boxplotting
        # if (input$toggleCompare == 1) {
        #     p <- p + geom_boxplot(aes(
        #         x = HOUR,
        #         y = NUsers,
        #         group = HOUR,
        #         fill = factor(GROUP)
        #     ),
        #     notch = F) +
        #         facet_grid(DAYTYPE ~ GROUP, scales = "free_y") +
        #         scale_fill_manual(values = s$HexCode)
        #
        # } else {
        p <- p + geom_boxplot(aes(
          x = HOUR,
          y = NUsers,
          fill = factor(HOUR)
        ),
        notch = F)
        # }
        
      } else if (input$summaryOptions == 2) {
        # BAR PLOT
        p <- p + geom_bar(stat = "identity", aes(
          x = HOUR,
          y = NUsers,
          fill = factor(HOUR)
        )) +
          geom_label_repel(aes(
            x = HOUR,
            y = NUsers,
            label = round(NUsers, 2)
          ), fill = alpha("white", 0.5))
      } else if (input$summaryOptions == 3) {
        # LINE PLOT
        p <- p + geom_line(aes(x = HOUR, y = NUsers))
      }
      
      
      if (showOccupiedOnly) {
        p <- p + ylim(0, 100) +
          labs(y = "Percent Occupied")
      }
      
      p # display the plot
      
    }
  
  
  # Training Capacity ----
  getTraining <- function(toolCharges,
                          FacName,
                          Cap) {
    
    # clean-up the data so the alg. can plot correctly
    AnnualToolUse <- toolCharges %>%
      rename(ID = FacilityID, UserName = PI.User) %>% # rename columns
      filter(lookup(c(FacName), ID)) %>% # Filter out the data you want
      mutate(FY = year(Date)) # use the calendar year
    
    
    # Generate the plot
    p <- AnnualToolUse %>%
      group_by(ID, UserName) %>%
      arrange(FY) %>%
      distinct(FY, UserName) %>%
      mutate(Count = 1:n()) %>%
      ungroup() %>%
      filter(Count == 1) %>%
      group_by(FY, ID) %>%
      summarize(NewUserCount = n()) %>%
      ungroup() %>%
      left_join(
        AnnualToolUse %>%
          group_by(FY, ID) %>%
          distinct(UserName) %>%
          summarize(TotalUserCount = n()) %>%
          ungroup(),
        by = c("FY", "ID")
      ) %>%
      ggplot() +
      geom_col(
        aes(FY - 0.2, NewUserCount, fill = factor(ID)),
        width = 0.37,
        position = position_stack()
      ) +
      geom_text(
        aes(
          FY - 0.2,
          NewUserCount,
          label = NewUserCount,
          group = factor(ID)
        ),
        position = position_stack(vjust = .5),
        color = "white"
      ) +
      geom_col(
        aes(FY + 0.2, TotalUserCount, fill = factor(ID)),
        width = 0.37,
        position = position_stack()
      ) +
      geom_text(
        aes(
          FY + 0.2,
          TotalUserCount,
          label = TotalUserCount,
          group = factor(ID)
        ),
        position = position_stack(vjust = .5),
        color = "white"
      ) +
      gtheme +
      scale_fill_manual(values = s$HexCode) +
      theme(plot.caption = element_text(size = 10)) +
      labs(
        x = "Fiscal Year",
        y = "New User (Left) and Annual User (Right) Counts",
        fill = "",
        caption = "Left Bar: New User Count which are the number of users who logged on for the first time. \nRight Bar: Annual User Count is the total number of unique users."
      ) +
      scale_x_discrete(limits = c(1990:2040))
    
    
    p
  }
  
  
  
  # Load the data ----
  
  allCharges <- reactive({
    # Get the file name
    infile <- input$datafile
    
    # Make sure the file name exists
    validate(
      need(
        infile != "" | !is.null(infile),
        "Please upload CSV data for processing and viewing by clicking the Browse button. A semi-colon must be used as the delimiter. The following columns are required: Facility, Tool ID, User, Date, and Duration Columns names are case-sensitive. Facility is the name of the facility being accessed. 'Tool ID' is the name of the instrument being accessed. User is the user's name accessing the space and serves as a unique identifier for each record. Date is the time of access formatted as YYYY-MM-DD HH:MM:SS. Duration is the total reseration time in hours in decimal format. Any additional columns will be ignored."
      )
    )
    
    # Make sure something is selected in the dropdown
    validate(need(
      length(input$Facility) > 0,
      "Please select an item from the Facility/Tool dropdown."
    ))
    
    
    # Create the tibble of lab access
    toolCharges <- read_delim(infile$datapath,
                              delim = ";",
                              trim_ws = TRUE) %>%
      filter(Duration > 0) %>%
      rename(PI.User = User)
    
    # Analyze by Tool
    if (input$toolFacility == 2) {
      toolCharges <- toolCharges %>% rename(FacilityID = `Tool ID`)
    } else {
      toolCharges <- toolCharges %>% rename(FacilityID = Facility)
    }
    
    toolCharges
    
  })
  
  
  filedata <- reactive({
    toolCharges <- allCharges()
    
    # Get values from user input fields on GUI
    STARTDATE <- input$dates[1]
    ENDDATE <- input$dates[2]
    FacName <- input$Facility
    
    t <- toolCharges %>%
      filter(Date > STARTDATE) %>% # Remove all data prior to the selected Start Date
      filter(Date < ymd_hms(paste0(
        as.Date(ENDDATE) + days(1), " 00:00:00"
      ))) %>% # Remove all data after the selected end date
      filter(grepl(paste(FacName, collapse = "|"), FacilityID)) %>% # Filter for only the selected Facility: THIS IS THE `TOOL ID`
      arrange(Date) %>% # order the observations by date in ascending order
      mutate(END = Date + seconds(Duration * 60 * 60)) %>% # Calculate the duration for each entry
      mutate(MMDDSTART = as.POSIXct(floor_date(Date, "hour"))) %>%  # Generate the Start Time
      mutate(MMDDEND = as.POSIXct(floor_date(END, "hour"))) %>%  # Generate the End Time
      group_by(PI.User, Date) %>%
      complete(MMDDSTART = seq(
        from = min(MMDDSTART),
        to = max(MMDDEND),
        by = "hour"
      )) %>% # Completing the hourly sequence to represent occupancy; using the Start Date Column to hold this data
      ungroup() %>%
      select(PI.User, MMDDSTART) %>%  # Reduce Data volume
      distinct() %>% # Remove duplicates
      group_by(MMDDSTART) # Group the data by the date
    
    
    
    if (input$showToolOccupation) {
      t <-
        t %>% summarise(NUsers = ifelse(n() > 0, 1, 0))  # and count the number of users per hour; Limit to only 1 user to show occupation.
    } else {
      t <-
        t %>% summarise(NUsers = n())  # and count the number of users per hour
    }
    
    
    t <- t %>% ungroup() %>% # ungroup to process further
      complete(MMDDSTART = seq(
        from = ymd_hms(paste0(STARTDATE, " 00:00:00")),
        to = (if (input$lastDayComplete)
          ymd_hms(paste0(
            as.Date(ENDDATE) + days(1), " 00:00:00"
          ))
          else
            max(MMDDSTART)),
        # to = ifelse(input$lastDayComplete, ymd_hms(paste0(ymd(as_date(max(MMDDSTART))+days(1)), " 00:00:00")), max(MMDDSTART)),
        # to = max(MMDDSTART),
        # to = ymd_hms(paste0(
        #     as.Date(ENDDATE) + days(1), " 00:00:00"
        # )),
        by = "hour"
      )) %>% # Complete the sequence between the Selected Start and End Dates
      replace_na(list(NUsers = 0)) %>% # Set NA to zero for proper calculation
      rename(Date = MMDDSTART) %>%
      filter(Date < ymd_hms(paste0(
        as.Date(ENDDATE) + days(1), " 00:00:00"
      ))) %>% # Trim off any entries that extend past the end date
      # filter(Date < max(Date)) %>% # Trim off any entries that extend past the end date
      mutate(
        WEEK = floor_date(Date, "week"),
        # Labeling the Week name of the each entry to its Sunday, YYYY-MM-DD
        DAY = weekdays(Date) ,
        # Set the day of the week
        DAYTYPE = isWeekend(weekdays(Date)),
        # Is it a weekend or weekday?
        HOUR = hour(Date),
        # Indicating the hour
        # FacilityID = FacName,
        GROUP = paste0(STARTDATE, " to ", ENDDATE)
      )
    
    
    
    output$tableUniqueUsers <- DT::renderDataTable(
      toolCharges %>%
        rename(User = PI.User) %>%
        filter(FacilityID == FacName) %>%
        filter(Date > STARTDATE) %>% # Remove all data prior to the selected Start Date
        filter(Date < ymd_hms(paste0(
          as.Date(ENDDATE) + days(1), " 00:00:00"
        ))) %>% # Remove all data after the selected end date
        group_by(User) %>%
        summarize(Usage = sum(Duration)),
      extensions = 'Buttons',
      options = list(dom = 'Blfrtip',
                     buttons = c('excel', 'pdf'))
    )
    
    t # return t
    
  })
  
  
  #---- INPUT CONTROL/ERROR CHECKING ----
  
  
  # If the box-whisker is selected,
  #disable the options for statistics
  observeEvent(input$summaryOptions, {
    if (input$summaryOptions == 1) {
      shinyjs::disable("statOptions")
    } else {
      shinyjs::enable("statOptions")
    }
  })
  
  
  # Make user aware a malformed date range
  observeEvent(input$dates, {
    if (input$dates[1] > input$dates[2]) {
      shinyalert("That date range doesn't make sense. Try again.")
    }
  })
  
  
  
  
  #---- BUTTON OBSERVERS ----
  
  # Move backward one week
  observeEvent(input$Backward, {
    newEnd <- ceiling_date(input$dates[2], "week") - weeks(1) - days(1)
    newStart <- floor_date(input$dates[1], "week") - weeks(1)
    
    updateDateRangeInput(session,
                         inputId = "dates",
                         start = newStart,
                         end = newEnd)
    
  })
  
  # Move forward one week
  observeEvent(input$Forward, {
    newEnd <- ceiling_date(input$dates[2], "week") + weeks(1) - days(1)
    newStart <- floor_date(input$dates[1], "week") + weeks(1)
    
    updateDateRangeInput(session,
                         inputId = "dates",
                         start = newStart,
                         end = newEnd)
  })
  
  # Generate the dates to view last week
  observeEvent(input$LastWeek, {
    updateDateRangeInput(
      session,
      inputId = "dates",
      start = floor_date(now() - weeks(1), "week"),
      end = floor_date(now(), "week") - days(1)
    )
    
  })
  
  # Generate the dates to view this week
  observeEvent(input$ThisWeek, {
    updateDateRangeInput(
      session,
      inputId = "dates",
      start = floor_date(now(), "week"),
      end = ceiling_date(now(), "week") - days(1)
    )
    
  })
  
  
  
  #---- PLOT FUNCTIONS ----
  
  plotInput1 = function() {
    getHourlyHeadCount(
      filedata(),
      input$Facility,
      paste0("Showing data for ", paste(input$Facility, collapse = ", ")),
      input$dates[1],
      input$dates[2]
    )
  }
  
  plotInput2 = function() {
    getAvgHourlyHeadCountTrunc(
      filedata(),
      input$Facility,
      paste0("Showing data for ", paste(input$Facility, collapse = ", ")),
      input$dates[1],
      input$dates[2],
      input$showToolOccupation
    )
  }
  
  plotInput3 = function() {
    getTraining(allCharges(),
                input$Facility,
                paste0("Showing data for ", paste(input$Facility, collapse = ", ")))
  }
  
  
  #---- PLOT EXPORT ----
  
  # Export Weekly/Hourly Plot
  output$downloadPlot1 <- downloadHandler(
    filename = function() {
      paste0(input$Facility,
             "_",
             input$dates[1],
             "-",
             input$dates[2],
             # "-",
             # typeName(input$dataToggle),
             "_",
             "Full",
             ".png")
    },
    
    content = function(file) {
      ggsave(
        file,
        plotInput1(),
        width = 17,
        height = 36,
        units = "in"
      )
    }
  )
  
  
  # Export Summary Plot
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      paste0(input$Facility,
             "_",
             input$dates[1],
             "-",
             input$dates[2],
             # "-",
             # typeName(input$dataToggle),
             "_",
             "Summary",
             ".png")
    },
    
    content = function(file) {
      ggsave(
        file,
        plotInput2(),
        width = 17,
        height = 8.5,
        units = "in"
      )
    }
  )
  
  
  #---- PLOT RENDERING ----
  # Weekly plot(s)
  output$WeeklyPlot <- renderUI({
    plotOutput("plot1", height = plotHeight())
  })
    
  output$plot1 <-  renderPlot({
    plotInput1()
  })
  
  # Weekly Statistical Summary
  output$plot2 <- renderPlot({
    plotInput2()
  })
  
  # New User Logins and Total Users by Year
  output$plot3 <- renderPlot({
    plotInput3()
  })
  
})
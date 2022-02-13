#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(lubridate)
library(dplyr)
library(tidyr)

load(".RData")

years <- c(2001:2021)


# convert the date codes to more memorable date names

names(allData_uic)[names(allData_uic)=="newDate"] <- "entries by each day"
names(allData_uic)[names(allData_uic)=="month"] <- "total entries by each month"
names(allData_uic)[names(allData_uic)=="day_week"] <- "total entries by each day of week"

# Create the menu items to select the different years and the different chart
listNames <- c(colnames(allData_uic))
listNamesGood <- listNames[listNames != "station_id" & listNames != "stationname" 
                          & listNames != "daytype" & listNames != "rides"
                          & listNames != "year" & listNames != "day"]
barchart_check <- c(listNamesGood,"All")

#date_data < -subset(allData_uic, year(allData_uic$year) == input$Year )

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title="CS 424 Project 1 Spring 2022"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                     menuItem("Compare", tabName = "compare", icon = icon("th")),
                     menuItem("About", tabName = "about", icon = icon("info")),
                     
                     selectInput("Year", "Select the year to visualize", years, selected = 2021),
                     
                     checkboxGroupInput("sources", "Check Bar Chart to show",
                                        barchart_check,
                                        selected = "All")
                   )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard", 
              fluidRow(
                column(8, 
                       fluidRow(
                         box(title = "Total Entriees at UIC-Halsted by Year", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             plotOutput("totalbyyear"), height="700") 
                       ), # row 1   /col 1
                       fluidRow(
                         box(title = "Entries at UIC-Halsted by Each Day", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             plotOutput("totalbyday"),height ="700")
                       ), # row 2   /col1
                       fluidRow(
                         box(title = "Total Entriees at UIC-Halsted by Each Month", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             plotOutput("totalbymonth"), height="700") 
                       ), # row 3  /Col1
                       fluidRow(
                         box(title = "Total Entriees at UIC-Halsted by Each Day of Week", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             plotOutput("totalbyday_week"), height="700") 
                       )), # row 4 / column 1
              
                
                column(4,
                       fluidRow(
                         box(title = "Total Entriees at UIC-Halsted by Year(Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             dataTableOutput("total_year_table"))
                       ), # row 1
                       fluidRow(
                         box(title = "Entries at UIC-Halsted by Each Day (Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             dataTableOutput("total_day_table"))
                       ), # row 2
                       fluidRow(
                         box(title = "Total Entriees at UIC-Halsted by Each Month(Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             dataTableOutput("total_month_table"))
                       ), # row 3
                       fluidRow(
                         box(title = "Total Entriees at UIC-Halsted by Each Day of Week (Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             dataTableOutput("total_dayweek_table"))
                       )
                      )     
              )  # end fluidRow
      ), # end tab item 
     
      tabItem(tabName = "about",
              h2("About Me"),
              p("This project was made by Kai Qi for CS 424 Spring 2022"),
              p("Original data available from https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f")
      ) # end tab item
    ) # end tab items
  )# end body
)# end page

# Define server logic required to draw a histogram
server <- function(input, output) {
  # increase the default font size
  theme_set(theme_grey(base_size = 15))   
  #justOneYearReactive <- reactive(if("All" %in% input$sources){ subset(allData_uic,year(allData_uic$year) == input$Year)  }
                         # else{subset(allData_uic$input$sources, year(allData_uic$year)== input$Year ) } )
  justOneYearReactive <- reactive({allData_uic })
                                 
  
  output$totalbyyear <- renderPlot({
    justOneYear <- justOneYearReactive()
    
    ggplot(justOneYear, aes(fill=year,x=year, y=rides)) + geom_bar(position="identity" ,stat = 'identity',width=0.7)+labs(title="                 The total entry at UIC-Halsted from 2001 to 2021", 
                                                                                                                                                             x="The year from 2001 to 2021", y = "The total entries  unit: person")
  })
  
  output$totalbyday <- renderPlot({
    justOneYear <- justOneYearReactive()
    
    ggplot(data2021, aes(fill=newDate,x=newDate, y=rides)) + geom_bar(position="identity" ,stat = 'identity',width=0.7)+labs(title="                 The entry at UIC-Halsted each day for 2021 ", 
                                                                                                                                   x="The day from January 01 to Dec 31 2021", y = "The total entries  unit: person")
  })
  output$totalbymonth <- renderPlot({
    justOneYear <- justOneYearReactive()
    
    ggplot(data2021, aes(fill=month,x=fct_inorder(month), y=rides)) + geom_bar(position="identity" ,stat = 'identity',width=0.7)+labs(title="                 The total entry at UIC-Halsted each day for each month in 2021 ", 
                                                                                                                                   x="Month from January to December", y = "The total entries  unit: person")
  })
  output$totalbyday_week <- renderPlot({
    justOneYear <- justOneYearReactive()
    
    ggplot(data2021, aes(fill=day_week,x=fct_inorder(day_week), y=rides)) + geom_bar(position="identity" ,stat = 'identity',width=0.7)+labs(title="                 Thetotal  entry at UIC-Halsted for each day of the week in 2021 ", 
                                                                                                                                      x="From Monday to Sunday", y = "The total entries  unit: person")
  })
  
  output$total_year_table <- renderDataTable({
    justOneYear <- justOneYearReactive()
    
    agg <- aggregate('Total Rides'~year,data=allData_uic, FUN=sum,na.rm=TRUE)
    
  },options = list(pageLength = 5, autoWidth=TRUE, order=list(1, 'asc')))
    
  
  
}
  



# Run the application 
shinyApp(ui = ui, server = server)

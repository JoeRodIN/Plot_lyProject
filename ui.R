#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# increase max file upload size
options(shiny.maxRequestSize = 25 * 1024 ^ 2);

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("In-store Item Movement"),
  
  sidebarLayout(
    sidebarPanel(
       h3(paste("Date run ", Sys.Date())),
       
       dateInput("dateSelected", label = "Enter Date: ", min = '2016-05-16', max = '2016-07-31', value = '2016-05-16'),
       h6("(Valid dates for demo:  5/16 through 7/10)"),
       checkboxInput("toggle", "Swap bar length and color", value = FALSE),
       hr(),
       checkboxInput("ownFile", "Upload my own file", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(type="tabs",
          tabPanel("How to Use",
            h2("Welcome to Item Tracker"),
            br(),
            tags$pre(span("STEP 1: Read this How To page")),
            tags$pre(span("STEP 2: Read the Introduction tab for an overview of what this application does")),
            tags$pre(span("STEP 3: If you don't have a file to upload, click on the Prep Data tab and the demo file will be processed"),
            span(" \tIf you have a file to upload, click on the checkbox for uploading your own file"),
            span(" \tThen click on the Prep Data tab to choose the file to upload."),
            span(" \tThen click on the Process the file to upload and process the file"),
            span(" \tIn either case, when done, the text Processing Complete... will appear")),
            tags$pre(span("STEP 4: Click on the Results tab to see the resulting graph."),
            span(" \tTo swap what is used to determine the length and color of the bars, click on the Swap bar and length checkbox"),
            span(" \tChange the date picker on the left to the date desired to show the graph for that date"))
          ),
          tabPanel("Introduction",
            br(),
            p("Read the How To tab for specifics on how to use this application"),
            p("This simulates attaching RFID tags to items to track movement through a store.  Specifically, track items
                when removed from the display structure (either a RACK or a TABLE) to either back to the display structure
                or if sold (REGISTER)."),
            p("The graph generated takes the average time held and average time item picked up and normalizes is against the average time held 
                and average time picked up for all items for all stores for all days."),
            p("If you have your own file to process, check the checkbox and you will be given a fileInput box to upload. If not,
                our demo data will be used."),
            h4("If you want to use your own data, construct a .csv file with at least the following columns and with the names given:"),
            code("TAG_DATA: item ID"), br(),
            code("EVENT_DATE: Date RFID tag read"), br(),
            code("EVENT_TYPE: whether reading as item entering (ENTER) or exiting (EXIT) location"), br(),
            code("STORE_ID: store ids"), br(),
            code("LOCATION: where in the store RFID tag read (TABLE, RACK, REGISTER)")
          ),
          tabPanel("Prep Data",
            br(),
            span(h4("Actual file processing will take a couple minutes, please be patient..."), style="color:red"),
            h5("If not loading a file to process please ignore the fileUpload element below"),
            fileInput('file1', 'Choose Item movement file (CSV File)',
               accept=c('text/csv', 
                'text/comma-separated-values
                ,text/plain', 
                '.csv')),
            actionButton("goButton", "Process the file"),
            span(h4(textOutput("progresss")), style="color:red")
          ),
          tabPanel("Results", 
            br(), 
            p("The length of the bar graph represents the number of standard deviations greater/lesser than the 
                overall average, and the color represents the number of standard deviations of the average number 
                of times the item was picked up - the bluer the lower below the average, the redder the greater 
                it waspwd held."),
            br(),
            plotlyOutput("distPlot")
          )
      )
    )
  )
))

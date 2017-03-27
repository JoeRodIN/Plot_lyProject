#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  source("calcDurations.R");
  source("prepData.R");
  
  # PREPARE DATA
  output$progresss <- renderText(
    if (input$ownFile) {
      if (input$goButton > 0) { 
      events <- readInFile(input$file1$datapath);
      output$progresss <- renderText("Events file loaded. Prepping...");
      
      tablewDurations <- prepareTableData(events);
      rackwDurations <- prepareRackData(events);
  
      TableTMeans <- computeTableTMeans (tablewDurations);
      TableTTMeans <- computeTableTTMeans (tablewDurations);
      
      RackTMeans <- computeTableTMeans (rackwDurations);
      RackTTMeans <- computeTableTTMeans (rackwDurations);
      
      all <- combineTogether(TableTMeans, TableTTMeans, RackTMeans, RackTTMeans);
      write.csv(all, "all.csv", row.names = FALSE);
      
      allTMeans <- computeAllTMeans(all);
      allTTMeans <- computeAllTTMeans(all);
      
      output$progresss <- renderText("Finding population mean....");
      popMeans <- getPopMeans(merge(allTMeans, allTTMeans, all = TRUE));
      names(popMeans) = c("TOUCH_TIME", "TOUCHES");
      write.csv(popMeans, "popMeans.csv", row.names = FALSE);

      output$progresss <- renderText("Finding population stadard deviations...");
      popSTDs <- getStdDevs(merge(allTMeans, allTTMeans, all = TRUE));
      names(popSTDs) = c("TOUCH_TIME", "TOUCHES");
      write.csv(popSTDs, "popSTDs.csv", row.names = FALSE);
      
      output$progresss <- renderText("Processing complete...");
      "Processing Complete...";
    }
    else {
      "Will take awhile to process, please be patient";
    }
  }
  else {
    events <- readInFile("events.csv");
    output$progresss <- renderText("Events file loaded. Prepping...");
    
    tablewDurations <- prepareTableData(events);
    rackwDurations <- prepareRackData(events);
    
    TableTMeans <- computeTableTMeans (tablewDurations);
    TableTTMeans <- computeTableTTMeans (tablewDurations);
    
    RackTMeans <- computeTableTMeans (rackwDurations);
    RackTTMeans <- computeTableTTMeans (rackwDurations);
    
    all <- combineTogether(TableTMeans, TableTTMeans, RackTMeans, RackTTMeans);
    write.csv(all, "all.csv", row.names = FALSE);
    
    allTMeans <- computeAllTMeans(all);
    allTTMeans <- computeAllTTMeans(all);
    
    output$progresss <- renderText("Finding population mean....");
    popMeans <- getPopMeans(merge(allTMeans, allTTMeans, all = TRUE));
    names(popMeans) = c("TOUCH_TIME", "TOUCHES");
    write.csv(popMeans, "popMeans.csv", row.names = FALSE);
    
    output$progresss <- renderText("Finding population stadard deviations...");
    popSTDs <- getStdDevs(merge(allTMeans, allTTMeans, all = TRUE));
    names(popSTDs) = c("TOUCH_TIME", "TOUCHES");
    write.csv(popSTDs, "popSTDs.csv", row.names = FALSE);
    
    output$progresss <- renderText("Processing complete...");
    "Processing Complete...";
  }
  );
  
  # GENERATE GRAPH
  output$distPlot <- renderPlotly({
    all <- read.csv("all.csv");
    all$TAG_DATA <- as.factor(all$TAG_DATA);
    popMeans <- read.csv("popMeans.csv");
    popSTDs <- read.csv("popSTDs.csv");

    thisDate = input$dateSelected;

    today <- all[as.Date(all$DATE) ==  thisDate, ];

    dayMeans <- normalizeMeans(today, popMeans, popSTDs);

    plotData <- cbind(today$TAG_DATA, dayMeans); 
    plotData[is.na(plotData)] <- 0;
    names(plotData)[1] = "TAG_DATA";

    color.gradient <- function(x, colors=c("blue", "white", "yellow", "red"), colsteps=1000) {
      return (colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ]);
    }

    if(input$toggle) {
      MainTitle <- paste("Normed Avg Num Times Picked up for", thisDate, "Colored by Avg Length Item Held")
      HoverText <- paste("Item ID: ", plotData$TAG_DATA, "<br />Norm Avg Time Held", plotData$TOUCH_TIME);
      
      
      colorList = list(color.gradient(plotData$TOUCH_TIME));
      
      plot_ly(plotData, x = ~TOUCHES, y = ~TAG_DATA,
              type = 'bar', orientation = 'h',
              text = HoverText, hoverinfo = "text + x",
              marker = list(color = colorList[[1]]), height = 1500) %>% 
        layout(title = MainTitle,
               yaxis = list(title = "", showticklabels = FALSE),
               xaxis = list(title = "", range = list(min(plotData$TOUCHES), max(plotData$TOUCHES))));
    }
    else {
      MainTitle <- paste("Normalized Avg Length Item Held for", thisDate, "Colored by Avg Num Times Picked up");
      HoverText <- paste("Item ID: ", plotData$TAG_DATA, "<br />Norm Times Picked up", plotData$TOUCHES);
      
      colorList = list(color.gradient(plotData$TOUCHES));
      
      plot_ly(plotData, x = ~TOUCH_TIME, y = ~TAG_DATA,
              type = 'bar', orientation = 'h', 
              text = HoverText, hoverinfo = "text + x",
              marker = list(color = colorList[[1]]), height = 1500) %>% 
        layout(title = MainTitle,
               yaxis = list(title = "", showticklabels = FALSE),
               xaxis = list(title = "", range = list(min(plotData$TOUCH_TIME), max(plotData$TOUCH_TIME))));
    }
  })
})

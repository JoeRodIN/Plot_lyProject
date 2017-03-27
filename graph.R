
  ##today <- all[as.POSIXct(all$DATE) ==  as.POSIXct(thisDate), ];
  today <- all[as.POSIXct(all$DATE) ==  as.POSIXct('2016-05-20'), ];
  
  dayMeans <- normalizeMeans(today, popMeans, popSTDs);

  plotData <- cbind(today$TAG_DATA, dayMeans); 
  plotData[is.na(plotData)] <- 0;
  names(plotData)[1] = "TAG_DATA";

  ##SHOENAMES <- sapply(plotData$TAG_DATA, function (x) { substr(events[events$TAG_DATA == x, "COMMENTS"][1], 1, 30); });
  ##plotData <- cbind(plotData, SHOENAMES);
  
  color.gradient <- function(x, colors=c("blue", "white", "yellow", "red"), colsteps=1000) {
    return (colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ]);
  }

  colorList = list(color.gradient(plotData$TOUCHES));

  title <- paste("Normalized Touch Time for", '2016-05-20');
  par(mai=c(.5,2.65,0.2,0.2));
##  barplot(plotData$TOUCH_TIME, main=title, horiz = TRUE, names.arg = plotData$SHOENAMES, las = 1, col=color.gradient(plotData$TOUCHES))
  
  plot_ly(plotData, x = ~TOUCH_TIME, y = ~TAG_DATA, 
          name = paste("Normalized Touch Time for ", '2016-05-20'), type = 'bar', 
          orientation = 'h', marker = list(color = colorList[[1]])) %>% 
    layout(title = paste("Average Item Time Held Normalized against Averages of all Items,  Colored by Number Picked up. Generated ", Sys.Date()), 
           xaxis = list(title = "Avg Time Held Normalized against AVg for All Items", range = list(min(plotData$TOUCH_TIME), max(plotData$TOUCH_TIME))));

  
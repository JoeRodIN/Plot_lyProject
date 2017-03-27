## starting point, takes events split by shoe
## and splits them based on day and passes each on
computeTouchTime <- function (x) {
    result <- lapply(split(x, x$STORE_ID), splitbydate);
    
    return (mergeBack(result, "STORE_ID"));
}  

## already split by shoe and store
## final split is to do it by date
splitbydate <- function (x) {
  result <- lapply(split(x, x$DATE), computeTouchTimeForDay);

  return (mergeBack(result, "DATE"));
}

## now we can compute durations for each shoe on a particular day
computeTouchTimeForDay <- function (x) {
  touches <- data.frame();
  aday <- x;
  DATE <- unique(aday$DATE);
  rackData <- aday[aday$LOCATION == "RACK", ];
  EnterRack <- rackData[rackData$EVENT_TYPE == "ENTER", ];
  ExitRack <- rackData[rackData$EVENT_TYPE == "EXIT", ];
  durationsRack <- calcDurations(ExitRack, EnterRack);
  tableData <- aday[aday$LOCATION == "TABLE", ];
  EnterTable <- tableData[tableData$EVENT_TYPE == "ENTER", ];
  ExitTable <- tableData[tableData$EVENT_TYPE == "EXIT", ];
  durationsTable <- calcDurations(ExitTable, EnterTable);
  durations <- c(durationsRack, durationsTable);
  Exit <- rbind(ExitRack, ExitTable);
  totalTouchesTime <- mean(durations, na.rm = TRUE);
  thisrow <- data.frame(DATE, ifelse(nrow(Exit) == 0, NA, (totalTouchesTime / nrow(Exit))), nrow(Exit));
  names(thisrow) <- c("DATE", "TOUCH_TIME", "TOUCHES");
  touches <- rbind(touches, thisrow);
  names(touches) <- c("DATE", "TOUCH_TIME", "TOUCHES");
  
  touches;
}

## computes the duration for a single day for a single shoe
calcDurations <- function (begin, end) {
  d <- vector();
  for (i in 1:nrow(begin)) {
    b <- begin[i, ];
    storeid <- b[["STORE_ID"]];
    loc <- b[["LOCATION"]];
    exitDate <- b$EVENT_DATE;
    df <- end[end$STORE_ID == storeid, ];
    e <- (df[df$EVENT_DATE > exitDate, ])[1, ];
    d <- c(d, ifelse(! is.na(e$EVENT_DATE), difftime(e$EVENT_DATE, exitDate, units = "secs"), 0));
  };

  return (d);
}

## this takes the results and puts them all back together
mergeBack <- function (dtList, Name) {
  df <- data.frame();

  for (i in 1:length(names(dtList))) {
    id <- names(dtList)[i];
    
    x <- dtList[[id]];

    if (! (Name %in% names(x))) { thisrow <- data.frame(id, x); names(thisrow)[1] = Name; } else { thisrow <- x };
    
    df <- rbind(df, thisrow);
  }
  
  df;
}

computeTMeans <- function (x) {
  if (length(x) == 0) { return (data.frame()); };
  
  result <- lapply(split(x, x$DATE), function (s) { mean(s[["TOUCHES"]], rm.na = TRUE)});

  return (mergeBack(result, "DATE"));
}

computeTTMeans <- function (x) {
  if (length(x) == 0) { return (data.frame()); };
  
  result <- lapply(split(x, x$DATE), function (s) { mean(s[["TOUCH_TIME"]], rm.na = TRUE)});
  
  return (mergeBack(result, "DATE"));
};

getPopMeans <- function(mergeData) {
  scaleData <- mergeData[, c("TOUCH_TIME", "TOUCHES")];
  
  t <- mean(scaleData$TOUCHES, na.rm = TRUE);
  tt <- mean(scaleData$TOUCH_TIME, na.rm = TRUE);
  data.frame(tt, t);
}

getStdDevs <- function(mergeData) {
  scaleData <- mergeData[, c("TOUCH_TIME", "TOUCHES")];
  
  data.frame(sd(scaleData$TOUCH_TIME, na.rm = TRUE), sd(scaleData$TOUCHES, na.rm = TRUE));
}

normalizeMeans <- function (data, pm, pstd) {
  result <- data.frame();
  
  for (i in 1:nrow(data)) {
    arow <- data[i, ]; 
    
    rowTT <- ifelse(is.na(arow[["TOUCH_TIME"]]), 0, (arow[["TOUCH_TIME"]] - pm["TOUCH_TIME"]) / pstd["TOUCH_TIME"]);
    rowT <- ifelse(is.na(arow$TOUCHES), 0, (arow$TOUCHES - pm["TOUCHES"]) / pstd["TOUCHES"]);
  
    thisrow <- data.frame(rowTT, rowT);
    names(thisrow) <- c("TOUCH_TIME", "TOUCHES");

    result <- rbind(result, thisrow); 
  }

  return (result);
}




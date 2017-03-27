
readInFile <- function (path) {
  actFile <- read.csv(path);
  actFile <- actFile[,c("TAG_DATA", "EVENT_DATE", "EVENT_TYPE", "STORE_ID", "LOCATION", "COMMENTS")];
  
  actFile$EVENT_DATE <- as.POSIXct(actFile$EVENT_DATE);
  DATE <- as.Date(actFile$EVENT_DATE);
  actFile <- cbind(actFile, DATE);

  names(actFile)[length(names(actFile))] <- "DATE";
  
  actFile;
};

prepareTableData <- function (df) {
  tableData <- df[df$LOCATION == 'TABLE', ]; 

  ## split by tag id
  tableByTagId <- split(tableData, tableData$TAG_DATA);

#  print("Computing touch duration...");
  tableWDurations <- lapply(tableByTagId, computeTouchTime);

  lapply(tableWDurations, function (x) { names(x) <- c("STORE_ID", "DATE", "TOUCH_TIME", "TOUCHES"); x; });
}

prepareRackData <- function (events) {
  rackData <- events[events$LOCATION == 'RACK', ];

  rackByTagId <- split(rackData, rackData$TAG_DATA);

  rackWDurations <- lapply(rackByTagId, computeTouchTime);
  
  lapply(rackWDurations, function (x) { names(x) <- c("STORE_ID", "DATE", "TOUCH_TIME", "TOUCHES"); x; });
}  
  
computeTableTMeans <- function (tableWDurations) {
  TableTMeans <- lapply(tableWDurations, computeTMeans)
  TableTMeans <- mergeBack(TableTMeans, "TAG_DATA")
  names(TableTMeans) = c("TAG_DATA", "DATE", "TOUCHES");
  
  TableTMeans;
}

computeTableTTMeans <- function (tableWDurations) {
  TableTTMeans <- lapply(tableWDurations, computeTTMeans)
  TableTTMeans <- mergeBack(TableTTMeans, "TAG_DATA")
  names(TableTTMeans) = c("TAG_DATA", "DATE", "TOUCH_TIME");
  
  TableTTMeans;
}
  
computeRackTMeans <- function (rackWDurations) {
  RackTMeans <- lapply(rackWDurations, computeTMeans)
  RackTMeans <- mergeBack(RackTMeans, "TAG_DATA")
  names(RackTMeans) = c("TAG_DATA", "DATE", "TOUCHES");
  
  RackTMeans;
}

computeRackTTMeans <- function (rackWDurations) {
  RackTTMeans <- lapply(rackWDurations, computeTTMeans)
  RackTTMeans <- mergeBack(RackTTMeans, "TAG_DATA")
  names(RackTTMeans) = c("TAG_DATA", "DATE", "TOUCH_TIME");
  
  RackTTMeans;
}

combineTogether <- function (TableTMeans, TableTTMeans, RackTMeans, RackTTMeans) {
  allTable <- merge(TableTMeans, TableTTMeans, all = TRUE);
  allRack <- merge(RackTMeans, RackTTMeans, all = TRUE);
  
  rbind(allRack, allTable);
}

computeAllTMeans <- function (all) {
  allTMeans <- lapply(split(all, all$TAG_DATA), computeTMeans);
  allTMeans <- mergeBack(allTMeans, "TAG_DATA");
  names(allTMeans)[3] = "TOUCHES";
  
  allTMeans;
}

#  print("Finding standard deviations for all items...");

computeAllTTMeans <- function (all) {
  allTTMeans <- lapply(split(all, all$TAG_DATA), computeTTMeans);
  allTTMeans <- mergeBack(allTTMeans, "TAG_DATA");
  names(allTTMeans)[3] = "TOUCH_TIME";
  
  allTTMeans;
}


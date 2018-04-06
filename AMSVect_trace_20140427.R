

traceLines <- function(con, schemaname, map.thinned) {

  # LINE TRACING ALGORITHM
  #
  # Author:
  # Philipp Hunziker, hunziker@icr.gess.ethz.ch
  #
  # Last Edited:
  # 27/04/2014
  #
  # Decription:
  # Takes a thinned raster map and returns continuous lines as a SpatialDataFrameLines object
  
  res.map <- res(map.thinned)
  component.length <- round(1.1*res.map[1], 14)
  diagonal.length <- round(1.1*sqrt(sum(res.map^2)), 14)
  
  pixels.spdf <- rasterToPoints(map.thinned, fun=function (x) x==1, spatial=TRUE)
  names(pixels.spdf) <- "value"
  pixels.spdf$pid <- 1:nrow(pixels.spdf)
  rs <- dbWritePoints(con, pixels.spdf, schemaname, "inputpoints", replace=TRUE)
  
  dbRunScript(con, "SQL Code/TraceLines_20140427.sql", return=FALSE, script.param=list(COMPLENGTH=component.length, DIAGLENGTH=diagonal.length, SCHEMANAME=schemaname)) 
  
  q.res <- dbSendQuery(con, statement=paste("SELECT COUNT(lid) FROM ", schemaname, ".outputlines;", sep=""))
  q.df <- fetch(q.res, -1)
  if (q.df$count > 0) {
    tracelines.sldf <- dbReadSpatial(con, schemaname, "outputlines", idcol="lid")
    snaplines.sldf <- snapNetwork(con, schemaname, tracelines.sldf, buffersize=diagonal.length, breakfirst=FALSE, snapAnywhere=FALSE)
    
    rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".outputlines;", sep=""))
    
    return(snaplines.sldf)
  } else {
    return(NULL)
  }
}
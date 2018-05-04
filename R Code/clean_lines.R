

deletePoints <- function(inputlines) {
# DELETE POINT GEOMETRIES FROM SPATIAL LINE DATA FRAME
#
# Author:
# Philipp Hunziker, hunziker@icr.gess.ethz.ch
#
# Last Edited:
# 23/05/2013
#
# Decription:
# Takes a SpatialLineDataFrame or a SpatialLines object and removes all lines consisting of only one coordinate 
# (i.e., that aren't really lines but points.) In case the input object is a SLDF, returns a SpatialLineDataFrame 
# with the same attributes as the input SLDF. In case the input object is a SL object, simply returns a SL object.
	
	sldf <- FALSE
	if (class(inputlines)[1] == "SpatialLinesDataFrame") {
		sldf <- TRUE
		input.df <- inputlines@data
		inputlines <- as.SpatialLines.SLDF(inputlines)
	}	
	
	keep.bool <- rep(NA, length(inputlines))
	for (l in 1:length(inputlines)) {
		geom <- inputlines[l]
		if (nrow(coordinates(geom)[[1]][[1]]) < 2) {
			keep.bool[l] <- FALSE
		} else {
			keep.bool[l] <- TRUE
		}
	}
	outputlines <- inputlines[keep.bool]
	
	if (sldf) {
		outputlines <- SpatialLinesDataFrame(outputlines, input.df[keep.bool,,drop=FALSE], match.ID=FALSE)
	}
	return(outputlines)
}


deleteShortLines <- function(con, schemaname, inputlines, minLength) {
# DELETE END-POINT-LINES SHORTER THAN THE SPECIFIED INPUT LENGTH
#
# Author:
# Philipp Hunziker, hunziker@icr.gess.ethz.ch
#
# Last Edited:
# 23/05/2013
#
# Decription:
# Takes a SpatialLineDataFrame or a SpatialLines object and removes all end-point lines that are shorter than
# the length specified in the argument minLength. End-point lines are lines that do not intersect with another
# line either at their start, or end point (or both).  In case the input object is a SLDF, returns a SLDF
# with the same attributes as the input SLDF. In case the input object is a SL object, simply returns a SL object.
	
	if (class(inputlines)[1] == "SpatialLinesDataFrame") {
		sldf <- TRUE
		realendpoints <- getRealEndPoints(con, schemaname, inputlines)
		endlines.bool <- inputlines$lid %in% realendpoints$lid
		input.df <- inputlines@data
		inputlines <- as.SpatialLines.SLDF(inputlines)
	} else {
		sldf <- FALSE
		inputlines.sldf <- SpatialLinesDataFrame(inputlines, data.frame(lid=cbind(c(1:length(inputlines)))), match.ID=FALSE)
		realendpoints <- getRealEndPoints(con, schemaname, inputlines.sldf)
		endlines.bool <- inputlines.sldf$lid %in% realendpoints$lid
	}
	
	line.lengths <- rep(NA, length(inputlines))
	for (l in 1:length(inputlines)) {
		geom <- inputlines[l]
		if (nrow(coordinates(geom)[[1]][[1]]) < 2) {
			length <- 0
		} else {
			length <- gLength(geom)
		}
		line.lengths[l] <- length
	}
	keep <- !endlines.bool | line.lengths >= minLength
	if (!(any(keep))) {
	  return(data.frame(empty=as.character()))
	}
	outputlines <- inputlines[keep]
  
	if (sldf) {
		outputlines <- SpatialLinesDataFrame(outputlines, input.df[keep,,drop=FALSE], match.ID=FALSE)
	}
	return(outputlines)
}


removeDuplicates <- function(con, schemaname, inputlines) {
# REMOVE DUPLICATE LINE GEOMETRIES
#
# Author:
# Philipp Hunziker, hunziker@icr.gess.ethz.ch
#
# Last Edited:
# 23/05/2013
#
# Decription:
# Takes a SpatialLineDataFrame and removes duplicate lines, i.e. geometrically identical line geometries. 
# Removal is performed regardless of line directionality (line AB is the same as line BA), and regardless
# of differences in the line attributes. Returns a SLDF  with the same attributes as the input SLDF.
	
	# Write 'inputLines' spatial data frame to table on DB
	dbWriteSpatial(con, inputlines, schemaname, "inputlines", T)
	
	# Execute SQL script on DB that returns a table with nondupliate IDs
	nondup.df <- dbRunScript(con, path="SQL Code/select_nonduplicate_lines.sql", script.param=list(SCHEMANAME=schemaname), return=TRUE)	
	
	# Clean up on DB
	rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".inputlines;", sep=""))
	
	# Select non-duplicate inputlines
	outputlines <- inputlines[inputlines$lid %in% nondup.df$lid,]
	
	return(outputlines)
}



getRealEndPoints <- function(con, schemaname, inputlines) {
# CREATE A SPDF OF REAL END POINTS FROM A SLDF
#
# Author:
# Philipp Hunziker, hunziker@icr.gess.ethz.ch
#
# Last Edited:
# 23/05/2013
#
# Decription:
# Generates a SpatialPointDataFrame with real end-points (REP) from a given SpatialLinesDataFrame.
# REP are line start or end points that do not intersect with any other line in the input SLDF.
# Returns a SPDF with REP and one attribute column called "pid" with unique point IDs.
	
	# Write 'inputLines' spatial data frame to table on DB
	dbWriteSpatial(con, inputlines, schemaname, "inputlines", T)
	
	# Execute SQL script on DB that creates a table with real end points in new 'outputpoints' table
	dbRunScript(con, path="SQL Code/select_real_endpoints.sql", script.param=list(SCHEMANAME=schemaname))
	
	# Get real end points from DB and clean up on DB
	realendpoints.spdf <- dbReadSpatial(con, schemaname, "outputpoints", idcol="pid")
	rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".outputpoints;", sep=""))
	rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".inputlines;", sep=""))
	
	return(realendpoints.spdf)
}



standardizeGeom <- function(con, schemaname, input.sdf, idcol="lid") {
# STANDARDIZE COORDINATES OF GEOMETRIES IN INPUT SPATIAL DATA FRAME
#
# Author:
# Philipp Hunziker, hunziker@icr.gess.ethz.ch
#
# Last Edited:
# 29/05/2013
#
# Decription:
# Takes a spatial data frame and standardizes the coordinate values of the respective
# geometries (i.e., slightly reduces precision). 
	
	# Remove everything but the id column from the input.sdf data frame
	data.df <- data.frame((input.sdf@data)[,which(names(input.sdf@data) == idcol)])
	names(data.df) <- "lid"
	working.sdf <- input.sdf
	working.sdf@data <- data.df
	
	# Write 'working.sdf' spatial data frame to table on DB
	dbWriteSpatial(con, working.sdf, schemaname, "inputgeoms", replace=T)
	
	# Execute SQL script on DB that creates a new table with standardized geometries
	rs <- dbRunScript(con, path="SQL Code/standardize_geom.sql", script.param=list(SCHEMANAME=schemaname))	
	
	# Get standardized geoms DB and clean up on DB
	stand.sdf <- dbReadSpatial(con, schemaname, "newgeoms", idcol="lid")
	rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".inputgeoms;", sep=""))
	rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".newgeoms;", sep=""))
	
	# Attach initially removed data back to output data frame
	stand.df <- stand.sdf@data
	output.df <- merge(stand.df, input.sdf@data, by=idcol, x.all=TRUE, y.all=FALSE, sort=FALSE)
	output.sdf <- stand.sdf
	output.sdf@data <- output.df
	
	return(output.sdf)
}



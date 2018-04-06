
breakNodes <- function (con, schemaname, inputlines, idcol="lid", verbose=FALSE, original.lid=FALSE) {
	
	# Remove everything but the id column from the inputline data frame
	data.df <- data.frame((inputlines@data)[,which(names(inputlines@data) == idcol)])
	names(data.df) <- "lid"
	inputlines@data <- data.df
	
	# Write 'inputLines' spatial data frame to table on DB
	if (verbose) {
		print(paste("Writing", length(inputlines), "lines on DB..."))
		flush.console()
	}
	dbWriteSpatial(con, inputlines, schemaname, "inputlines", T)
	
	# Execute SQL script on DB that creates a table with broken lines
	if (verbose) {
		print(paste("Compiling break nodes on DB..."))
		flush.console()
	}
	rv <- dbRunScript(con, path="SQL Code/BreakNodes2_20140425.sql", script.param=list(SCHEMANAME=schemaname))
	
	# Get table with broken lines and clean up on DB
	rs <- dbSendQuery(con, statement=paste("SELECT lid FROM ", schemaname, ".newlines;", sep=""))
	rs.df <- fetch(rs, -1)
	if (nrow(rs.df) > 0) {
		splitlines <- dbReadSpatial(con, schemaname, "newlines", idcol="lid")
	} else {
		if (verbose) {
			print("No lines to be broken. Returning input SpatialDataFrame.")
		}
    if (original.lid) {
      inputlines$lid_original <- NA
    }
		return(inputlines)
	}
	rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".newlines;", sep=""))
	rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".inputlines;", sep=""))
	
	# Remove original broken lines from inputlines, and instead append newly broken lines
	if (verbose) {
		print(paste(length(unique(splitlines$lid_original)), "lines broken."))
	}
	inputlines.nobreak <- inputlines[!(inputlines$lid %in% splitlines$lid_original),]
	if (original.lid) {
		inputlines.nobreak$lid_original <- NA
		outputlines <- mergeSLDF(inputlines.nobreak, splitlines)
	} else {
		splitlines <- splitlines[,1]
		outputlines <- mergeSLDF(inputlines.nobreak, splitlines)
	}
	
	if (verbose) {
		print(paste("Done."))
	}
	return(outputlines)
}


mergeSLDF <- function(df1, df2) {
	merge.df <- rbind(df1@data, df2@data)
	l1.ls <- df1@lines
	l2.ls <- df2@lines
	merge.ls <- c(l1.ls, l2.ls)
	merge.sl <- SpatialLines(merge.ls)
	merge.sldf <- SpatialLinesDataFrame(merge.sl, merge.df, match.ID = FALSE)
	return(merge.sldf)
}

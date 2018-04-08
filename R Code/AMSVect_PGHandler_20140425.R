# PostGIS handler functions

require(RPostgreSQL)
require(rgeos)

getPGConn <- function(dbname, port, host, usr, pwd) {
	drv <- dbDriver("PostgreSQL")
	con <- dbConnect(drv, dbname = dbname, port=port, host=host, user=usr, password=pwd)
	return(con)
}

dbWriteSpatial <- function(con, spatialdf, schemaname=NULL, tablename, replace=FALSE) {
  
  # Create well known text
  spatialwkt <- writeWKT(spatialdf, byid=TRUE)
  spatialdf$wkt <- spatialwkt
  
  # Add unique index
  spatialdf$spatial_id <- 1:nrow(spatialdf)
  
  # Upload data to DB
  data.df <- spatialdf@data
  schemaname <- ifelse(is.null(schemaname), "public", schemaname)
  rv <- dbWriteTable(con, c(schemaname, tablename), data.df, overwrite=replace, row.names=FALSE)
  
  # Create geometry column and clean up table
  schema.table <- paste(schemaname, ".", tablename, sep="")
  query1 <- paste("ALTER TABLE ", schema.table, " ADD COLUMN the_geom GEOMETRY;", sep="")
  query2 <- paste("UPDATE ", schema.table, " SET the_geom = ST_GEOMETRYFROMTEXT(t.wkt) FROM ", schema.table, 
                  " t  WHERE t.spatial_id = ", schema.table, ".spatial_id;", sep="")
  query3 <- paste("ALTER TABLE ", schema.table, " DROP COLUMN spatial_id;")
  query4 <- paste("ALTER TABLE ", schema.table, " DROP COLUMN wkt;")
  er <- dbSendQuery(con, statement=query1)
  er <- dbSendQuery(con, statement=query2)
  er <- dbSendQuery(con, statement=query3)
  er <- dbSendQuery(con, statement=query4)
  
  return(TRUE)
}


dbWritePoints <- function(con, spatialdf, schemaname, tablename, replace=FALSE, maxrow=NULL) {
  data.df <- spatialdf@data
  # Create new table if table doesn't exist or option replace is enabled
  if (!is.null(schemaname)) {
    er <- dbSendQuery(con, statement=paste("SELECT * FROM information_schema.tables WHERE table_schema ='", schemaname,"' AND table_name = '", tablename, "';", sep=""))
    long.tablename = paste(schemaname, ".", tablename, sep="")
  } else {
    er <- dbSendQuery(con, statement=paste("SELECT * FROM information_schema.tables WHERE table_name = '", tablename, "';", sep=""))
    long.tablename = tablename
  }
  exists <- ifelse(nrow(fetch(er, -1)) > 0, TRUE, FALSE)
  if (!exists | replace) {
    # Build create table query
    colnames <- c(names(data.df), "x", "y")
    colnames <- sub("[.]", "_", colnames)  # Replace dots in column names with underscores
    colclass <- sapply(data.df[1,], class)
    colclass.sql <- c(ifelse(colclass == "integer", "int", ifelse(colclass == "numeric", "real", "varchar")), "real", "real")
    colcreate.sql <- paste(paste(colnames, colclass.sql), collapse=",")
    q <- dbSendQuery(con, statement = paste("DROP TABLE IF EXISTS", long.tablename, ";"))
    q <- dbSendQuery(con, statement = paste("CREATE TABLE", long.tablename," (", colcreate.sql, ");"))
  }
  # Add coordinates of points to data frame as numeric columns
  coords <- coordinates(spatialdf)
  data.df$x <- coords[,1]
  data.df$y <- coords[,2]
  # Upload values to DB in packages
  colclass <- sapply(data.df[1,], class)
  string.bol <- ifelse(colclass %in% c("character", "factor"), TRUE, FALSE)
  if (is.null(maxrow)) {
    maxrow <- nrow(data.df)
  }
  package.n <- ceiling(nrow(data.df)/maxrow)
  print(paste("Writing file to data base in", package.n, "package(s)..."))
  pb <- txtProgressBar(min = 0, max = package.n, style = 3)
  beginrow <- 1
  for (b in 1:package.n) {
    endrow <- ifelse(beginrow + maxrow - 1 > nrow(data.df), nrow(data.df), beginrow + maxrow - 1)
    package.df <- data.df[beginrow:endrow,]
    for (i in 1:ncol(package.df)) {
      if(string.bol[i]) {
        package.df[,i] <- sapply(package.df[,i], function(x) gsub("[']", "''", x))
        package.df[,i] <- paste("'", package.df[,i], "'", sep="")
      } else {
        package.df[,i] <- paste(package.df[,i])
        package.df[,i] <- ifelse(package.df[,i] == "Inf" , "NULL", package.df[,i])
        package.df[,i] <- ifelse(package.df[,i] == "-Inf" , "NULL", package.df[,i])
      }
    }
    package.df$sql <- apply(package.df, 1, function (x) paste("(",paste(x, collapse=","), ")"))
    values.sql <- paste(package.df$sql, collapse=",")
    iq <- dbSendQuery(con, statement=paste("INSERT INTO", long.tablename, "VALUES", values.sql, ";"))
    beginrow <- endrow + 1
    setTxtProgressBar(pb, b)
  }
  close(pb)
  
  # Create point geometries on DB
  cq <- dbSendQuery(con, statement=paste("ALTER TABLE", long.tablename, "ADD COLUMN the_geom geometry;"))
  cq <- dbSendQuery(con, statement=paste("UPDATE", long.tablename, "SET the_geom = ST_MAKEPOINT(x,y);"))
  cq <- dbSendQuery(con, statement=paste("ALTER TABLE", long.tablename, "DROP COLUMN x; ALTER TABLE", long.tablename, "DROP COLUMN y;"))
}



dbReadSpatial <- function(con, schemaname=NULL, tablename, geomcol= NULL, idcol=NULL) {
  # Dynamic query builder
  if (!is.null(schemaname)) {
    q.res <- dbSendQuery(con, statement=paste("SELECT column_name FROM information_schema.columns WHERE table_name ='", tablename, "' AND table_schema ='", schemaname, "';", sep=""))
    long.tablename = paste(schemaname, ".", tablename, sep="")
  } else {
    q.res <- dbSendQuery(con, statement=paste("SELECT column_name FROM information_schema.columns WHERE table_name ='", tablename, "';", sep=""))
    long.tablename = tablename
  }
  q.df <- fetch(q.res, -1)
  if (is.null(geomcol)) {
    geomcol <- "the_geom"
  }
  if (!(geomcol %in% q.df[,1])) {stop(paste("No", geomcol, "column in specified table."))}
  if (!is.null(idcol)) {
    if (!(idcol %in% q.df[,1])) { stop(paste("Specified idname '", idcol, "' not found."))}
  }
  query <- paste("SELECT", paste(q.df[,1][q.df[,1] != geomcol], collapse=", "), paste(", ST_ASTEXT(", geomcol, ") AS the_geom FROM", sep=""), long.tablename, ";")
  # Execute query
  t.res <- dbSendQuery(con, statement=query)
  t.df <- fetch(t.res, -1)
  # Get geometry ID column number (if any)
  if (!is.null(idcol)) {
    idcolnum <- which(names(t.df) == idcol)
  } else {
    t.df$id.new <- 1:nrow(t.df)
    idcolnum <- which(names(t.df) == "id.new")
  }
  # Get geometry column number
  geomcolnum <- which(names(t.df) == "the_geom")
  # Build spatial data frame
  spatial.df <- NULL
  if (grepl("LINE", t.df[1,geomcolnum])) {
    spatial.list <- vector("list", 0)
    for (i in 1:nrow(t.df)) {
      spatial.list[[length(spatial.list) + 1]] <- (readWKT(text=t.df[i,geomcolnum], id=paste(t.df[i,idcolnum])))@lines[[1]]
    }
    spatial <- SpatialLines(spatial.list)	
    data.df <- data.frame(t.df[,-geomcolnum])
    names(data.df) <- names(t.df)[-geomcolnum]
    spatial.df <-  SpatialLinesDataFrame(spatial, data.df, match.ID=FALSE)
  }
  if (grepl("POINT", t.df[1,geomcolnum])) {
    coord.df <- data.frame(x=numeric(), y=numeric())
    for (i in 1:nrow(t.df)) {
      coord.df[nrow(coord.df) + 1,] <- (readWKT(text=t.df[i,geomcolnum], id=paste(t.df[i,idcolnum])))@coords
    }
    data.df <- data.frame(t.df[,-geomcolnum])
    names(data.df) <- names(t.df)[-geomcolnum]
    spatial.df <-  SpatialPointsDataFrame(coord.df, data.df, match.ID=FALSE)
  }
  if (grepl("POLYGON", t.df[1,geomcolnum])) {
    spatial.list <- vector("list", 0)
    for (i in 1:nrow(t.df)) {
      spatial.list[[length(spatial.list) + 1]] <- (readWKT(text=t.df[i,geomcolnum], id=paste(t.df[i,idcolnum])))@polygons[[1]]
    }
    spatial <- SpatialPolygons(spatial.list)	
    data.df <- data.frame(t.df[,-geomcolnum])
    names(data.df) <- names(t.df)[-geomcolnum]
    spatial.df <-  SpatialPolygonsDataFrame(spatial, data.df, match.ID=FALSE)
  }
  return(spatial.df)
}


dbRunScript <- function(con, path, return=FALSE, script.param=vector("list", 0), param.quote=NULL) {
# Execute SQL script on DB
	script.sql <- readChar(path, file.info(path)$size)
	script.sql <- gsub("\n", " \n ", script.sql, fixed = TRUE)
	script.sql <- gsub("\t", " ", script.sql, fixed = TRUE)
	script.sql <- gsub("﻿", "", script.sql, fixed = TRUE)
	if (length(script.param) > 0) {
		if (is.null(param.quote)) {
			param.quote <- rep(FALSE, length(script.param))
		} else {
			if (length(param.quote) != length(script.param)) {
				stop("'param.quote' and 'script.param' are of unequal length.")
			}
		}
		for (i in 1:length(script.param)) {
			param.name <- names(script.param)[i]
			param.value <- script.param[[i]]
			param.value <- ifelse(param.quote[i], paste("'", param.value, "'", sep=""), param.value)
			script.sql <- gsub(param.name, param.value, script.sql, fixed = TRUE)
		}
	}
	rs <- dbSendQuery(con, statement=script.sql)
	if(return) {
		rs.df <- fetch(rs, -1)
		return(rs.df)
	}
} 


##### Create Schema
dbCreateSchema <- function(con, schemaname) {
  rs <- dbSendQuery(con, statement=paste("DROP SCHEMA IF EXISTS ", schemaname, " CASCADE;", sep=""))
  rs <- dbSendQuery(con, statement=paste("CREATE SCHEMA ", schemaname, ";", sep=""))
}

dbDropSchema <- function(con, schemaname) {
  rs <- dbSendQuery(con, statement=paste("DROP SCHEMA IF EXISTS ", schemaname, " CASCADE;", sep=""))
}


library(igraph)

graph.split.SLDF <- function(inputlines, plot=FALSE) {
# Create a graph from splitted SLDF (every corner in SLDF becomes a vertex)
  
	# Store line attributes for later usage
	inputlines.df <- inputlines@data
	
	# Create vertex and edge list from inputlines SLDF
	coordinates.list <-  lapply(coordinates(as.SpatialLines.SLDF(inputlines)), function(x) (x[[1]]))
	for (l in 1:length(coordinates.list)) {
	  coordinates.list[[l]] <- data.frame(x=(coordinates.list[[l]])[,1], y=(coordinates.list[[l]])[,2], lid=inputlines.df$lid[l])
  }
	coordinates.df <- data.frame(do.call(rbind, coordinates.list))
  names(coordinates.df) <- c("x", "y", "lid")
  coordinates.df$lid <- as.character(coordinates.df$lid)
  
  coordinates.df$order <- 1:nrow(coordinates.df)
	verteces.df <- unique(coordinates.df[,c("x", "y")])
	verteces.df$pid <- c(1:nrow(verteces.df))
	coordinates.df <- merge(coordinates.df, verteces.df, by=c("x", "y"), all.x=TRUE, all.y=FALSE, sort=FALSE)
  coordinates.df <- coordinates.df[order(coordinates.df$order),]
  
  coordinates2.df <- coordinates.df
  coordinates2.df$order <- NA
  coordinates2.df$order[2:nrow(coordinates2.df)] <- 1:(nrow(coordinates.df)-1)
  coordinates2.df <- coordinates2.df[,c("pid", "order", "lid")]
  names(coordinates2.df) <- c("pid2", "order", "lid2")
  
  coordinates.df <- merge(coordinates.df, coordinates2.df, by="order", all.x=TRUE, all.y=FALSE, sort=FALSE)
	coordinates.df <- coordinates.df[order(coordinates.df$order),]
	coordinates.df <- coordinates.df[coordinates.df$lid==coordinates.df$lid2 & !is.na(coordinates.df$pid2),]
  
  edge.df <- coordinates.df[,c("pid", "pid2", "lid")]
  names(edge.df) <- c("vertex1", "vertex2", "lid")
	edge.df$eid <- c(1:nrow(edge.df))
	
	# Add line attributes to edge list
	edge.df <- merge(edge.df, inputlines.df, by="lid", all.x=TRUE, all.y=FALSE, sort=FALSE)
	edge.df <- data.frame(vertex1=edge.df$vertex1, vertex2=edge.df$vertex2, edge.df[,!names(edge.df) %in% c("vertex1", "vertex2"), drop=FALSE])
	
	# Create graph
	graph <- graph.data.frame(edge.df, directed=FALSE)
	
	# Add line coordinates as vertex attributes
	graph.verteces.df <- data.frame(pid=as.numeric(V(graph)$name))
	graph.verteces.df <- merge(graph.verteces.df, verteces.df, by="pid", all=TRUE, sort=FALSE)
	graph <- set.vertex.attribute(graph, name="x", value=graph.verteces.df$x)
	graph <- set.vertex.attribute(graph, name="y", value=graph.verteces.df$y)
	
	if(plot) {
    plot(graph, layout=layout.sldf(graph), vertex.size=1, vertex.label=NA, vertex.color="red",
				edge.color="black")
	}
	
	return(graph)
}


graph.SLDF <- function(inputlines, plot=FALSE) {
  # Create a graph from SLDF (every line end in SLDF becomes vertex)
  
  # Store line attributes for later usage
  inputlines.df <- inputlines@data
  
  # Create vertex and edge list from inputlines SLDF
  lid.vec <- as.character(inputlines$lid)
  length.vec <- gLength(inputlines, byid=TRUE)
  start.coordinates.list <-  lapply(coordinates(as.SpatialLines.SLDF(inputlines)), function(x) (x[[1]])[1,])
  end.coordinates.list <-  lapply(coordinates(as.SpatialLines.SLDF(inputlines)), function(x) (x[[1]])[nrow((x[[1]])),])
  verteces.df <- data.frame(unique(rbind(do.call("rbind", start.coordinates.list), do.call("rbind", end.coordinates.list))))
  names(verteces.df) <- c("x", "y")
  verteces.df$pid <- 1:nrow(verteces.df)
  edge.df <- data.frame(pid=numeric(), pid2=numeric(), lid=character(), weight=numeric(), stringsAsFactors=FALSE)
  
  for (i in 1:nrow(inputlines)) {
    startx <- (start.coordinates.list[[i]])[1]
    starty <- (start.coordinates.list[[i]])[2]
    endx <- (end.coordinates.list[[i]])[1]
    endy <- (end.coordinates.list[[i]])[2]
    pid <- verteces.df$pid[verteces.df$x == startx & verteces.df$y == starty]
    pid2 <- verteces.df$pid[verteces.df$x == endx & verteces.df$y == endy]
    lid <- lid.vec[i]
    weight <- length.vec[i]
    edge.df[i,1:2] <- c(pid, pid2)
    edge.df[i,3] <- lid
    edge.df[i,4] <- weight
  }
  edge.df$eid <- 1:nrow(edge.df)
  names(edge.df) <- c("vertex1", "vertex2", "lid", "weight", "eid")
  
  # Add line attributes to edge list
  edge.df <- merge(edge.df, inputlines.df, by="lid", all.x=TRUE, all.y=FALSE, sort=FALSE)
  edge.df <- data.frame(vertex1=edge.df$vertex1, vertex2=edge.df$vertex2, edge.df[,!names(edge.df) %in% c("vertex1", "vertex2"), drop=FALSE])
  
  # Create graph
  graph <- graph.data.frame(edge.df, directed=FALSE)
  
  # Add line coordinates as vertex attributes
  graph.verteces.df <- data.frame(pid=as.numeric(V(graph)$name))
  graph.verteces.df <- merge(graph.verteces.df, verteces.df, by="pid", all=TRUE, sort=FALSE)
  graph <- set.vertex.attribute(graph, name="x", value=graph.verteces.df$x)
  graph <- set.vertex.attribute(graph, name="y", value=graph.verteces.df$y)
  
  if(plot) {
    plot(graph, layout=layout.sldf(graph), vertex.size=1, vertex.label=NA, vertex.color="red",
         edge.color="black")
  }
  
  return(graph)
}


layout.sldf <- function(graph) {
	x <- V(graph)$x
	y <- V(graph)$y
	return(cbind(x,y))
}


setDist <- function(graph) {
	edge.df <- get.data.frame(graph, what="edges")
	vertex.df <- get.data.frame(graph, what="vertices")
	vertex.df <- vertex.df[,c("name", "x", "y")]
	names(vertex.df)[1] <- "pid"
	names(edge.df)[1] <- "pid"
	edge.df <- merge(edge.df, vertex.df, by="pid", all.x=TRUE, all.y=FALSE, sort=FALSE)
	names(edge.df)["x" == names(edge.df)] <- "x1"
	names(edge.df)["y" == names(edge.df)] <- "y1"
	names(edge.df)[1:2] <- c("from", "pid")
	edge.df <- merge(edge.df, vertex.df, by="pid", all.x=TRUE, all.y=FALSE, sort=FALSE)
	names(edge.df)["x" == names(edge.df)] <- "x2"
	names(edge.df)["y" == names(edge.df)] <- "y2"
	edge.df$dist <- sqrt((edge.df$x2 - edge.df$x1)^2 + (edge.df$y2 - edge.df$y1)^2)
	dist.df <- edge.df[, c("eid", "dist")]
	
	graph.edge.df <- data.frame(eid=E(graph)$eid)
	graph.edge.df <- merge(graph.edge.df, dist.df, by="eid", all=TRUE, sort=FALSE)
	
	graph <- set.edge.attribute(graph, "weight", value=graph.edge.df$dist)
	
	return(graph)
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


snapNetwork <- function(con, schemaname, inputlines, buffersize=20, breakfirst=TRUE, snapAnywhere=TRUE, partitionName=NULL, parallel=FALSE, OS="Win", cores=4) {
	
	# Transform inputlines into graph
	if (breakfirst) {
    inputlines <- breakNodes(con, schemaname, inputlines)
	}
  
  if (snapAnywhere) {
    # Create graph from minimally split line segments
	  graph <- graph.split.SLDF(inputlines)
  	# Add line length as edge attribute
	  graph <- setDist(graph)
  } else {
    # Create graph from line segments
    graph <- graph.SLDF(inputlines)
  } 
	
	# Get a table of all vertex IDs ("pid") together with corresponding line IDs ("lid") and edge IDs ("eid")
	edge.df <- get.data.frame(graph, what="edges")
	from.df <- edge.df[,c("from", "lid", "eid")]
	to.df <- edge.df[,c("to", "lid", "eid")]
	names(from.df) <- names(to.df) <- c("pid", "lid", "eid")
	apoints.df <- rbind(from.df, to.df)
	apoints.df <- unique(apoints.df)
	
	# Add coordinates to vertex ID table and create according SpatialPointDataFrame
	vertex.df <- get.data.frame(graph, what="vertices")
	names(vertex.df)[1] <- "pid"
	apoints.df <- merge(apoints.df, vertex.df, by="pid", all.x=TRUE, all.y=TRUE, sort=FALSE)
	apoints.spdf <- SpatialPointsDataFrame(apoints.df[,c(4:5)], apoints.df[,c(1:3)], match.ID = FALSE)
	
  # Add partition name to apoints
  if (!is.null(partitionName)) {
    apoints.df <- apoints.spdf@data
    apoints.df$order <- 1:nrow(apoints.df)
    partition.df <- (inputlines@data)[,c("lid", partitionName)]
    apoints.df <- merge(apoints.df, partition.df, by="lid", all.x=TRUE, all.y=FALSE, sort=FALSE)
    apoints.df <- apoints.df[order(apoints.df$order),]
    apoints.df <- apoints.df[,!(names(apoints.df) == "order")]
    apoints.spdf@data <- apoints.df
  }
  
	# Get a table with snapline candidates 
	# (shortest vertex combinations connecting two edges with maximal distance of "buffersize")
	rs <- dbWritePoints(con, apoints.spdf, schemaname, "apoints", TRUE)
  if (is.null(partitionName)) {
    snapcandidates.df <- dbRunScript(con, "SQL Code/snap_line_points.sql", return=TRUE, script.param=list(BUFFERSIZE=buffersize, SCHEMANAME=schemaname)) 
  } else {
    # Only looks for candidates snappling lines *across* partitions!
    snapcandidates.df <- dbRunScript(con, "SQL Code/snap_line_points_partition.sql", return=TRUE, script.param=list(BUFFERSIZE=buffersize, SCHEMANAME=schemaname, PARTITIONNAME=partitionName)) 
  }
  rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".apoints_b;", sep=""))
	rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".apoints;", sep=""))
	
  if(nrow(snapcandidates.df) == 0) {
    return(inputlines)
  }
  
	# Calculate the effectiveness of each snapline candidate
	# Effectiveness measures how much shorter the candidate snapline makes the path between the vertices of the candidate snapline
  if (parallel) {
    snapcandidates.df$effectiveness <- NA
    chunksize <- ceiling(nrow(snapcandidates.df)/12)
    snapcandidates.list <- vector("list", 0)
    c <- 1
    startrow <- 1
    repeat {
      endrow <- min(nrow(snapcandidates.df), startrow + (chunksize-1))
      snapcandidates.list[[c]] <- snapcandidates.df[startrow:endrow,]
      if (endrow == nrow(snapcandidates.df)) {
        break
      }
      startrow <- endrow + 1
      c <- c + 1
    }
    # Calculate effectiveness in parallel
    if (OS == "Win") {
      cl <- makeCluster(cores)
      registerDoSNOW(cl)
    } else {
      registerDoMC(cores)
    }
    result.list <- foreach(i=1:length(snapcandidates.list), .packages=c("igraph"), .export=c("snapcandidates.list", "graph")) %dopar% {
      thiscand.df <- snapcandidates.list[[i]]
      for (s in 1:nrow(thiscand.df)) {
        cand.from <- thiscand.df$pid[s]
        cand.to <- thiscand.df$pid2[s]
        cand.dist <- thiscand.df$dist[s]
        orig.path <- shortest.paths(graph, v=V(graph)[V(graph)$name == cand.from], 
                                    to=V(graph)[V(graph)$name == cand.to], mode = "all", algorithm = "automatic")
        thiscand.df$effectiveness[s] <- orig.path/cand.dist
      }
      thiscand.df
    }
    snapcandidates.df <- do.call("rbind", result.list)
    
  } else {
    snapcandidates.df$effectiveness <- NA
  	for (s in 1:nrow(snapcandidates.df)) {
  		cand.from <- snapcandidates.df$pid[s]
  		cand.to <- snapcandidates.df$pid2[s]
  		cand.dist <- snapcandidates.df$dist[s]
  		orig.path <- shortest.paths(graph, v=V(graph)[V(graph)$name == cand.from], 
  				to=V(graph)[V(graph)$name == cand.to], mode = "all", algorithm = "automatic")
  		snapcandidates.df$effectiveness[s] <- orig.path/cand.dist
  	}
  }

	
	# Only keep snapline candidates with effectiveness >= 5
	snapcandidates.df <- snapcandidates.df[snapcandidates.df$effectiveness >= 5,]
	if(nrow(snapcandidates.df) == 0) {
	  return(inputlines)
	}
  
	# Only keep the most effective snapline connecting two existing lines
	snapcandidates.df$effectiveness <- ifelse(snapcandidates.df$effectiveness == Inf, 999999, snapcandidates.df$effectiveness)
	r <- dbWriteTable(con, c(schemaname, "snapcandidates"), snapcandidates.df)
	snapeffcand.df <- dbRunScript(con, "SQL Code/select_effective_snap.sql", return=TRUE, script.param=list(SCHEMANAME=schemaname)) 
	rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".snapcandidates;", sep=""))
	snapeffcand.df <- snapeffcand.df[with(snapeffcand.df, order(dist)), ]
	
	# Iterate through candidate snaplines (from shortest to longest) and add them to inputlines iff they still feature effectiveness >= 5,
	# given the previously added lines. This step ensures that snaplines made redundant by shorter snaplines aren't added to inputlines.
	input.lid <- as.character(inputlines$lid)
  newlines.ls <- vector("list", 0)
	newlines.vec <- vector("character", 0)
	snap.graph <- graph
	for (s in 1:nrow(snapeffcand.df)) {
    
    snap.from <- snapeffcand.df$pid[s]
		snap.to <- snapeffcand.df$pid2[s]
		snap.dist <- snapeffcand.df$dist[s]
		snap.eid <- length(E(snap.graph)$eid) + 1
		snap.lid <- paste("N-", snap.eid, sep="")
    if (snap.lid %in% input.lid) {
      snap.lid <- paste("N-", snap.lid, sep="")
    }
		
		snap.path <- shortest.paths(snap.graph, v=V(snap.graph)[V(snap.graph)$name == snap.from], 
				to=V(snap.graph)[V(snap.graph)$name == snap.to], mode = "all", algorithm = "automatic")	
		snap.eff <- snap.path/snap.dist
		
		if (snap.eff >= 5) {
			snap.graph <- add.edges(snap.graph, c(snap.from, snap.to), attr=list(weight=snap.dist, eid=snap.eid, lid=snap.lid)) 
			coords <- rbind(vertex.df[vertex.df$pid==snap.from, c("x", "y")], vertex.df[vertex.df$pid==snap.to, c("x", "y")])
			newlines <- Lines(list(Line(coords)), ID=snap.lid)
			newlines.ls[length(newlines.ls) + 1] <- newlines
			newlines.vec <- c(newlines.vec, snap.lid)
		}
	}
	newlines.sl <- SpatialLines(newlines.ls)
	newlines.sldf <- SpatialLinesDataFrame(newlines.sl, data.frame(lid=newlines.vec), match.ID = FALSE)
  
  # Ensure that newlines.sldf has the same columns as inputlines
  input.df <- inputlines@data
  input.names <- names(input.df)
  input.names <- input.names[input.names != "lid"]
  newdata <- newlines.sldf@data
  if (length(input.names) > 0) {
    newmat <- matrix(NA, nrow(newdata), length(input.names))
    newdata <- cbind(newdata, newmat)
    names(newdata) <- c("lid", input.names)
    if(!is.null(partitionName)) {
      newdata[,"partition"] <- 99
    }
  }
  newlines.sldf@data <- newdata
  
	# Create output SpatialLineDataFrame
	snaplines.sldf <- mergeSLDF(inputlines, newlines.sldf)
	
	return(snaplines.sldf)
}



simplifyNetwork <- function(inputlines, maxlength, mincentrality) {
  
  ## Delete short line sections with low edge betweenness centrality
  
  baselines <- inputlines[1:1000,]
  
  # Transform inputlines into graph
  graph <- graph.SLDF(baselines, plot=FALSE)
  
  c <- 0
  repeat {
    # Calculate betweenness edge centrality
    btw <- edge.betweenness(graph, e=E(graph), directed = FALSE)
    length <- E(graph)$weight
    if (any(btw < mincentrality & length < maxlength)) {
        deledges <- E(graph)[btw < mincentrality & length < maxlength]
        del.lids <- deledges$lid
        baselines <- baselines[!(baselines$lid %in% del.lids),]
        baselines <- mergeLines(baselines, verbose=TRUE)
        graph <- graph.SLDF(baselines, plot=FALSE)
    } else {
      break
    }
    c <- c + 1
    print(c)
    flush.console()
  }
  
  return(baselines)
}




cleanNetwork <- function(con, schemaname, inputlines, maxlength, maxdetour= 4*maxlength) {
  # Delete short, unnecessary lines
  
  baselines <- inputlines
  
  repeat {
    
    graph <- graph.SLDF(baselines, plot=FALSE)
    
    # Get edges shorter than maxlength
    cand.e <- E(graph)[E(graph)$weight < maxlength]
    cand.lid <- cand.e$lid
    cand.dist <- cand.e$weight
    cand.lid <- cand.lid[order(-cand.dist)]
    
    rem <- 0
    delete.lid <- vector("character", 0)
    if (length(cand.lid) > 0) {
      for (i in 1:length(cand.lid)) {
        this.lid <- cand.lid[i]
        this.cand <- E(graph)[E(graph)$lid == this.lid]
        this.v <- V(graph)[adj(this.cand)]
        if (length(this.v) > 1) {
          this.v1 <- this.v[c(TRUE, FALSE)]
          this.v2 <- this.v[c(FALSE, TRUE)]
          next.lid <- E(graph)[adj(this.v1)]
          prev.lid <- E(graph)[adj(this.v2)]
          if (length(next.lid) > 1 & length(prev.lid) > 1) {
            alt.graph <- delete.edges(graph, this.cand)
            dist <- shortest.paths(alt.graph, v=this.v1$name, to=this.v2$name)
            if (dist < maxdetour) {
              graph <- delete.edges(graph, this.cand)
              delete.lid[length(delete.lid)+1] <- this.lid
              rem <- rem + 1
            }
          }
        }
      }
      if (rem == 0) {
        break
      }
      baselines <- baselines[!baselines$lid %in% delete.lid,]
      baselines <- mergeLines(baselines, verbose=TRUE)
      baselines <- deleteShortLines(con, schemaname, baselines, maxlength)
      baselines <- mergeLines(baselines, verbose=TRUE)
    } else {
      break
    }
  } 

  return(baselines)
}


  

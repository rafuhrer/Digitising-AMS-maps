library(igraph)

##### Normalize coordinates of inputlines and inputpixels
normalizeCoordinates <- function(inputlines, inputpixels) {
  
  xmin <- xmin(inputpixels)
  ymin <- ymin(inputpixels)
  xmax <- xmax(inputpixels)
  ymax <- ymax(inputpixels)
  xres <- res(inputpixels)[1]
  yres <- res(inputpixels)[2]
  
  ip.norm <- inputpixels
  xmin(ip.norm) <- 0
  xmax(ip.norm) <- ncol(inputpixels)
  ymin(ip.norm) <- 0
  ymax(ip.norm) <- nrow(inputpixels)
  
  data <- inputlines@data
  Lines.ls <- vector("list", 0)
  for (i in 1:length(inputlines)) {
    li <- inputlines[i,]
    ci <- slot(slot(slot(li, "lines")[[1]], "Lines")[[1]], "coords")
    ii <- slot(slot(li, "lines")[[1]], "ID")
    t.ci <- cbind((ci[,1]-xmin)/xres, (ci[,2]-ymin)/yres)
    t.li <- Lines(list(Line(t.ci)), ii)
    Lines.ls[[length(Lines.ls)+1]] <- t.li
  }
  il.norm <- SpatialLinesDataFrame(SpatialLines(Lines.ls), data, match.ID=FALSE)
  
  return(list(ip.norm, il.norm))
}


##### Get all edge attributes of given edges as data frame
getEdgeDF <- function(graph, edges) {
  edge.df <- NULL
  vars <- list.edge.attributes(graph)
  for (v in 1:length(vars)){
    if (v == 1) {
      this.df <- data.frame(get.edge.attribute(graph, name=vars[v], index=edges), stringsAsFactors=FALSE)
    } else {
      this.df <- cbind(this.df, get.edge.attribute(graph, name=vars[v], index=edges))
    }
  }
  names(this.df) <- vars
  return(this.df)
}


graph.SLDF <- function(inputlines, lengthAsWeight=FALSE) {
  # Create a graph from SLDF (every line end in SLDF becomes vertex)
  
  # Store line attributes for later usage
  inputlines.df <- inputlines@data
  
  # Create vertex and edge list from inputlines SLDF
  lid.vec <- as.character(inputlines$lid)
  length.vec <- gLength(inputlines, byid=TRUE)
  
  getStartEnd <- function(x) {
    matrix(c(as.vector((x[[1]])[1,]),as.vector((x[[1]])[nrow((x[[1]])),])), 1,4)
  }
  startend.coordinates.list <-  lapply(coordinates(as.SpatialLines.SLDF(inputlines)), getStartEnd)
  startend.coordinates.df <- data.frame(do.call("rbind", startend.coordinates.list))
  startend.coordinates.df$lid <- lid.vec
  startend.coordinates.df$length <- length.vec
  names(startend.coordinates.df) <- c("x", "y", "x2", "y2", "lid", "length")
  verteces.df <- as.data.frame(unique(rbind(as.matrix(startend.coordinates.df[,1:2]), as.matrix(startend.coordinates.df[,3:4]))))
  names(verteces.df) <- c("x", "y")
  verteces.df$pid <- 1:nrow(verteces.df)
  edge.df <- merge(startend.coordinates.df, verteces.df, by=c("x", "y"), all.x=TRUE, all.y=FALSE)
  names(verteces.df) <- c("x2", "y2", "pid2")
  edge.df <- merge(edge.df, verteces.df, by=c("x2", "y2"), all.x=TRUE, all.y=FALSE)
  names(verteces.df) <- c("x", "y", "pid")
  edge.df <- edge.df[,c("pid", "pid2", "lid", "length")]
  names(edge.df) <- c("vertex1", "vertex2", "lid", "length")
  edge.df$eid <- 1:nrow(edge.df)
  
  # Add line attributes to edge list
  edge.df <- merge(edge.df, inputlines.df, by="lid", all.x=TRUE, all.y=FALSE, sort=FALSE)
  edge.df <- data.frame(vertex1=edge.df$vertex1, vertex2=edge.df$vertex2, edge.df[,!names(edge.df) %in% c("vertex1", "vertex2"), drop=FALSE])
  if (lengthAsWeight) {
    edge.df$weight <- edge.df$length
  }
  
  # Create graph
  graph <- graph.data.frame(edge.df, directed=FALSE)
  
  # Add line coordinates as vertex attributes
  graph.verteces.df <- data.frame(pid=as.numeric(V(graph)$name))
  graph.verteces.df <- merge(graph.verteces.df, verteces.df, by="pid", all=TRUE, sort=FALSE)
  graph <- set.vertex.attribute(graph, name="x", value=graph.verteces.df$x)
  graph <- set.vertex.attribute(graph, name="y", value=graph.verteces.df$y)
  
  return(graph)
}


connectClassified <- function(inputlines.sldf, inputraster=NULL, im.th=10, w=3, buffer=c(100, 250, 500), clustersize=c(0,100,400)) {
  
  # Adjust distances if inputraster is provided
  if (!is.null(inputraster)) {
    buffer <- buffer*res(inputraster)[1]
    clustersize <- clustersize*res(inputraster)[1]
  }
  graphlines <- inputlines.sldf
  
  graphlines$newsp <- graphlines$statepred
  graph <- graph.SLDF(graphlines, lengthAsWeight=TRUE)
  newgraph <- graph
  
  for (j in 1:length(buffer)) {
    d <- buffer[j]
    c <- clustersize[j]
    
    # Get candidate verteces (verteces where a main road stops, but other roads continue)
    main.e <- E(newgraph)[E(newgraph)$newsp == 1]
    main.v <- V(newgraph)[adj(main.e)]
    keep.vec <- rep(FALSE, length(main.v))
    i <- 0
    for (v in main.v){
      i <- i + 1
      spred <- (E(newgraph)[adj(v)])$newsp
      if (sum(spred) == 1 & length(spred) > 1) {
        keep.vec[i] <- TRUE
      }
    }
    candidate.v <- main.v[keep.vec]
    
    # Get candidate vertex dyads (candidate vertex + any vertex on main road)
    candidate.df <- data.frame(x=candidate.v$x, y=candidate.v$y)
    candidate.df$pid <- as.character(candidate.v$name)
    main.df <- data.frame(x2=main.v$x, y2=main.v$y, pid2=main.v$name)
    dyads.df <- merge(main.df, candidate.df)
    dyads.df$dist <- sqrt((dyads.df$x - dyads.df$x2)^2 + (dyads.df$y - dyads.df$y2)^2)
    dyads.df <- dyads.df[dyads.df$dist < d & dyads.df$pid != dyads.df$pid2,]
    dyads.df <- dyads.df[order(dyads.df$dist),]
    
    # Iterate through vertex dyads and connect via closest path
    if (nrow(dyads.df) > 0) {
      for (i in 1:nrow(dyads.df)) {
        pid1 <- dyads.df$pid[i]
        pid2 <- dyads.df$pid2[i]
        v1 <- V(newgraph)[as.character(V(newgraph)$name) == pid1]
        v2 <- V(newgraph)[as.character(V(newgraph)$name) == pid2]
        
        # Calculate length of connected network for each candidate vertex
        maingraph <- newgraph
        maingraph <- delete.edges(maingraph, E(maingraph)[which(E(maingraph)$newsp==0)])
        clusters <- clusters(maingraph)
        cl1.id <- clusters$membership[V(maingraph)$name == pid1]
        cl1.members <- which(clusters$membership == cl1.id)
        cl1.v <- V(maingraph)[cl1.members]
        cl1.e <- E(maingraph)[adj(cl1.v)]
        cl1.length <- sum(cl1.e$weight)
  
        cl2.id <- clusters$membership[V(maingraph)$name == pid2]
        cl2.members <- which(clusters$membership == cl2.id)
        cl2.v <- V(maingraph)[cl2.members]
        cl2.e <- E(maingraph)[adj(cl2.v)]
        cl2.length <- sum(cl2.e$weight)
        
        if (cl1.length > c & cl2.length > c) {
          main.unconnected.dist <- shortest.paths(newgraph, v1, v2, weights=1-E(newgraph)$newsp)  # Are the candidate verteces connected via main roads?
          main.unconnected <- ifelse(main.unconnected.dist > 0, TRUE, 0)
          if (!main.unconnected){  # If there's a main road connection between the verteces: calculate candidate efficiency improvement
            main.dist <- shortest.paths(newgraph, v1, v2, weights=ifelse(E(newgraph)$newsp == 1, E(newgraph)$weight, Inf))
            alt.dist <- shortest.paths(newgraph, v1, v2, weights=E(newgraph)$weight)
            if (alt.dist < Inf) {
              im.ratio <- main.dist/alt.dist
            } else {
              im.ratio <- 0
            }
          } else {
            im.ratio <- Inf
          }
          if (im.ratio > im.th) {
            cand.path <- get.shortest.paths(newgraph, v1, v2, weights=E(newgraph)$weight*((1-E(newgraph)$stateprob)^w), output="epath")
            # Use mixture between state probabilities and distance as weights for calculating shortest paths
            if (length(cand.path$epath) > 0) {
              cand.edges <- E(newgraph)[unlist(cand.path$epath)]
              newgraph <- set.edge.attribute(newgraph, "newsp", index=cand.edges, 1)
            }
          }
        }
      }
    }
  }
  
  # Remove unconnected clusters
  maingraph <- newgraph
  maingraph <- delete.edges(maingraph, E(maingraph)[which(E(maingraph)$newsp==0)])
  clusters <- clusters(maingraph)
  cl.large <- which(clusters$csize > 8)
  cl.large.id <- which(clusters$membership %in% cl.large)
  cl.large.v <- V(maingraph)[cl.large.id]
  cl.large.e <- E(maingraph)[adj(cl.large.v)]
  cl.small.e <- E(maingraph)[!(E(maingraph)$lid %in% cl.large.e$lid)]
  if (length(cl.small.e) > 0) {
    graph.small.e <- E(newgraph)[E(newgraph)$lid %in% cl.small.e$lid]
    newgraph <- set.edge.attribute(newgraph, "newsp", graph.small.e, 0)
  }
  
  # Add new classification to inputlines
  edge.df <- getEdgeDF(newgraph, E(newgraph))
  edge.df <- edge.df[,c("lid", "newsp")]
  edge.df$lid <- as.character(edge.df$lid)
  input.df <- inputlines.sldf@data
  input.df$order <- 1:nrow(input.df)
  input.df <- merge(input.df, edge.df, by="lid", all.x=TRUE, all.y=FALSE)
  input.df <- input.df[order(input.df$order),]
  input.df <- input.df[,!(names(input.df) == "order")]
  outputlines.sldf <- inputlines.sldf
  outputlines.sldf@data <- input.df
  
  return(outputlines.sldf)
}



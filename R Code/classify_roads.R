library(kernlab)
library(data.table)
library(reshape2)
library(compiler)
library(randomForest)
library(igraph)


###### Set all vertex attributes at once
set.vertex.attributes <- function(graph, attr) {
  g2 <- unclass(graph)
  g2[[9]][[3]] <- as.list(attr)
  class(g2) <- "igraph"
  g2
}


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



###### Transform a SLDF into a directed graph where the graph verteces represent the spatial lines
node.graph.SLDF <- function(inputlines) {
  
  # Perform a self join on inputlines do determine neighboring line dyads
  inputlines$nid <- 1:nrow(inputlines)
  ints <- gIntersects(inputlines, inputlines, byid=TRUE)
  rownames(ints) <- inputlines$nid
  colnames(ints) <- inputlines$nid
  m.ints <- melt(ints, varnames=c("nid1", "nid2"), value.name="neighbor")
  m.ints <- m.ints[m.ints$neighbor & m.ints$nid1 != m.ints$nid2,] # Only keep neighboring dyads
  m.ints <- m.ints[,-3]
  
  # Create a directed graph from line dyad list
  graph <- graph.data.frame(m.ints, directed=TRUE)
  
  # Add line centroid coordinates and attributes as vertex attributes
  graph.verteces.df <- data.frame(nid=as.numeric(V(graph)$name))
  
  verteces.df <- inputlines@data
  centroids <- gCentroid(inputlines, byid=TRUE)
  verteces.df$x <- centroids$x
  verteces.df$y <- centroids$y
  
  graph.verteces.df$order <- 1:nrow(graph.verteces.df)
  graph.verteces.df <- merge(graph.verteces.df, verteces.df, by="nid", all.x=TRUE, all.y=FALSE, sort=FALSE)
  graph.verteces.df <- graph.verteces.df[order(graph.verteces.df$order),]
  graph.verteces.df <- graph.verteces.df[,!(names(graph.verteces.df) == "order")]
  
  for(i in 1:ncol(graph.verteces.df)) {
    this.name <- names(graph.verteces.df)[i]
    graph <- set.vertex.attribute(graph, this.name, index=V(graph), graph.verteces.df[,this.name])
  }

  return(graph)
}


###### Get line-pixel overlap information
getLinePixels <- function(con, schemaname, inputlines, inputpixels) {
  
  # Upload inputlines and inputpixels to DB
  rs <- dbWriteSpatial(con, inputlines, schemaname, "inputlines", replace=TRUE)
  pixels.spdf <- rasterToPoints(inputpixels, fun=function (x) x==1, spatial=TRUE)
  names(pixels.spdf) <- "value"
  rs <- dbWriteSpatial(con, pixels.spdf, schemaname, "inputpoints", replace=TRUE)
  
  # Run script creating segmentized lines with line-pixel overlap variable
  linepixels.df <- dbRunScript(con, path="SQL Code/select_line_pixels.sql", return=TRUE, script.param=list(PIXELSIZE = res(inputpixels)[1], SCHEMANAME=schemaname))
  
  # Clean up on DB
  rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".inputlines;", sep=""))
  rs <- dbSendQuery(con, statement=paste("DROP TABLE IF EXISTS ", schemaname, ".inputpoints;", sep=""))
  
  # Return overlap table
  return(linepixels.df)
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


##### Get all vertex attributes of given verteces as data frame
getVertexDF <- function(graph, verteces) {
  vertex.df <- NULL
  vars <- list.vertex.attributes(graph)
  for (v in 1:length(vars)){
    if (v == 1) {
      this.df <- data.frame(get.vertex.attribute(graph, name=vars[v], index=verteces))
    } else {
      this.df <- cbind(this.df, get.vertex.attribute(graph, name=vars[v], index=verteces))
    }
  }
  names(this.df) <- vars
  return(this.df)
}

##### Get ROC cut-off
getCutOff <- function(pred, true, plot=FALSE) {
  roc <- roc(true, pred)
  if (plot) {
    plot(roc)
  }
  th <- coords(roc, x="best", best.method="youden")[1]
  return(th)
}


##### Add edge coviariates to node graph
add.cov.node.graph <- function(con, schemaname, graph, inputlines, other.vars=c("ppl")) {
  # Add covarites for lid1 and lid2
  edges.df <- as.data.frame(get.edgelist(graph))
  names(edges.df) <- c("nid1", "nid2")
  edges.df$order <- 1:nrow(edges.df)
  verteces.df <- getVertexDF(graph, V(graph))
  edges.df <- merge(edges.df, verteces.df, by.x="nid1", by.y="nid", all.x=T, all.y=F)
  names(edges.df)[4:ncol(edges.df)] <- paste(names(verteces.df)[names(verteces.df)!="nid"], "1", sep="")
  nc <- ncol(edges.df)
  edges.df <- merge(edges.df, verteces.df, by.x="nid2", by.y="nid", all.x=T, all.y=F)
  names(edges.df)[(nc+1):ncol(edges.df)] <- paste(names(verteces.df)[names(verteces.df)!="nid"], "2", sep="")

  # Add covariates for other lines intersecting the point where lid1 and lid2 meet
  rs <- dbWriteSpatial(con, inputlines, schemaname, "inputlines", replace=TRUE)
  rs <- dbWriteTable(con, c(schemaname, "edges"), edges.df, row.names=FALSE, overwrite=TRUE)
  otherlines.df <- dbRunScript(con, path="SQL Code/select_other_lines.sql", return=TRUE, script.param=list(SCHEMANAME=schemaname))
  otherjoin.df <- verteces.df[,c("lid", other.vars)]
  otherlines.dt <- data.table(merge(otherlines.df, otherjoin.df, by.x="otherlid", by.y="lid"))
  otherlines.dt[,otherlid:=NULL]
  maxotherlines.df <- data.frame(otherlines.dt[, lapply(.SD, max), by=c("lid1", "lid2")])
  names(maxotherlines.df)[3:ncol(maxotherlines.df)] <- paste("maxother_", names(maxotherlines.df)[3:ncol(maxotherlines.df)], sep="")
  
  # Add everything to graph
  edges.df <- merge(edges.df, maxotherlines.df, by=c("lid1", "lid2"), all.x=TRUE, all.y=FALSE)
  for (i in 1:ncol(edges.df)) {
    if (class(edges.df[,i]) == "factor") {
      edges.df[,i] <- as.character(edges.df[,i])
    }
    if (class(edges.df[,i]) == "numeric") {
      edges.df[is.na(edges.df[,i]),i] <- 0  # Replace NAs with 0s
    }
  }
  graphedges.df <- edges.df[order(edges.df$order),]
  graphedges.df <- graphedges.df[,names(graphedges.df) != "order"]
  for (i in 1:ncol(graphedges.df)) {
    name <- names(graphedges.df)[i]
    graph <- set.edge.attribute(graph, name=name, value=graphedges.df[,name])
  }
  return(graph)
}

##### Create directed neighborhood table
createNeighborhoodTable <- function(graph, inputlines) {
  
  # Get edge information
  edges.df <- getEdgeDF(graph, E(graph))[,c("nid1", "nid2", "lid1", "lid2")]
  edges.dt <- data.table(edges.df)
  setkey(edges.dt, lid1)
  
  # Determine line dyads
  dyads.df <- edges.df[,c("lid1", "lid2", "nid2")] # Dyad info already in graph!
  names(dyads.df) <- c("lid1", "lid3", "nid3")
  dyads.dt <- data.table(dyads.df)
  setkey(dyads.dt, lid1)
  
  # Merge line dyads to edges and only keep rows with extra lines
  merge.dt <- merge(edges.dt, dyads.dt, all.x=TRUE, all.y=FALSE, by="lid1", allow.cartesian=TRUE)
  merge.dt <- merge.dt[!(merge.dt$lid2 == merge.dt$lid3),]
  merge.dt$lid1 <- as.character(merge.dt$lid1)
  merge.dt$lid2 <- as.character(merge.dt$lid2)
  merge.dt$lid3 <- as.character(merge.dt$lid3)
  
  return(merge.dt)
}


##### Loopy belief propagation...
propagate <- function(graph, markov.fit) {
  
  # Estimate markov transition probabilities and add to edges.df
  edges.df <- getEdgeDF(graph, E(graph))
  edges.df$roadtype1 <- 1
  edges.df$p.j1.i1 <- predict(markov.fit, newdata=edges.df, type="probabilities")[,2]
  edges.df$roadtype1 <- 0
  edges.df$p.j1.i0 <- predict(markov.fit, newdata=edges.df, type="probabilities")[,2]

  # Only keep propagation relevant information in edges.df
  edges.df <- edges.df[,c("lid1", "lid2", "nid1", "nid2", "prob1", "p.j1.i0", "p.j1.i1")]
  edges.df$p.j0.i0 <- 1 -  edges.df$p.j1.i0
  edges.df$p.j0.i1 <- 1 - edges.df$p.j1.i1
  edges.df$lid1 <- as.character(edges.df$lid1)
  edges.df$lid2 <- as.character(edges.df$lid2)

  # Get directed neighborhood table (table listing all incoming edges (k->i) for each edge (i->j))
  nbh.dt <- createNeighborhoodTable(graph, inputlines)
  
  # Initialize messages
  edges.df$m.old <- runif(nrow(edges.df))
  edges.df$m.new <- NA
  edges.dt <- data.table(edges.df)
  
  # Repeated propagation
  err.vec <- c()
  best.err <- Inf
  iter <- 0
  best.iter <- 0
  repeat {
    
    # Calculate messages targeting i for each edge i->j
    mold.dt <- copy(edges.dt)
    mold.dt <- subset(mold.dt, select=c("lid1", "lid2", "m.old"))
    setnames(mold.dt, "lid2", "lid3")
    nbh.m.dt <- merge(nbh.dt, mold.dt, by=c("lid1", "lid3"), all.x=TRUE, all.y=FALSE) # Add messages to neighborhood table
    nbh.m.dt$m.old.0 <- 1 - nbh.m.dt$m.old
    nbh.m.dt <- nbh.m.dt[,list(m.old=prod(m.old),m.old.0=prod(m.old.0)),by=list(lid1, lid2)]
    
    # Add neighborhood messages to edge data frame
    this.edges.dt <- copy(edges.dt)
    set(this.edges.dt, j=which((colnames(this.edges.dt) %in% c("m.old", "m.new"))), value=NULL)
    this.edges.dt <- merge(this.edges.dt, nbh.m.dt, by=c("lid1", "lid2"), all.x=TRUE, all.y=FALSE)
    this.edges.dt$m.old[is.na(this.edges.dt$m.old)] <- 1
    this.edges.dt$m.old.0[is.na(this.edges.dt$m.old.0)] <- 1
    
    # Calculate new messages
    this.edges.dt$m.j.1 <- this.edges.dt$p.j1.i1*this.edges.dt$prob1*this.edges.dt$m.old + this.edges.dt$p.j1.i0*(1-this.edges.dt$prob1)*this.edges.dt$m.old.0
    this.edges.dt$m.j.0 <- this.edges.dt$p.j0.i1*this.edges.dt$prob1*this.edges.dt$m.old + this.edges.dt$p.j0.i0*(1-this.edges.dt$prob1)*this.edges.dt$m.old.0
    this.edges.dt$m.j <- this.edges.dt$m.j.1/(this.edges.dt$m.j.1 + this.edges.dt$m.j.0)  # Normalize to prevent over-/underflow
    
    # Add new messages to edge.dt
    set(this.edges.dt, j=which(!(colnames(this.edges.dt) %in% c("lid1","lid2","m.j"))), value=NULL)
    set(edges.dt, j=which((colnames(edges.dt) %in% c("m.new"))), value=NULL)
    edges.dt <- merge(edges.dt, this.edges.dt, by=c("lid1", "lid2"), all.x=TRUE, all.y=FALSE)
    setnames(edges.dt, "m.j", "m.new")
    
    # Calculate sum of absolute error
    err <- sum(abs(edges.dt$m.new - edges.dt$m.old))
    
    # Make new messages the old messages
    edges.dt$m.old <- edges.dt$m.new
    
    # Check for convergence (converged if no global improvement in the last N steps)
    err.vec <- c(err.vec, err)
    iter <- iter + 1
    if (err < best.err) {
      best.err <- err
      best.iter <- 0
    } else {
      best.iter <- best.iter + 1
    }
    if (best.iter > 50 | best.err < 1) {
      break
    }
  }
  print(paste("Converged in ", iter, " steps."))
  flush.console()
  
  # Belief read-out
  vertex.dt <- copy(edges.dt)
  vertex.dt <- subset(vertex.dt, select=c("lid1", "nid1", "prob1"))
  setnames(vertex.dt, old=c("lid1", "nid1", "prob1"), new=c("lid", "nid", "prob"))
  vertex.dt <- unique(vertex.dt)
  messages.dt <- copy(edges.dt)
  messages.dt <- subset(messages.dt, select=c("lid2", "m.new"))
  setnames(messages.dt, old=c("lid2", "m.new"), new=c("lid", "m"))
  vertex.dt <- merge(vertex.dt, messages.dt, by="lid", all.x=TRUE, all.y=FALSE)
  vertex.dt$m.0 <- 1-vertex.dt$m
  vertex.dt <- vertex.dt[,list(m=prod(m),m.0=prod(m.0)),by=list(lid, nid, prob)]
  vertex.dt$p1 <- vertex.dt$prob*vertex.dt$m
  vertex.dt$p0 <- (1-vertex.dt$prob)*vertex.dt$m.0
  vertex.dt$p1 <- vertex.dt$p1/(vertex.dt$p0 + vertex.dt$p1)  # Normalize to unity
  vertex.dt <- subset(vertex.dt, select=c("nid", "p1"))
  
  # Add results to graph
  org.vertex.df <- data.frame(nid=get.vertex.attribute(graph, name="nid"))
  org.vertex.df$order <- 1:nrow(org.vertex.df)
  vertex.df <- data.frame(vertex.dt)
  org.vertex.df <- merge(org.vertex.df, vertex.df, by="nid", all.x=TRUE, all.y=FALSE)
  org.vertex.df <- org.vertex.df[order(org.vertex.df$order),]
  graph <- set.vertex.attribute(graph, name="p1", value=org.vertex.df$p1)
  
  return(graph) 
}


##### Fit models for SVM- and Random-Forest-based Markov-Random-Field Classifier
fitSVMRF <- function(con, schemaname, trainlines.ls, trainpixels.ls, get.best=FALSE) {
  
  inputlines.ls <- vector("list", length(trainlines.ls))
  inputlines.df.ls <- vector("list", length(trainlines.ls))
  for (t in 1:length(trainlines.ls)) {
    
    inputlines <- trainlines.ls[[t]]
    inputpixels <- trainpixels.ls[[t]]
    
    # Normalize coordinates of inputlines and inputpixels
    input.ls <- normalizeCoordinates(inputlines, inputpixels)
    inputpixels <- input.ls[[1]]
    inputlines <- input.ls[[2]]
    
    # Prepare input data
    inputlines$lid <- as.character(inputlines$lid)
    inputlines.df <- inputlines@data
    inputlines.df$order <- 1:nrow(inputlines.df)
    
    # Define outcome variable
    inputlines.df$type <- as.factor(inputlines.df$roadtype)
    
    # Match pixel information on lines
    linepixels.df <- getLinePixels(con, schemaname, inputlines, inputpixels)
    inputlines.df <- merge(inputlines.df, linepixels.df, by="lid")
    inputlines.df <- inputlines.df[order(inputlines.df$order),]
    inputlines.df <- inputlines.df[,!(names(inputlines.df) == "order")]
    inputlines.df <- inputlines.df[,!(names(inputlines.df) %in% c("stateprob", "statepred", "newsp", "state_pred"))]
    inputlines@data <- inputlines.df
    
    inputlines.ls[[t]] <- inputlines
    inputlines.df.ls[[t]] <- inputlines.df
  }
  
  estdata.df <- do.call("rbind", inputlines.df.ls)
  
  # Estimate tuned Random Forest
  if (get.best) {
    test.params <- expand.grid(ntree=c(200, 250), mtry=c(5,6,7))
    error.vec <- c()
    K <- 3
    for (i in 1:nrow(test.params)) {
      ntree <- test.params[i,1]
      mtry <- test.params[i,2]
      blocks <- sample(1:K, nrow(estdata.df), replace=T)
      err.k <- vector("numeric", K)
      for (k in 1:K) {
        train.df <- estdata.df[blocks != k,]
        test.df <- estdata.df[blocks == k,]
        rf.fit <- randomForest(type ~ ppl1 + ppl1_sd + ppl1_min + ppl1_max + ppl2 + ppl2_sd +
                                 ppl2_min + ppl2_max + ppl3 + ppl3_sd + ppl3_min + ppl3_max + length + sectioncount, data = train.df, mtry=mtry, ntree=ntree)
        rf.pred <- predict(rf.fit, newdata=test.df)
        err <- sum(test.df$type != rf.pred)/nrow(test.df)
        err.k[k] <- err
      }
      error.vec <- c(error.vec, mean(err.k))
    }
    best.params <- test.params[which(error.vec == min(error.vec)),]
  } else {
    best.params <- matrix(c(250,5), 1, 2)
  }
  unary.fit <- randomForest(type ~ ppl1 + ppl1_sd + ppl1_min + ppl1_max + ppl2 + ppl2_sd +
                              ppl2_min + ppl2_max + ppl3 + ppl3_sd + ppl3_min + ppl3_max + length + sectioncount, data = estdata.df, mtry=best.params[,2], ntree=best.params[,1])
  
  # Create network from SLDF and add edge covariates
  edge.df.ls <- vector("list", length(inputlines.ls))
  for (i in 1:length(inputlines.ls)) {
    inputlines <- inputlines.ls[[i]]
    graph <- node.graph.SLDF(inputlines)
    graph <- add.cov.node.graph(con, schemaname, graph, inputlines, c("ppl3", "ppl3_max", "ppl3_min", "ppl3_sd", "length"))
    edges.df <- getEdgeDF(graph, E(graph))
    edges.df$type <- as.factor(edges.df$roadtype2)
    edge.df.ls[[i]] <- edges.df
  }
  edge.est.df <- do.call("rbind", edge.df.ls)

  # Estimate markov probabilities with covariates
  markov.fit <- ksvm(type ~ maxother_ppl3 + maxother_ppl3_max + maxother_ppl3_min + ppl31 + ppl3_max1 + ppl3_min1 + roadtype1 + ppl32 + ppl3_max2 + ppl3_min2, data = edge.est.df, prob.model=TRUE)

  return(list(unary.fit, markov.fit))
}


classifyRoads <- function(con, schemaname, SVMRF, inputlines, inputpixels, th=0.5) {
  
  unary.fit <- SVMRF[[1]]
  markov.fit <- SVMRF[[2]]
  
  # Normalize coordinates of inputlines and inputpixels
  origlines <- inputlines
  input.ls <- normalizeCoordinates(inputlines, inputpixels)
  inputpixels <- input.ls[[1]]
  inputlines <- input.ls[[2]]
  
  # Prepare input lines
  inputlines$lid <- as.character(inputlines$lid)
  inputlines.df <- inputlines@data
  inputlines.df$order <- 1:nrow(inputlines.df)

  # Match pixel information on lines
  linepixels.df <- getLinePixels(con, schemaname, inputlines, inputpixels)
  inputlines.df <- merge(inputlines.df, linepixels.df, by="lid")
  inputlines.df <- inputlines.df[order(inputlines.df$order),]
  inputlines.df <- inputlines.df[,!(names(inputlines.df) == "order")]
  inputlines@data <- inputlines.df
  
  # Predict unary probabilities
  inputlines$prob <- predict(unary.fit, newdata=inputlines.df, type="prob")[,2]
  
  # Create network from inputlines SLDF and add edge covariates
  graph <- node.graph.SLDF(inputlines)
  graph <- add.cov.node.graph(con, schemaname, graph, inputlines, c("ppl3", "ppl3_max", "ppl3_min", "ppl3_sd", "length"))
  
  # Perform belief propagation
  bgraph <- propagate(graph, markov.fit)
  
  # Predict from estimated probabilities
  pred.df <- data.frame(lid=V(bgraph)$lid, stateprob=V(bgraph)$p1)
  pred.df$statepred <- ifelse(pred.df$stateprob > th, 1, 0)
  pred.df$lid <- as.character(pred.df$lid)
  
  # Add new predictions to original line
  origlines.df <- origlines@data
  origlines.df$order <- 1:nrow(origlines.df)
  origlines.df <- merge(origlines.df, pred.df, by="lid", all.x=TRUE, all.y=FALSE)
  origlines.df$statepred[is.na(origlines.df$statepred)] <- 0
  origlines.df <- origlines.df[order(origlines.df$order),]
  origlines.df <- origlines.df[,names(origlines.df) != "order"]
  origlines@data <- origlines.df
  
  return(origlines)
}
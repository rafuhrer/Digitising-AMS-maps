library(tripack)
library(data.table)

toCoordinate <- function(i, j, nrow) {
  x <- j + 0.5
  y <- nrow - i + 0.5
  return(cbind(x, y))
}


getPoints <- function(points.spdf, sample.size) {
## Get a list of circles constructed from aribrarily sampled triplets
  
  points.df <- points.spdf@data
  
  rid <- c()
  repeat {
    rid <- c(rid, points.spdf$id[sample(nrow(points.spdf))])
    if (length(rid) > 3*sample.size) {
      break
    }
  }
  rid <- rid[1:(3*sample.size)]
  rid.df <- data.frame(id=rid)
  rid.df <- rid.df[1:(3*(floor(nrow(rid.df)/3))),,drop=FALSE]
  rid.df$tid <- rep(1:(nrow(rid.df)/3), each=3)
  triplets.df <- merge(rid.df, points.df, by="id", all.x=TRUE, all.y=FALSE)
  triplet.list <- split(triplets.df, triplets.df$tid)
  
  # Remove collinear triplets and duplicate triplets
  notCollinear <- function(x) {
    c <- cbind(x$x, x$y)
    lhs <- (c[2,2] - c[1,2])*(c[3,1] - c[2,1])
    rhs <- (c[3,2] - c[2,2])*(c[2,1] - c[1,1])
    idn <- length(unique(x$id))
    return(lhs!=rhs & idn == 3)
  }
  notCo <- unlist(lapply(triplet.list, notCollinear))
  triplet.list <- triplet.list[notCo]
  
  # Circle parameters for each triplet; returs a list of lists of length 2
  getCircle <- function(x) {
    c <- cbind(x$x, x$y)
    cc <- circumcircle(c[,1], c[,2], num.touch=3)
    return(cc)
  }
  circles <- lapply(triplet.list, getCircle)
  
  return(circles)
}



getScores <- function(circles, fgp.df, err) {
# Calculate number of inliners and M-estimation score for each circle
  
  circles.df <- data.frame(do.call("rbind", lapply(circles, function(x) cbind(x$x, x$y, x$radius))))
  names(circles.df) <- c("cx", "cy", "radius")
  circles.df$cid <- 1:nrow(circles.df)
  circles.dt <- data.table(circles.df)
  fgp.dt <- data.table(fgp.df)
  
  setkeyv(circles.dt[,k:=1], c(key(circles.dt), "k"))
  setkeyv(fgp.dt[,k:=1], c(key(fgp.dt), "k"))
  dyads.dt <- merge(circles.dt, fgp.dt, by="k", allow.cartesian=TRUE)
  dyads.dt <- dyads.dt[,k:=NULL]
  
  centerdist <- sqrt((dyads.dt$cx - dyads.dt$x)^2 + (dyads.dt$cy - dyads.dt$y)^2)
  radiusdist <- ifelse(centerdist > dyads.dt$radius, centerdist - dyads.dt$radius, dyads.dt$radius - centerdist)
  dyads.dt$inline <- radiusdist < err
  dyads.dt$score <- radiusdist*dyads.dt$inline + err*(1-dyads.dt$inline)
  #dyads.dt$score <- -(dyads.dt$inline/(2*pi*dyads.dt$radius))
  
  scores.dt <- dyads.dt[,list(score=sum(score), ni=sum(inline)),by="cid"]
  
  return(data.frame(scores.dt))
}



ransac <- function(input.raster, err, epsilon, ni.known=NULL) {
# RANSAC algorithm for finding circles with arbitrary radius
  
  # Create data frame with foreground point coordinates
  input.mat <- as.matrix(input.raster)
  fgp <- which(input.mat == 1, , arr.ind=TRUE)
  fgp <- toCoordinate(fgp[,1], fgp[,2], nrow(input.mat))
  fgp.df <- data.frame(cbind(seq(1:nrow(fgp)), fgp))
  names(fgp.df) <- c("id", "x", "y")
  fgp.df$x <- fgp.df$x + xmin(input.raster)
  fgp.df$y <- fgp.df$y + ymin(input.raster)
  
  # Create SPDF with points
  fgp.spdf <- SpatialPointsDataFrame(fgp.df[,2:3], fgp.df)
  
  N <- nrow(fgp.df)
  q <- 3
  sample.size <- 1000
  
  # Perform RANSAC
  iter <- 0
  best.ni <- NULL
  best.fit <- NULL
  best.circle <- NULL
  repeat {
    # Get candidate circles
    circles <- getPoints(fgp.spdf, sample.size)
    
    # Get scores
    scores.df <- getScores(circles, fgp.df, err)
    
    # Calculate best fit and best number of inliners so far
    scores.df$best.ni <- NA
    scores.df$best.fit <- NA
    if (iter == 0) {
      scores.df$best.ni[1] <- scores.df$ni[1]
      scores.df$best.fit[1] <- scores.df$score[1]
    } else {
      scores.df$best.ni[1] <- best.ni
      scores.df$best.fit[1] <- best.fit
    }
    for(i in 2:nrow(scores.df)) {
      scores.df$best.ni[i] <- ifelse(scores.df$ni[i] > scores.df$best.ni[i-1], scores.df$ni[i], scores.df$best.ni[i-1])
      scores.df$best.fit[i] <- ifelse(scores.df$score[i] < scores.df$best.fit[i-1], scores.df$score[i], scores.df$best.fit[i-1])
    }
    
    # Replace best circle (if it's better than in last round)
    best.ni <- scores.df$best.ni[nrow(scores.df)]
    best.fit <- scores.df$best.fit[nrow(scores.df)]
    if (best.fit %in% scores.df$score) {
      best.id <- (scores.df$cid[best.fit == scores.df$score])[1]
      best.circle <- circles[[best.id]]
    }
    
    # Calculate total expected number of iterations
    q <- 3
    if (is.null(ni.known)) {
      P <- (scores.df$best.ni/N)^q
    } else {
      P <- (ni.known/N)^q
    }
    h <- log(epsilon)/log(1-P)
    
    # Test whether expected number of total iterations is reached already
    iter <- iter + nrow(scores.df)
    #print(c(h[length(h)], iter))
    #flush.console()
    if (h[length(h)] <= iter) {  
      #plot(fgp.spdf)
      #symbols(x=best.circle[1], y=best.circle[2], circles=best.circle$radius, add=TRUE, inches=FALSE, fg="red")
      
      break
    }
  }
  best <- cbind(best.circle$x, best.circle$y, best.circle$radius, best.ni, best.fit)
  return(best)
}


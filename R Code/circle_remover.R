

require(raster)
require(data.table)
require(reshape2)
require(extremevalues)
require(fpc)
require(sp)
library(biOps)


detectCircles <- function(input.img, r, buffer, sensitivity=40) {
# Finds circles of radius r with buffer b in input.img
# Returns data frame with circle-center coordinates and input radius
# Based on Circular Hough Transform Algorithm
	
	##### Create matrix from input image
	input.mat <- matrix(getValues(input.img), nrow(input.img), ncol(input.img), byrow = TRUE)
	
	
	##### Abort if input image does not have foreground points
	if (sum(input.mat) == 0) {
		return(data.frame(x=numeric(), y=numeric(), radius=numeric(), buffer=numeric()))
	}
	
	
	##### Internal Functions
	toCoordinate <- function(i, j, nrow) {
		x <- j - 1
		y <- nrow - i
		return(cbind(x, y))
	}
	
	
	##### Create (a,b) sequences
	a.seq <- seq(from=0, to=ncol(input.mat)-1, by=1)
	b.seq <- seq(from=0, to=nrow(input.mat)-1, by=1)
	
	
	##### *Fast* population of accumulator matrix (Circular Hough Transform Algorithm)
	# Get data frame with coordinates of all foreground points
	fgp <- which(input.mat == 1, arr.ind=TRUE)
	fgp <- toCoordinate(fgp[,1], fgp[,2], nrow(input.mat))
	fgp.df <- data.frame(cbind(seq(1:nrow(fgp)), fgp))
	names(fgp.df) <- c("id", "x", "y")
	fgp.dt <- data.table(fgp.df)
	setkey(fgp.dt, id)
	
	# Create data frame with foreground coordinates and all possible a values, then according b-values
	fgpe.dt <- data.table(data.frame(expand.grid(id=fgp.df$id, a=a.seq)))
	setkey(fgpe.dt, id)
	fgpe.dt <- merge(fgpe.dt, fgp.dt)
	fgpe.dt$id <- seq(1:nrow(fgpe.dt))
	fgpe.dt <- fgpe.dt[r^2 >= (fgpe.dt$x - fgpe.dt$a)^2,]  # Points where r^2 < (x-a)^2 cannot be evidence for a circle with center involving a (too far away!)
	fgpe1.dt <- fgpe.dt  # Duplicated because each (x, y, a) combination may be evindence for two different b!
	fgpe2.dt <- fgpe.dt
	fgpe1.dt$b <- round(fgpe.dt$y - sqrt(r^2 - (fgpe.dt$x - fgpe.dt$a)^2), 0)  # Rearranged parameterized circle equation
	fgpe2.dt$b <- round(fgpe.dt$y + sqrt(r^2 - (fgpe.dt$x - fgpe.dt$a)^2), 0)  # ""
	fgpe.dt <- rbind(fgpe1.dt, fgpe2.dt)
	
	# Count "votes" for each (a,b) pair
	accscore.dt <- fgpe.dt[ , list(count=length(id)), by=list(a, b)]
	
	# Merge accumulator scores to long version of accumulator
	accumulator.dt <- data.table(expand.grid(a.seq, b.seq))
	setnames(accumulator.dt, c("Var1", "Var2"), c("a", "b"))
	accumulator.dt <- merge(accumulator.dt, accscore.dt, all.x=TRUE, by=c("a", "b"))
	accumulator.dt$count[is.na(accumulator.dt$count)] <- 0
	
	# Reshape the long accumulator into a wide accumulator matrix
	accumulator <- acast(data=data.frame(accumulator.dt), formula=b ~ a)
	
	
	##### Visualize accumulator as a simple raster image
	acc.ras <- raster(nrow=nrow(accumulator), ncol=ncol(accumulator), xmn=0, ymn=0,
			xmx = a.seq[length(a.seq)]+1, ymx=b.seq[length(b.seq)]+1)
	values(acc.ras) <- as.vector(t(accumulator))
	#plot(acc.ras)

	
	###### Find center-point candidates using observations greater than sensitivity value
	locmax.val <- as.vector(accumulator[accumulator >= sensitivity])
	if (length(locmax.val) == 0) {
		return(data.frame(x=numeric(), y=numeric(), radius=numeric(), buffer=numeric())) # If no circles are detected
	}
	locmax.def <- matrix(accumulator %in% locmax.val, nrow(accumulator), ncol(accumulator))
	locmax.ij <- which(locmax.def == TRUE, arr.ind=TRUE)  # Get indices of selected local maxima
	locmax.coord <- cbind(locmax.ij[,2] + xmin(input.img), locmax.ij[,1] + ymin(input.img)) - 0.5
	
	
	##### Cluster and average circle-center candidates
	# We use the DBSCAN cluster algorithm to group points that probably originate from the same circle
	# See http://en.wikipedia.org/wiki/DBSCAN
	dbs <- dbscan(data=locmax.coord, eps=2*sqrt(2), MinPts=1)  # Reachability distance is two times diagonal pixel distance
	locmax.cl <- cbind(locmax.coord, dbs$cluster)
	locmax.cl.df <- data.frame(locmax.cl)
	names(locmax.cl.df) <- c("x", "y", "k")
	
	# For each cluster, we use the center of gravity (i.e., "average" point) as circle center
	centerpoints.df <- data.frame(matrix(NA, length(unique(locmax.cl.df$k)), 3)) 
	names(centerpoints.df) <- c("x", "y", "e")
	for (k in 1:length(unique(locmax.cl.df$k))) {
		centerpoints.df[k, 1:2] <- colMeans(locmax.cl.df[locmax.cl.df$k==k, 1:2])
		centerpoints.df[k, 3] <- length(locmax.cl.df$k[locmax.cl.df$k==k])
	}
	
	
	##### Create return data frame with center-point coordinates and radius size
	return.df <- centerpoints.df[,c(1:2)]
	return.df$radius <- r
	return.df$buffer <- buffer
	
	
	##### Return data frame with center points
	return(return.df)
}
	

removeCircles <- function(binary.img, centerpoints.df, buffer) {
# Fills circles (as specified in centerpoints.df) in binary.img
# Returns binary image with filled circles	
	
	##### Fill circle interior in binary image
	if (nrow(centerpoints.df) > 0) {
		circle.ras <- binary.img
		values(circle.ras) <- 0
		all.coords <- data.frame(expand.grid(x=xmin(circle.ras):xmax(circle.ras), y=ymin(circle.ras):ymax(circle.ras)))
		
		for (c in 1:nrow(centerpoints.df)) {
			r <- centerpoints.df$radius[c]
			circle.coords <- all.coords[(all.coords$x-centerpoints.df$x[c])^2 + (all.coords$y - centerpoints.df$y[c])^2 <= (r+buffer)^2, ]
			values(circle.ras)[cellFromXY(circle.ras, circle.coords)] <- 1
		}
		
		cleaned.ras <- binary.img
		values(cleaned.ras) <- ifelse(values(circle.ras) == 1, 1, values(binary.img))
	} else {
		cleaned.ras <- binary.img
	}
	
	##### Return image with filled circles
	return(cleaned.ras)
}

  

cleanCircles <- function(binary.img, color.img, radii=c(10,11,12), buffer=1, sensitivity=20, ni=27, err=0.5, epsilon=0.01, input.folder, output.folder, plot=FALSE) {
# Searches for circles using a circular Hough transform and the RANSAC algorithm
# Fills detected circles with foreground pixels
  
  source("R Code/ransac.R")
  
  ##### Normalize extent and resolution of input images
  input.img <- binary.img
  ymin.org <- ymin(input.img)
  ymax.org <- ymax(input.img)
  xmin.org <- xmin(input.img)
  xmax.org <- xmax(input.img)
  res.org <- res(input.img)
  crs.org <- projection(input.img)
  xmin(color.img) <- ymin(color.img) <- xmin(input.img) <- ymin(input.img) <- 0
  xmax(input.img) <- xmax(color.img) <- ncol(input.img)
  ymax(input.img) <- ymax(color.img) <- nrow(input.img)
  
  ##### Perform Shen-Castan edge-detection on greyscale version of color image
  color.df <- data.frame(getValues(color.img))
  grey <-  0.21*color.df[,1] + 0.72*color.df[,2] + 0.07*color.df[,3]
  binary <- getValues(input.img)
  bingrey <- ifelse(binary==1, grey, 255)
  bingrey.img <- input.img
  bingrey.img <- setValues(bingrey.img, bingrey)
  bingrey.imd <- imagedata(as.matrix(bingrey.img), type="grey")
  binshen.imd <- imgShenCastan(bingrey.imd)  
  binshen.mat <- ifelse(apply(binshen.imd, 2, function(x) x) > 0, 0, 1)
  binshen.img <- input.img
  binshen.img <- setValues(binshen.img, as.vector(t(binshen.mat)))

  grey.imd <- imagedata(as.matrix(setValues(input.img, grey)))
  shen.imd <- imgShenCastan(grey.imd)
  shen.mat <- ifelse(apply(shen.imd, 2, function(x) x) > 0, 0, 1)
  shen.img <- input.img
  shen.img <- setValues(shen.img, as.vector(t(shen.mat)))

  ##### Partition input image & binary shen image 
  source("R Code/partition_images.R")
  partitions.shen <- partitionImage(binshen.img, 500, 30)

  ##### Get shen centerpoints for each partition and radius
  candidatepoints.df <- data.frame(x=numeric(), y=numeric(), radius=numeric(), buffer=numeric(), method=character())
  for (p in 1:length(partitions.shen)) {
    for (r in radii) {
      shenpoints.df <- detectCircles(partitions.shen[[p]], r=r, buffer=buffer, sensitivity=sensitivity)
      candidatepoints.df <- rbind(candidatepoints.df, shenpoints.df)
    }
  }

  ##### Cluster circle-center candidates
  if (nrow(candidatepoints.df) == 0) {
    return(binary.img)
  }
  candcoords <- candidatepoints.df[,1:2]
  dbs <- dbscan(data=candcoords, eps=2*sqrt(2), MinPts=1)  # Reachability distance is two times diagonal pixel distance
  candidatepoints.df$k <- dbs$cluster

  # For each cluster, we use the center of gravity (i.e., "average" point) as circle center
  centerpoints.df <- data.frame(matrix(NA, length(unique(candidatepoints.df$k)), 3)) 
  names(centerpoints.df) <- c("x", "y", "e")
  for (i in 1:nrow(centerpoints.df)) {
    k <- unique(candidatepoints.df$k)[i]
    centerpoints.df[i, 1:2] <- colMeans(candidatepoints.df[candidatepoints.df$k==k, 1:2])
    centerpoints.df[i, 3] <- length(candidatepoints.df$k[candidatepoints.df$k==k])
  }


  ##### Run RANSAC on each candidate and look for actual circles with the full greyscale shen image
  ransac.df <- data.frame(x=numeric(), y=numeric(), radius=numeric(), best.ni=numeric(), best.fit=numeric())
  for (i in 1:nrow(centerpoints.df)) {
    center <- SpatialPoints(centerpoints.df[i,1:2])
    envelope <- gEnvelope(gBuffer(center, width=15))
    cand.raster <- crop(shen.img, envelope)
    ran <- ransac(cand.raster, err=err, epsilon=epsilon, ni.known=ni)
    ransac.df[i,] <- ran
  }

  ##### Filter out false-positives
  ransac.df <- ransac.df[ransac.df$best.ni >= ni & ransac.df$radius < 13 & ransac.df$radius > 9,]
  if (nrow(ransac.df) == 0) {
    return(binary.img)
  }
  
  ##### Remove circles
  result.img <- removeCircles(input.img, ransac.df, buffer)
  
  ##### Plot results
  if (plot) {
    png(paste(output.folder, "/", "circleremover.png", sep=""), height=nrow(input.img)+200, width=ncol(input.img)+200)
    plot(input.img)
    symbols(ransac.df$x, ransac.df$y, circles=ransac.df$radius, add=TRUE, inches=FALSE, fg="red")
    dev.off()
  }
  
  ##### Adjust coordinates
  xmin(result.img) <- xmin.org
  ymin(result.img) <- ymin.org
  xmax(result.img) <- xmax.org
  ymax(result.img) <- ymax.org
  
  return(result.img)
}



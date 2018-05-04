
zsThin <- function(ras, verbose=FALSE) {
## Zhang-Suen thinning algorithm
	
  
  ##### Functions
  
	thin <- function(mat) {
		# Pad matrix and set counter
		m.p <- padMatrix(mat)
		counter = 0	
		# Iterate until convergence
		repeat {
			# Subiteration 1
			m.p <- markDeletion1(m.p)
			nmarked <- length(m.p[m.p == -1])
			# Stop if nothing marked
			if (nmarked == 0) {
				break
			}
			# Delete marked values
			m.p[m.p == -1] <- 0
			
			# Subiteration 2
			m.p <- markDeletion2(m.p)
			nmarked <- length(m.p[m.p == -1])
			# Stop if nothing marked
			if (nmarked == 0) {
				break
			}
			# Delete marked values
			m.p[m.p == -1] <- 0
			counter <- counter + 1
			if (verbose) {
				print(paste("Thinning iteration", counter, ":", nmarked, "pixels deleted."))
				flush.console()
			}
		}
		# Unpad matrix and return thinned result
		mat <- unpadMatrix(m.p)
		if (verbose) {
			print(paste("Thinning converged in",counter,"iteration(s)."))
		}
		return(mat)
	}
	
	# Mark foreground values for deletion, subiteration 1
	markDeletion1 <- function(mat) {
		for (j in 2:(ncol(mat)-1)) {
			for (i in 2:(nrow(mat)-1)) {
				if (mat[i,j] != 0) {
					hood.mat <- getHood(i, j, mat)
					hood.vec <- as.vector(t(hood.mat))
					
					# Foreground value check
					fv.check <- hood.vec[5] != 0
					
					# Connectivity check
					check.c1 <- connectivity(hood.vec) == 1
					
					# Foreground neighbor count check
					nf <- countForeground(hood.vec)
					check.nf <- nf >= 2 & nf <= 6
					
					# Background neighbors check
					check.bg <- (hood.vec[2]*hood.vec[6]*hood.vec[8] == 0) & (hood.vec[4]*hood.vec[6]*hood.vec[8] == 0)
					
					if (check.c1 & check.nf & check.bg & fv.check) {
						mat[i, j] <- -1
					}
				}
			}
		}
		return(mat)
	}
	
	# Mark foreground values for deletion, subiteration 2
	markDeletion2 <- function(mat) {
		for (j in 2:(ncol(mat)-1)) {
			for (i in 2:(nrow(mat)-1)) {
				if (mat[i,j] != 0) {
					hood.mat <- getHood(i, j, mat)
					hood.vec <- as.vector(t(hood.mat))
					
					# Foreground value check
					fv.check <- hood.vec[5] != 0
					
					# Connectivity check
					check.c1 <- connectivity(hood.vec) == 1
					
					# Foreground neighbor count check
					nf <- countForeground(hood.vec)
					check.nf <- nf >= 2 & nf <= 6
					
					# Background neighbors check
					check.bg <- (hood.vec[2]*hood.vec[4]*hood.vec[6] == 0) & (hood.vec[2]*hood.vec[4]*hood.vec[8] == 0)
					
					if (check.c1 & check.nf & check.bg & fv.check) {
						mat[i, j] <- -1
					}
				}
			}
		}
		return(mat)
	}
	
	# Pad a given matrix with 0s
	padMatrix <- function(m) {
		m.pad <- rbind(rep(0, ncol(m)), m, rep(0, ncol(m)))
		m.pad <- cbind(rep(0, nrow(m) + 2), m.pad, rep(0, nrow(m) + 2))
		return(m.pad)
	}
	
	# Unpad a given matrix
	unpadMatrix <- function(m) {
		m.unpad <- m[2:(nrow(m)-1), 2:(ncol(m)-1)]
		return(m.unpad)
	}
	
	# Get Moore neighborhood
	getHood <- function(i, j, m) {
		vec <- c(m[i-1,j-1],m[i-1,j],m[i-1,j+1],m[i,j-1],m[i,j],m[i,j+1],m[i+1,j-1],m[i+1,j],m[i+1,j+1])
		matrix(vec, 3, 3, byrow=TRUE)
	}
	
	# Count foreground cells in neighborhood
	countForeground <- function(inVal) {
		circVal <- c(inVal[1:4],inVal[6:9])
		fg <- length(circVal[circVal!=0])
		return(fg)
	}
	
	# Compute connectivity number of foreground cell
	connectivity <- function(inVal) {  
		n <- c(inVal[6], inVal[3], inVal[2], inVal[1], inVal[4], inVal[7],
				inVal[8], inVal[9], inVal[6])  # Last term added to ensure circular algorithm
		n[n==-1] <- 1  # Marked values are counted as foreground cells
		c <- 0
		for (i in 1:8) {
			c <- c + ifelse((n[i] + 1) * n[i+1] == 1, 1, 0)  # Count number of 0-1 transitions
		}
		return(c)
	}
  
  
  ##### Body
  
	## Normalize extent and resolution of input raster
	ymin.org <- ymin(ras)
	ymax.org <- ymax(ras)
	xmin.org <- xmin(ras)
	xmax.org <- xmax(ras)
	res.org <- res(ras)
	crs.org <- projection(ras)
	xmin(ras) <- ymin(ras) <- 0
	xmax(ras) <- ncol(ras)
	ymax(ras) <- nrow(ras)
  
  ## Perform thinning
	mat <- matrix(values(ras), nrow(ras), ncol(ras), byrow=TRUE)
	mat.th <- thin(mat)
	ras.th <- ras
	values(ras.th) <- as.vector(t(mat.th))
  
  ## Reset correct extent and resolution
  xmin(ras.th) <- xmin.org
  xmax(ras.th) <- xmax.org
  ymin(ras.th) <- ymin.org
  ymax(ras.th) <- ymax.org
  projection(ras.th) <- crs.org
  
	return(ras.th)
}
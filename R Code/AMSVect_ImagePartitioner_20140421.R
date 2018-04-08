

require(raster)

partitionImage <- function(original.img, part.size, overlap.size) {
# Partition image into (overlapping) pixel images
# Proceeds from top to bottom, left to right	

	input.rows <- nrow(original.img)
	input.cols <- ncol(original.img)
	
	rowcounter <- 1
	colcounter <- 1
  
  x.res <- res(original.img)[1]
  y.res <- res(original.img)[2]
  x.min <- xmin(original.img)
	x.max <- xmax(original.img)
	y.min <- ymin(original.img)
	y.max <- ymax(original.img)
  input.crs <- projection(original.img)
	
	partitions <- vector("list", 0)
	repeat {
		this.rows <- rowcounter:(min(rowcounter + part.size - 1, input.rows))
		this.cols <- colcounter:(min(colcounter + part.size - 1, input.cols))
		
		for (l in 1:nlayers(original.img)) {
			this.xmin <- x.min + (colcounter-1)*x.res
      this.ymin <- y.min + (input.rows-max(this.rows))*y.res
      this.xmax <- x.min + max(this.cols)*x.res
      this.ymax <- y.min + (input.rows-rowcounter+1)*y.res
      this.layer <- raster(nrow=length(this.rows), ncol=length(this.cols), 
                           xmn=this.xmin, ymn=this.ymin,
                           xmx=this.xmax, ymx=this.ymax)
			values(this.layer) <- getValuesBlock(original.img[[l]], row=min(this.rows), nrows=length(this.rows), col=min(this.cols), ncols=length(this.cols))
			if (l==1) {
				this.ras <- this.layer
			} else {
				this.ras <- stack(this.ras, this.layer)
			}
		}
    projection(this.ras) <- input.crs
		
		partitions[length(partitions)+1] <- this.ras
		
		if (max(this.rows) < input.rows) {
			rowcounter <- rowcounter + part.size - overlap.size
		} else {
			if (max(this.cols) < input.cols) {
				rowcounter <- 1
				colcounter <- colcounter + part.size - overlap.size
			} else {
				break
			}
		}
	}
	
	return(partitions)	
}



reassembleImage <- function(partitions, original.img, part.size, overlap.size, usemax=TRUE) {
# Reassemble partitioned images into one image
	input.rows <- nrow(original.img)
	input.cols <- ncol(original.img)

	x.res <- res(original.img)[1]
	y.res <- res(original.img)[2]
	x.min <- xmin(original.img)
	x.max <- xmax(original.img)
	y.min <- ymin(original.img)
	y.max <- ymax(original.img)
	input.crs <- projection(original.img)  
	
	for (l in 1:nlayers(original.img)) {
	  rowcounter <- 1
	  colcounter <- 1
	  partitioncounter <- 1
		this.layer <- original.img[[l]]
		repeat {
			this.rows <- rowcounter:(min(rowcounter + part.size - 1, input.rows))
			this.cols <- colcounter:(min(colcounter + part.size - 1, input.cols))
			
			original.values <- values(this.layer)[cellFromRowColCombine(this.layer, this.rows, this.cols)]
			new.values <- values(partitions[[partitioncounter]][[l]])
			
			if (usemax) {
				values(this.layer)[cellFromRowColCombine(this.layer, this.rows, this.cols)] <- ifelse(original.values <= new.values, new.values, original.values)
			} else {
				values(this.layer)[cellFromRowColCombine(this.layer, this.rows, this.cols)] <- ifelse(original.values <= new.values, original.values, new.values)
			}
      
			partitioncounter <- partitioncounter + 1
      
			if (max(this.rows) < input.rows) {
				rowcounter <- rowcounter + part.size - overlap.size
			} else {
				if (max(this.cols) < input.cols) {
					rowcounter <- 1
					colcounter <- colcounter + part.size - overlap.size
				} else {
					break
				}
			}
		}
		if (l==1) {
			output.img <- this.layer
		} else {
			output.img <- stack(output.img, this.layer)
		}
	}

	return(output.img)
}



getPartitionInfo <- function(partitions) {
  
  partitions.df <- data.frame(ID=numeric(), xmin=numeric(), xmax=numeric(), ymin=numeric(), ymax=numeric())
  for (i in 1:length(partitions)) {
    partitions.df <- rbind(partitions.df, c(i, xmin(partitions[[i]]), xmax(partitions[[i]]), ymin(partitions[[i]]), ymax(partitions[[i]])))
  }
  
  names(partitions.df) <- c("ID", "xmin", "xmax", "ymin", "ymax")
  return(partitions.df)	
}


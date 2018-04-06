


processPartition <- function(partition.map, output.folder, partition.name, input.folder) {
	
	##########################
	# I. Initialize
	##########################
	
	###### Load libraries
	library(raster)
	library(sp)
	library(rgdal)
	library(RPostgreSQL)
	library(rgeos)
	
	print(paste("Processing ", output.folder, ".", sep=""))
	flush.console()
	
	##########################
	# II. Extract road pixels
	##########################
  
	###### SVM-based supervised image classification
	source(paste(input.folder, "R Code/AMSVect_Extraction2_20140508.R", sep=""))
	source(paste(input.folder, "R Code/AMSVect_SaveFiles_20130604.R", sep=""))
	
	# Train the model (if it's not already trained)
	if (!exists("gauss.ml.fit")) {
	  if (file.exists(paste(input.folder, "Input/PixelClassification_TrainingSet/gauss.ml.fit.Rdata", sep=""))) {
	    load("Input/PixelClassification_TrainingSet/gauss.ml.fit.Rdata")
	  } else {
	    # Get classified images
	    training.noroad.img <- stack(paste(input.folder, "/Input/PixelClassification_TrainingSet/1301-Mixed-TrainingSet-NoRoads.png", sep=""))
	    training.road.class.img <- stack(paste(input.folder, "/Input/PixelClassification_TrainingSet/1301-Advanced-TrainingSet-Roads-Classified.png", sep=""))
	    training.road.orig.img <- stack(paste(input.folder, "/Input/PixelClassification_TrainingSet/1301-Advanced-TrainingSet-Roads-Original.png", sep=""))
	    # Train
	    gauss.ml.fit <- learnClassifier(training.noroad.img, training.road.class.img, training.road.orig.img)
	    save(gauss.ml.fit, file=paste(input.folder, "Input/PixelClassification_TrainingSet/gauss.ml.fit.Rdata", sep=""))
	  }
	}
  
	filepath <- paste(output.folder, "/red.asc", sep="")
	if (file.exists(filepath)) {
	  red.svm <- raster(filepath)
	} else {
	  # Classify map
	  red.svm <- classifyPixels(partition.map, gauss.ml.fit)
	  # Save output
	  saveFile(red.svm, "red", output.folder)
	}

	print("Extracted road pixels.")
	flush.console()
	
  # Stop processing if almost no red pixels
  if(sum(values(red.svm)) < 15) {
    fileConn<-file(paste(output.folder, "/noroads.txt", sep=""))
    writeLines(c("No Roads!"), fileConn)
    close(fileConn)
    return(TRUE)
  }
  
  
	##########################
	# III. Clean pixel image
	##########################
	
	###### Remove road-number circles from pixel image
	source(paste(input.folder, "R Code/AMSVect_CircleRemover_20140509.R", sep=""))
	
	filepath <- paste(output.folder, "/nocircle.asc", sep="")
	if (file.exists(filepath)) {
	  map.nocircle <- raster(filepath)
	} else {  
    # Remove circles
    map.nocircle <- cleanCircles(red.svm, partition.map, input.folder=input.folder, output.folder=output.folder, plot=FALSE)
	  
    # Save output
    saveFile(map.nocircle, "nocircle", output.folder)
	}
	print("Removed circles.")
	flush.console()
	
  
  
	##########################
	# IV. Thin pixel image
	##########################
	
	###### Thinning
	source(paste(input.folder, "R Code/AMSVect_zhThin_20140422.R", sep=""))
	
	filepath <- paste(output.folder, "/thinned.asc", sep="")
	if (file.exists(filepath)) {
	  map.thinned <- raster(filepath)
	} else {
	  # Thin image
    map.thinned <- zsThin(map.nocircle, verbose=FALSE)
	  
    # Save output
    saveFile(map.thinned, "thinned", output.folder)
	}
    
	print("Thinned pixel image.")
	flush.console()
	
  
  
	##########################
	# V. Vectorize pixels to lines
	##########################
	
	###### Load PostGIS DB handler functions and prepare schema
	source(paste(input.folder, "R Code/AMSVect_PGHandler_20140425.R", sep=""))
	#con <- getPGConn("postgis", "5432", "localhost", "postgres", "A380-900")
  con <- getPGConn("growup", "5432", "cederman.ethz.ch", "admin", "hNo7Yoo")
	schemaname <- tolower(gsub("-", "_", partition.name))
	dbCreateSchema(con, schemaname)
  
	###### Tracing
	source(paste(input.folder, "R Code/AMSVect_MergeLines_20140424.R", sep=""))
	source(paste(input.folder, "R Code/AMSVect_NetworkTools_20140423.R", sep=""))
	source(paste(input.folder, "R Code/AMSVect_trace_20140427.R", sep=""))
  
	filepath <- paste(output.folder, "/traced.shp", sep="")
	if (file.exists(filepath)) {
	  lines.sldf <- readOGR(output.folder, "traced")
	} else {
    # Trace lines
	  lines.sldf <- traceLines(con, schemaname, map.thinned)
    
    # If tracing yields no lines
    if (is.null(lines.sldf)) {
      fileConn <- file(paste(output.folder, "/noroads.txt", sep=""))
      writeLines(c("No Roads!"), fileConn)
      close(fileConn)
      return(TRUE)
    }
    
    # Save output
	  saveFile(lines.sldf, "traced", output.folder)
	}
	print("Vectorized pixel image.")
	flush.console()
	
  
	##########################
	# VI. Standardize and smooth lines
	##########################
  
	###### Standardize coordinates
	source(paste(input.folder, "R Code/AMSVect_cleanLines_20130514.R", sep=""))
	source(paste(input.folder, "R Code/AMSVect_BreakNodes2_20130529.R", sep=""))
	source(paste(input.folder, "R Code/AMSVect_MergeLines_20140424.R", sep=""))
	lines.sldf <- standardizeGeom(con, schemaname, lines.sldf)
  
  ##### Smooth (and remove zero-length lines)
	smoothlines.sl <- gSimplify(lines.sldf, tol=0.005)
	smoothlines.sldf <- SpatialLinesDataFrame(smoothlines.sl, lines.sldf@data, match.ID=FALSE)
	smoothlines.sldf <- smoothlines.sldf[!(gLength(smoothlines.sldf, byid=T) == 0),]
  
	# Stop processing if (almost) no lines left
	if(nrow(smoothlines.sldf) <= 1) {
	  fileConn<-file(paste(output.folder, "/noroads.txt", sep=""))
	  writeLines(c("No Roads!"), fileConn)
	  close(fileConn)
	  return(TRUE)
	}
  
	###### Break and merge
	breaklines2.sldf <- breakNodes(con, schemaname, smoothlines.sldf, verbose=FALSE)
	breaklines2.sldf <- removeDuplicates(con, schemaname, breaklines2.sldf)
	cleanlines.sldf <- mergeLinesDB(con, schemaname, breaklines2.sldf, verbose=FALSE)
  
	saveFile(cleanlines.sldf, "cleanlines", output.folder)
  
	print("Cleaned line geometries.")
	flush.console()
  
  
	##########################
	# VIII. Snap lines and delete unnecessary lines
	##########################
	
	###### Snap lines (and another round of cleaning)
	source(paste(input.folder, "R Code/AMSVect_NetworkTools_20140423.R", sep=""))
	
	snaplines.sldf <- snapNetwork(con, schemaname, cleanlines.sldf, buffersize=15*res(red.svm)[1], breakfirst=TRUE, snapAnywhere=FALSE)
	
	snaplines.sldf <- breakNodes(con, schemaname, snaplines.sldf, verbose=FALSE)
	snaplines.sldf <- mergeLinesDB(con, schemaname, snaplines.sldf, verbose=FALSE)
  
	snaplines2.sldf <- snapNetwork(con, schemaname, snaplines.sldf, buffersize=75*res(red.svm)[1], breakfirst=TRUE, snapAnywhere=TRUE)
  
	snaplines2.sldf <- breakNodes(con, schemaname, snaplines2.sldf, verbose=FALSE)
	snaplines2.sldf <- mergeLinesDB(con, schemaname, snaplines2.sldf, verbose=FALSE)
	snaplines2.sldf <- deleteShortLines(con, schemaname, snaplines2.sldf, 20*res(red.svm)[1])
	
	# Stop processing if (almost) no lines left
	if(nrow(snaplines2.sldf) == 0) {
	  fileConn<-file(paste(output.folder, "/noroads.txt", sep=""))
	  writeLines(c("No Roads!"), fileConn)
	  close(fileConn)
	  return(TRUE)
	}
  
  sslines.sldf <- mergeLines(snaplines2.sldf, verbose=TRUE)
	saveFile(sslines.sldf, "sslines", output.folder)
	
	print("Snapped line geometries.")
	flush.console()
  
  dbDropSchema(con, schemaname)
  rv <- dbDisconnect(con)
  
  return(TRUE)
}



makeNetwork <- function(inputlines.sldf, inputraster, output.folder, input.folder, map.name, OS="Win", cores=4) {
  
  source(paste(input.folder, "R Code/AMSVect_SaveFiles_20130604.R", sep=""))
  
  ###### Load PostGIS DB handler functions and prepare schema
  source(paste(input.folder, "R Code/AMSVect_PGHandler_20140425.R", sep=""))
  #con <- getPGConn("postgis", "5432", "localhost", "postgres", "A380-900")
  con <- getPGConn("growup", "5432", "cederman.ethz.ch", "admin", "hNo7Yoo")
  schemaname <- tolower(gsub("-", "_", map.name))
  dbCreateSchema(con, schemaname)
  
  
	##########################
	# IX. Snap lines and delete unnecessary lines (again)
	##########################
	
	###### Snap lines (and another round of cleaning)
	source(paste(input.folder, "R Code/AMSVect_NetworkTools_20140423.R", sep=""))
  source(paste(input.folder, "R Code/AMSVect_BreakNodes2_20130529.R", sep=""))
  source(paste(input.folder, "R Code/AMSVect_MergeLines_20140424.R", sep=""))
  source(paste(input.folder, "R Code/AMSVect_cleanLines_20130514.R", sep=""))
  
	snaplines.sldf <- snapNetwork(con, schemaname, inputlines.sldf, buffersize=100*res(inputraster)[1], breakfirst=FALSE, snapAnywhere=TRUE, partitionName="partition", parallel=TRUE, OS=OS, cores=cores)
  saveFile(snaplines.sldf, "snaplines", output.folder)
  
  sslines.sldf <- breakNodes(con, schemaname, snaplines.sldf, verbose=FALSE)
  sslines.sldf <- mergeLinesDB(con, schemaname, sslines.sldf, verbose=FALSE)
  saveFile(sslines.sldf, "sslines", output.folder)
  
  ###### Delete short lines connecting otherwise connected nodes
	simplified.sldf <- cleanNetwork(con, schemaname, sslines.sldf, 10*res(inputraster)[1])
  saveFile(simplified.sldf, "simplified", output.folder)
  
	print("Snapped and simplified line geometries.")
	flush.console()

  
  
	##########################
	# IX. Classify primary roads
	##########################
	
	###### Classify lines with SVMRF classifier
	source(paste(input.folder, "R Code/AMSVect_ClassifyRoads3_20140505.R", sep=""))
  source(paste(input.folder, "R Code/AMSVect_ClassifyConnector_20140601.R", sep=""))
  

	# Train classifier (if it isn't already trained)
	if (!exists("rClassifier")) {
	  if (file.exists(paste(input.folder, "Input/rClassifier.Rdata", sep=""))) {
	    load(paste(input.folder, "Input/rClassifier.Rdata", sep=""))
	  } else {
	    nc28.trainlines <- readOGR(paste(input.folder, "Input/RoadTypeClassification_TrainingSet/1301-NC28", sep=""), "classified")
	    nc28.trainpixels <- raster(paste(input.folder, "Input/RoadTypeClassification_TrainingSet/1301-NC28/red.svm.asc", sep=""))
	    nd32.trainlines <- readOGR(paste(input.folder, "Input/RoadTypeClassification_TrainingSet/1301-ND32-5-ed", sep=""), "classified")
	    nd32.trainpixels <- raster(paste(input.folder, "Input/RoadTypeClassification_TrainingSet/1301-ND32-5-ed/red.svm.asc", sep=""))
	    nc32.trainlines <- readOGR(paste(input.folder, "Input/RoadTypeClassification_TrainingSet/1301-NC32-2-ed", sep=""), "classified")
	    nc32.trainpixels <- raster(paste(input.folder, "Input/RoadTypeClassification_TrainingSet/1301-NC32-2-ed/red.svm.asc", sep=""))
	    ng42.trainlines <- readOGR(paste(input.folder, "Input/RoadTypeClassification_TrainingSet/1301-NG42-7-ed", sep=""), "classified")
	    ng42.trainpixels <- raster(paste(input.folder, "Input/RoadTypeClassification_TrainingSet/1301-NG42-7-ed/red.svm.asc", sep=""))
	    nb47.trainlines <- readOGR(paste(input.folder, "Input/RoadTypeClassification_TrainingSet/1301-NB47-6-ed", sep=""), "classified")
	    nb47.trainpixels <- raster(paste(input.folder, "Input/RoadTypeClassification_TrainingSet/1301-NB47-6-ed/red.svm.asc", sep=""))
      trainlines.ls <- list(nd32.trainlines, nc28.trainlines, nc32.trainlines, ng42.trainlines, nb47.trainlines)
      trainpixels.ls <- list(nd32.trainpixels, nc28.trainpixels, nc32.trainpixels, ng42.trainpixels, nb47.trainpixels)
      rClassifier <- fitSVMRF(con, schemaname, trainlines.ls, trainpixels.ls)
      save("rClassifier", file=paste(input.folder, "Input/rClassifier.Rdata", sep=""))
	  }
	}
	# Classify lines
  classified.sldf <- classifyRoads(con, schemaname, rClassifier, inputlines=simplified.sldf, inputpixels=map.raster, th=0.0005)
  
	saveFile(classified.sldf, "classified", output.folder)
	
  # Connect classified lines
  connected.sldf <- connectClassified(classified.sldf, map.raster, im.th=10, w=3, buffer=c(250), clustersize=c(0))  
  
  # Save image of classified lines
	jpeg(filename = paste(output.folder, "/", "classified.jpg", sep=""), width = ncol(inputraster), height = nrow(inputraster), units = "px", pointsize = 12, quality = 100)
	plot(inputraster)
	plot(connected.sldf[connected.sldf$newsp == 0,], col="blue", lwd=1, add=TRUE)
	if (any(connected.sldf$newsp==1)) {
    plot(connected.sldf[connected.sldf$newsp == 1,], col="red", lwd=2, add=TRUE)
	}
	dev.off()
	
	print("Classified road types.")
	flush.console()


    
  ##########################
  # X. Finalize and save map
  ##########################  
  
  final.sldf <- connected.sldf
  projection(final.sldf) <- projection(inputraster)
	saveFile(final.sldf, "final", output.folder)

  dbDropSchema(con, schemaname)
  rv <- dbDisconnect(con)
}

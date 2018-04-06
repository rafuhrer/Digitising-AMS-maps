

processPartition <- function(partition.map, foldername) {
	
	##########################
	# I. Initialize
	##########################
	
	###### Load libraries
	library(raster)
	library(sp)
	library(rgdal)
	library(RPostgreSQL)
	library(rgeos)
	
	print(paste("Processing ", foldername, ".", sep=""))
	flush.console()
	
	##########################
	# II. Extract road pixels
	##########################
	
	###### SVM-based supervised image classification
	source("R Code/AMSVect_Extraction_20130221.R")
	source("R Code/AMSVect_SaveFiles_20130604.R")
	# Get classified images
	training.noroad.img <- stack("Input/PixelClassification_TrainingSet/1301-Mixed-TrainingSet-NoRoads.png")
	training.road.img <- stack("Input/PixelClassification_TrainingSet/1301-Mixed-TrainingSet-Roads.png")
	# Train the model (if it's not already trained)
	if (!exists("svm.fit")) {
		svm.fit <- learnClassifier(training.road.img, training.noroad.img, TRUE)
	}
	# Classify map
	red.svm <- classify(svm.fit, partition.map)
	# Save output
	saveFile(red.svm, "red", foldername)
	
	print("Extracted road pixels.")
	flush.console()
	
	##########################
	# III. Clean pixel image
	##########################
	
	###### Remove road-number circles from pixel image
	source("R Code/AMSVect_CircleRemover_20130325.R")
	map.nocircle <- removeCirclesLM(input.img=red.svm, r=11, buffer=3, sensitivity=30, color.img=partition.map, img.output=TRUE, img.path=foldername)
	
	print("Removed circles.")
	flush.console()
	
	##########################
	# IV. Thin pixel image
	##########################
	
	###### Thinning
	source("R Code/AMSVect_zhThin_20130217.R")
	map.thinned <- zsThin(map.nocircle, verbose=FALSE)
	saveFile(map.thinned, "thinned", foldername)
	
	print("Thinned pixel image.")
	flush.console()
	
	##########################
	# V. Vectorize pixels to lines
	##########################
	
	###### Tracing
	source("R Code/AMSVect_trace_20130218.R")
	lines.traced <- traceLines(map.thinned, verbose=FALSE)
	
	###### Create SpatialLineDataFrame with new ID variable
	lines.sldf <-  SpatialLinesDataFrame(lines.traced, data.frame(lid=cbind(c(1:length(lines.traced)))), match.ID=FALSE)
	
	print("Vectorized pixel image.")
	flush.console()
	
	##########################
	# VI. Break, merge, and clean line gemoetries
	##########################
	
	###### Load PostGIS DB handler functions
	source("R Code/AMSVect_PGHandler_20130220.R")
	con <- getPGConn("postgis", "5432", "localhost", "postgres", "FA18Hornet")
	
	###### Fix line geometries (eliminate points)
	source("R Code/AMSVect_cleanLines_20130514.R")
	lines.sldf <- deletePoints(lines.sldf)
	
	###### Break nodes and remove duplicates
	source("R Code/AMSVect_BreakNodes2_20130529.R")
	breaklines.sldf <- breakNodes(con, lines.sldf, verbose=FALSE)
	breaklines.sldf <- removeDuplicates(con, breaklines.sldf)
	
	###### Merge connected lines
	source("R Code/AMSVect_MergeLines_20130221.R")
	mergelines.sldf <- mergeLines(con, breaklines.sldf, verbose=FALSE)
	
	###### Remove very short line segments and standardize coordinates
	cleanlines.sldf <- deleteShortLines(con, mergelines.sldf, 3)
	cleanlines.sldf <- standardizeGeom(con, cleanlines.sldf)
	
	print("Cleaned line geometries.")
	flush.console()
	
	##########################
	# VII. Smooth line geometries
	##########################
	
	##### Smoothing (and yet another round of cleaning)
	smoothlines.sl <- gSimplify(cleanlines.sldf, tol=2)
	smoothlines.sldf <- SpatialLinesDataFrame(smoothlines.sl, cleanlines.sldf@data, match.ID=FALSE)
	smoothlines.sldf <- removeDuplicates(con, smoothlines.sldf)
	
	print("Smoothed line geometries.")
	flush.console()
	
	##########################
	# VIII. Snap lines
	##########################
	
	###### Snap lines (and another round of cleaning)
	source("R Code/AMSVect_SnapNetwork_20130611.R")
	snaplines.sldf <- snapNetwork(con, smoothlines.sldf, buffersize=30)
	snaplines.sldf <- mergeLines(con, snaplines.sldf, verbose=FALSE)
	snaplines.sldf <- breakNodes(con, snaplines.sldf, verbose=FALSE)
	snaplines.sldf <- deleteShortLines(con, snaplines.sldf, 20)
	saveFile(snaplines.sldf, "snaplines", foldername)
	
	print("Snapped line geometries.")
	flush.console()
	
	##########################
	# IX. Classify primary roads
	##########################
	
	###### Classify lines with SIKH classifier
	source("R Code/AMSVect_ClassifyRoads2_20130606.R")
	# Load classified maps (if they aren't already loaded)
	if (!exists("NB32.training.sldf")) {
		NB32.training.sldf <- readOGR("Input/1301-NB32-Douala_MiniSample-HeadsUp", "classlines")
		NB32.training.pixels <- raster("Input/1301-NB32-Douala_MiniSample-HeadsUp/red.asc")
		NB47.training.sldf <- readOGR("Input/1301-NB47-Penang_Sample-HeadsUp", "classlines")
		NB47.training.pixels <- raster("Input/1301-NB47-Penang_Sample-HeadsUp/red.asc")
		SB47.training.sldf <- readOGR("Input/1301-SB47-Surabaja_Sample-HeadsUp", "classlines")
		SB47.training.pixels <- raster("Input/1301-SB47-Surabaja_Sample-HeadsUp/red.asc")
	}
	# Train classifier (if it isn't already trained)
	if (!exists("rClassifier")) {
		rClassifier <- trainRoadClassifier(con, list(NB47.training.sldf, SB47.training.sldf), 
				list(NB47.training.pixels, SB47.training.pixels))
	}
	# Classify lines
	classified.sldf <- classifyRoads(con, rClassifier, snaplines.sldf, red.svm)
	saveFile(classified.sldf, "classified", foldername)
	# Save image of classified lines
	jpeg(filename = paste(foldername, "/", "classified.jpg", sep=""), width = ncol(partition.map), height = nrow(partition.map), units = "px", pointsize = 12, quality = 100)
	plotRGB(partition.map)
	plot(classified.sldf[classified.sldf$state.pred == 0,], col="blue", lwd=1, add=TRUE)
	plot(classified.sldf[classified.sldf$state.pred == 1,], col="green", lwd=2, add=TRUE)
	dev.off()
	
	print("Classified road types.")
	flush.console()
}

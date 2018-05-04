saveFile <- function(file, filename, foldername) {
# SAVE SPATIAL OBJECTS
#
# Author:
# Philipp Hunziker, hunziker@icr.gess.ethz.ch
#
# Last Edited:
# 04/06/2013
#
# Decription:
# Saves Raster and SpatialDataFrame objects to specified location.
	
	# Create appropriate directory (if it doesn't already exist)
	dir <- file.path(foldername)
	dir.create(dir, showWarnings=FALSE, recursive=TRUE)

	# Save rasters/SDFs appropriately
	saved <- FALSE
	filetype <- tolower(class(file))
	if (grepl("raster", filetype)) {
		if (dim(file)[3] > 1) {
			wr <- writeRaster(file, paste(dir, "/", filename, ".grd", sep=""), overwrite=TRUE)
		} else {
			wr <- writeRaster(file, paste(dir, "/", filename, ".asc", sep=""), overwrite=TRUE)
		}
		saved <- TRUE
	}
	if (grepl("spatial", filetype)) {
    writeOGR(file, dir, filename, driver="ESRI Shapefile", overwrite_layer=TRUE)
		saved <- TRUE
	}
	if (!saved){
		print("File not saved: File type not of type 'Raster' or 'SpatialDataFrame'.")
	}
}

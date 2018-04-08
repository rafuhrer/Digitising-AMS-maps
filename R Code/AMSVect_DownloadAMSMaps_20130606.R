library(XML)
library(stringr)

# URL of Map Index Page
url <- "http://www.lib.utexas.edu/maps/imw/"

# Target directory
target.dir <- "C:/Users/spadmin/PhD/Projects/HTND/AMS Road Vectorization/Maps"

# Get all links from given page
doc <- htmlParse(url)
links <- xpathSApply(doc, "//a/@href")

# Get links pointing to JPEG
link.endings <- sapply(str_replace_all(links, "[/]", ""), function(x) str_split(x, "[.]")[[1]][length(str_split(x, "[.]")[[1]])])
jpg.links <- links[link.endings=="jpg" | link.endings=="jpeg"]

# Get map names
map.names.full <- sapply(jpg.links, function(x) str_replace(str_extract(x, "n[[:alpha:]]-[[:graph:]]{1,}"), "[.]jpg", ""))
map.names.index <- vector("character", length(map.names.full))
map.names.edition <- vector("character", length(map.names.full))
for (m in 1:length(map.names.full)) {
	fullname <- map.names.full[m]
	extract <- str_extract(fullname, "-2nd[[:print:]]{0,}|-3rd[[:print:]]{0,}|-[[:digit:]]th[[:print:]]{0,}|-1st[[:print:]]{0,}")
	if (is.na(extract)) {
		name.index <- fullname
		name.edition <- NA
	} else {
		name.index <- str_replace(fullname, extract, "")
		name.edition <- str_extract(extract, "[[:digit:]][[:print:]]{2}")
	}
	map.names.index[m] <- name.index
	map.names.edition[m] <- name.edition
}

# Only keep links for which we could identify a map name
jpg.links <- jpg.links[!is.na(map.names.full)]
map.names.full <- map.names[!is.na(map.names.full)]
map.names.index <- map.names[!is.na(map.names.full)]
map.names.edition <- map.names[!is.na(map.names.full)]

# Download maps
for (m in 1:length(jpg.links)) {
	target.path <- file.path(target.dir, paste(map.names[m], ".jpg", sep=""))
	source.url <- jpg.links[m]
	if (!file.exists(target.path)) {
		download.file(source.url, target.path, quiet=FALSE, mode = "wb")
	}
}

# Create a map index table
maps.df <- data.frame(fullname=map.names.full, mapindex=map.names.index, mapedition=map.names.edition, link=jpg.links)
write.csv(maps.df, file.path(target.dir, "Index/map_index.csv"))
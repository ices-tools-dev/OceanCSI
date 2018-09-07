# ipak function ----------------------------------------------------------------
# install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "data.table", "dplyr", "rworldmap", "xts", "Kendall", "ggplot2", "rlist")
ipak(packages)

# Input files ------------------------------------------------------------------

searegionFile <- "Input/EEA_SeaRegion_20180831.shp"

# Read shapefile
searegion <- sf::st_read(searegionFile)

# create bounding box for plotting on European scale
bboxEurope <- st_bbox(searegion)

# create bounding boxes for the regions
bboxRegions = data.frame(  ID = as.integer(),  xmin = as.numeric(),  xmax = as.numeric(),  ymin = as.numeric(),  ymax = as.numeric())
for(ii in seq(1:length(colnames(searegion)))){
  temp = data.frame(ID = ii, xmin = st_bbox(searegion[ii,])[1], xmax = st_bbox(searegion[ii,])[2], ymin = st_bbox(searegion[ii,])[3], ymax =  st_bbox(searegion[ii,])[4]
  )
  bboxRegions = rbind(bboxRegions, temp)
}

# free memory
rm(searegion)

data("countriesLow")
world <- fortify(countriesLow) 

# function for plotting status per parameter
plotStatusMaps <- function(bboxEurope, data, xlong, ylat, parameterValue, Year, invJet = TRUE, limits) {
  
# create color scales for plotting, depending on whether good status is associated with high or low values
  if(!invJet){
    # normal jet scale
    colorscale <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))## use the jet colormap
  }
  if(invJet){
    # inverse jet colorscale
    colorscale <-
      colorRampPalette(rev(c("#00007F", "blue", "#007FFF", "cyan",
                             "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")))## use the inverse jet colormap
  }

# geographical limits  
  xxlim = c(bboxEurope[1], bboxEurope[3])
  yylim = c(bboxEurope[2], bboxEurope[4])
 
# limits for color scale 
  if(limits[1] == "auto")limits <- c(range(as.data.frame(data)[,parameterValue]))
  
  ggplot(data, mapping = aes_string(xlong, ylat)) +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "darkgrey", color = "black") +
    geom_point(shape = 21, aes_string(fill = parameterValue), color = "white", size = 2) +
    coord_quickmap(xlim = xxlim, ylim = yylim) +
    ggtitle(paste("Status of ", parameterValue, "2013 - 2017")) +
    scale_fill_gradientn(colours  = colorscale(7), guide = "colourbar", limits = limits) +
    theme_bw() + 
    theme(
      text = element_text(size = 15),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.position = "right",
      axis.line = element_blank(),
      axis.ticks = element_blank())
}


# idea to make wrapper around save 
saveEuropeMap <- function(parameter, width = 10, height = 8) {
  ggsave(filename = file.path("output", paste0(parameter, "_status", ".png")),
         height = height, width = width)
}




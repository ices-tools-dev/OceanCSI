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
searegions <- sf::st_read(searegionFile)

# create bounding box for plotting on European scale
bboxEurope <- st_bbox(searegions)

# create bounding boxes for the regions
bboxRegions = data.frame(  ID = as.integer(),  xmin = as.numeric(),  xmax = as.numeric(),  ymin = as.numeric(),  ymax = as.numeric())
for(ii in seq(1:length(rownames(searegions)))){
  temp = data.frame(ID = searegions$ID[ii], name = searegions$SubRegion[ii], xmin = st_bbox(searegions[ii,])[1], xmax = st_bbox(searegions[ii,])[2], ymin = st_bbox(searegions[ii,])[3], ymax =  st_bbox(searegions[ii,])[4], stringsAsFactors = F
  )
  bboxRegions = rbind(bboxRegions, temp)
}
rownames(bboxRegions) <- NULL
bboxRegions$name <- as.character(bboxRegions$name )


data("countriesLow")
world <- fortify(countriesLow) 

# function for plotting status per parameter
plotStatusMaps <- function(bboxEurope, data, xlong, ylat, parameterValue, Year, invJet = TRUE, limits) {

  # create color scales for plotting, depending on whether good status is associated with high or low values
  if(!invJet){
    # normal jet scale
    colorscale <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))## use the jet colormap
    setorderv(data, parameterValue, 1)
    statusplot <-  ggplot(data, mapping = aes_string(xlong, ylat))
  }
  if(invJet){
    # inverse jet colorscale
    colorscale <-
      colorRampPalette(rev(c("#00007F", "blue", "#007FFF", "cyan",
                             "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")))## use the inverse jet colormap
    setorderv(data, parameterValue, -1)
    statusplot <-  ggplot(data, mapping = aes_string(xlong, ylat))
  }
  
  # geographical limits  
  xxlim = c(bboxEurope[1], bboxEurope[3])
  yylim = c(bboxEurope[2], bboxEurope[4])
  
  # limits for color scale 
  if(limits[1] == "auto")limits <- c(range(as.data.frame(data)[,parameterValue]))
  
  statusplot + geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "darkgrey", color = "black") +
    geom_point(shape = 21, aes_string(fill = parameterValue), color = "white", size = 2) +
    coord_quickmap(xlim = xxlim, ylim = yylim) +
    ggtitle(paste("Status of", parameterValue, assessmentYear - 5 + 1, "-", assessmentYear)) +
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

# Around ggsave for saving status parameter plots 
saveEuropeStatusMap <- function(parameter, width = 10, height = 8) {
  ggsave(filename = file.path("output", paste0(parameter, "_status", ".png")),
         height = height, width = width)
}

plotKendallClasses <- function(plotdata, parameterValue){
  
  # define color scale for trendplotting
  cols <- c("increasing" = "red", "no trend" = "grey", "decreasing" = "green")
  if(parameterValue == "Oxygen") cols <- c("decreasing" = "red", "no trend" = "grey", "increasing" = "green")
  
  # geographical limits  
  xxlim = c(bboxEurope[1], bboxEurope[3])
  yylim = c(bboxEurope[2], bboxEurope[4])
  
  if(parameterValue != "Oxygen") setorderv(plotdata, "trend", 1)
  if(parameterValue == "Oxygen") setorderv(plotdata, "trend", 1)
  
  ggplot() +
    geom_polygon(data = world, aes(long, lat, group = group), fill = "darkgrey", color = "black") +
    geom_point(data = plotdata, aes(AvgLongitude, AvgLatitude, fill = trend, group = ClusterID), shape = 21, color = "white", size = 1.7) +
    scale_fill_manual(values = cols) +
    coord_quickmap(xlim = xxlim, ylim = yylim) +
    #ggtitle(paste("Trends in", parameterValue, prettyClassNames, "1980 -", assessmentYear)) +
    ggtitle(paste("Trends in", parameterValue, "1980 -", assessmentYear)) +
    theme_bw() + 
    theme(
      text = element_text(size = 15),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.position = "right",
      axis.line = element_blank(),
      axis.ticks = element_blank())
}

# Around ggsave for saving status parameter plots 
saveEuropeTrendMap <- function(parameter, width = 10, height = 8) {
  ggsave(filename = file.path("output", paste0(parameter, "_trend", ".png")),
         height = height, width = width)
}



# function for plotting status per parameter
plotRegionStatusMaps <- function(bboxEurope, data, xlong, ylat, parameterValue, Year, invJet = TRUE, limits, region = NULL) {
  
  plotdata <- as.data.frame(data)[data$SeaRegionID==region,]
  
  # create color scales for plotting, depending on whether good status is associated with high or low values
  if(!invJet){
    # normal jet scale
    colorscale <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))## use the jet colormap
    setorderv(plotdata, parameterValue, 1)
    statusplot <-  ggplot(plotdata, mapping = aes_string(xlong, ylat))
  }
  if(invJet){
    # inverse jet colorscale
    colorscale <-
      colorRampPalette(rev(c("#00007F", "blue", "#007FFF", "cyan",
                             "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")))## use the inverse jet colormap
    setorderv(plotdata, parameterValue, -1)
    statusplot <-  ggplot(plotdata, mapping = aes_string(xlong, ylat))
  }
  
  # geographical limits  
  if(is.null(region)){
    xxlim = c(bboxEurope[1], bboxEurope[3])
    yylim = c(bboxEurope[2], bboxEurope[4])
  }else{
    xxlim = as.numeric(c(bboxRegions[bboxRegions$ID==region,]$xmin, bboxRegions[bboxRegions$ID==region,]$xmax))
    yylim = as.numeric(c(bboxRegions[bboxRegions$ID==region,]$ymin, bboxRegions[bboxRegions$ID==region,]$ymax))
  }  
  
  
  # limits for color scale 
  if(limits[1] == "auto")limits <- c(range(as.data.frame(plotdata)[,parameterValue]))
  
    statusplot + geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "darkgrey", color = "black") +
      geom_point(shape = 21, aes_string(fill = parameterValue), color = "white", size = 3) +
      coord_quickmap(xlim = xxlim, ylim = yylim) +
      ggtitle(paste("Status of ", parameterValue, assessmentYear - 5 + 1, "-", assessmentYear)) +
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

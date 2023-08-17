# Hierarchical clustering
data <- Stations[which(Stations$SeaRegionID==2), c("UTM_E", "UTM_N"),]
data <- sf::st_set_geometry(data, NULL)
dist <- dist(data)
fit <- hclust(as.dist(dist), method = "complete")
plot(fit)
clusters <- cutree(fit, h = 1000)
plot(data, col = clusters)

data1 <- Stations[which(Stations$SeaRegionID==2), c("Longitude", "Latitude"),]
dist <- sf::st_distance(data1)
fit <- hclust(as.dist(dist), method = "complete")
clusters <- cutree(fit, h = 1000)
plot(data1, col = clusters)

install.packages("geosphere")
library(geosphere)
data2 <- Stations[which(Stations$SeaRegionID==2), c("Longitude", "Latitude"),]
data2 <- sf::st_set_geometry(data2, NULL)
mdist <- distm(data2)
fit <- hclust(as.dist(mdist), method = "complete")
clusters <- cutree(fit, h = 1000)
plot(data2, col = clusters)
data2$Clusters <- cutree(fit, h = 1000)
table(data2$Clusters)

dist <- sf::st_distance(Stations)
fit <- hclust(as.dist(dist), method = "complete")
Stations$clusters <- cutree(fit, h = 1000)
plot(Stations, col = clusters)

# Test --------------------------------------------------------------------

points_in_distance <- function(in_pts, maxdist, ncuts = 10) {
  require(data.table)
  require(sf)
  # convert points to data.table and create a unique identifier
  pts <-  data.table(in_pts)
  pts <- pts[, or_id := 1:dim(in_pts)[1]]
  
  # divide the extent in quadrants in ncuts*ncuts quadrants and assign each
  # point to a quadrant, then create the index over "x" to speed-up
  # the subsetting
  range_x  <- range(pts$x)
  limits_x <-(range_x[1] + (0:ncuts)*(range_x[2] - range_x[1])/ncuts)
  range_y  <- range(pts$y)
  limits_y <- range_y[1] + (0:ncuts)*(range_y[2] - range_y[1])/ncuts
  pts[, `:=`(xcut =  as.integer(cut(x, ncuts, labels = 1:ncuts)),
             ycut = as.integer(cut(y, ncuts, labels = 1:ncuts)))]  %>%
    setkey(x)
  
  results <- list()
  count <- 0
  # start cycling over quadrants
  for (cutx in seq_len(ncuts)) {
    
    # get the points included in a x-slice extended by `maxdist`, and build
    # an index over y to speed-up subsetting in the inner cycle
    min_x_comp    <- ifelse(cutx == 1,
                            limits_x[cutx],
                            (limits_x[cutx] - maxdist))
    max_x_comp    <- ifelse(cutx == ncuts,
                            limits_x[cutx + 1],
                            (limits_x[cutx + 1] + maxdist))
    subpts_x <- pts[x >= min_x_comp & x < max_x_comp] %>%
      setkey(y)
    
    for (cuty in seq_len(ncuts)) {
      count <- count + 1
      
      # subset over subpts_x to find the final set of points needed for the
      # comparisons
      min_y_comp  <- ifelse(cuty == 1,
                            limits_y[cuty],
                            (limits_x[cuty] - maxdist))
      max_y_comp  <- ifelse(cuty == ncuts,
                            limits_x[cuty + 1],
                            (limits_x[cuty + 1] + maxdist))
      subpts_comp <- subpts_x[y >= min_y_comp & y < max_y_comp]
      
      # subset over subpts_comp to get the points included in a x/y chunk,
      # which "neighbours" we want to find. Then buffer them by maxdist.
      subpts_buf <- subpts_comp[ycut == cuty & xcut == cutx] %>%
        sf::st_as_sf() %>% 
        sf::st_buffer(maxdist)
      
      # retransform to sf since data.tables lost the geometric attrributes
      subpts_comp <- sf::st_as_sf(subpts_comp)
      
      # compute the intersection and save results in a element of "results".
      # For each point, save its "or_id" and the "or_ids" of the points within "dist"
      inters <- sf::st_intersects(subpts_buf, subpts_comp)
      
      # save results
      results[[count]] <- data.table(
        id = subpts_buf$or_id,
        int_ids = lapply(inters, FUN = function(x) subpts_comp$or_id[x]))
    }
  }
  data.table::rbindlist(results)
}


pts <- data.frame(x = runif(20000, 0, 100000), y = runif(20000, 0, 100000), id = 1:20000) %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE)

maxdist <- 2000

out <- points_in_distance(pts, maxdist = maxdist, ncut = 10)

Atlantic <- fread("Input/Atlantic_time_series_EEA.txt", 
                  sep = "\t", na.strings = "NULL", 
                  stringsAsFactors = FALSE, header = TRUE, skip = "Cruise\t")


table2start <- which(names(Atlantic) == "time_ISO8601")

table1cols <- 1:(table2start-1)
table2cols <- table2start:ncol(Atlantic)

table1rows <- which(Atlantic$Cruise != "")

Atlantic1 <- Atlantic[table1rows, ..table1cols]
Atlantic2 <- Atlantic[, ..table2cols]

table1key <- 1:nrow(Atlantic1)

nrecords <- c(table1rows[-1] - 1, nrow(Atlantic)) - table1rows + 1
Atlantic2$ID <- rep(table1key, nrecords)
Atlantic1$ID <- table1key
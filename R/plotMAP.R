#' Plotting trips from a central-placve foraging species
#'
#' This function was developped for giving a quick visual on your trips. Land basemap and bathymetry is also 
#' render automatically when calling the function
#'
#' @param df data frame containing all the trips. For now, passing only CPFMove object
#' @param pmap logical. Should every plot be plotted in your active session? Depending of the number of single trip you have, this might generate a lot of plots
#' @param path path indicating where the plots should be saved
#' @param w width of the plot. Unit to select in the *units* argument. Passing to ggsave() function
#' @param h height of the plot. Unit to select in the *units* argument. Passing to ggsave() function
#' @param units unit to consider for plotting e.g. c("mm", "cm"). Passing to ggsave() function
#' @param ColLonga vector indicating the longitude of the colony and have the same length as your data frame. If nothing is passed to that
#' argument, the function will automatically take the first longitude of the trip as the colony's longitude
#' @param ColLat vector indicating the latitude of the colony and have the same length as your data frame. If nothing is passed to that
#' argument, the function will automatically take the first longitude of the trip as the colony's latitude
#' @details This function has been designed orignally to facilitate the visual inspection of various filtering parameters on the actual trips. Howerver, 
#' we plan to make this function more general as soon as possible. This function use mainly the plotting function from
#' the package [[ggOceanMaps]](https://mikkovihtakari.github.io/ggOceanMaps/)
#' @return This function returns a number equal as the number of unique trips in your dataset. If the argument **pmap** is *TRUE*
#' the plots will also be returned in R
#' @keywords trips movement central-place-forager cpf delineating colony movement-ecology
#' @examples
#'\dontrun{
#' path <- c("C:/Users/philip/Desktop/test")
#' test <- plotMAP(f, pmap = F, path = path, ColLong = f$ColLong, ColLat = f$ColLat)
#' }
#' @export



plotMAP <- function(allbirds, pmap = FALSE, path = NULL, w = 12, h = 12,
                units = "cm", ColLong = NULL, ColLat = NULL) {
  
  if (is.null(path))
    stop("path? You need to indicate where to save the figures")
  
  pack <- c("ggOceanMaps", "ggplot2", "ggspatial")
  
  if (length(setdiff(pack, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(pack, rownames(installed.packages())))  
  }
  
  sapply(pack, function(p) {require(p, quietly=T, character.only = T)})
  
  if(class(allbirds) == "list") {allbirds <- do.call(rbind, allbirds)}
  
  for(i in 1:length(unique(allbirds$birdTrip))){
    
    bt <- unique(allbirds$birdTrip)[i]
    
    bird <- subset(allbirds, allbirds$birdTrip == bt)
    
   if (is.null(ColLong) | is.null(ColLat)) {
    warning("You have no coordinate associated to your colony. Location will be automatically picked up from your dataset")
    
    ColonyPos <- subset(bird, bird$ColonyorTrip == "colony")
     
    m <- basemap(limits = c(min(bird$Longitude), max(bird$Longitude), 
          min(bird$Latitude), max(bird$Latitude)), bathymetry = TRUE, 
          glaciers = TRUE, bathy.style = "poly_greys") +
        geom_spatial_path(data = bird, aes(x = Longitude, y = Latitude), color = "light gray") +
        geom_spatial_point(data = bird, aes(x = Longitude, y = Latitude), color = "purple") +
        geom_spatial_point(data = ColonyPos[1, ], aes(x = Longitude, y = Latitude), color = "blue")
    
    } else {
      
    m <- basemap(limits = c(min(bird$Longitude), max(bird$Longitude), 
          min(bird$Latitude), max(bird$Latitude)), bathymetry = TRUE, 
          glaciers = TRUE, bathy.style = "poly_greys") +
        geom_spatial_path(data = bird, aes(x = Longitude, y = Latitude), color = "light gray") +
        geom_spatial_point(data = bird, aes(x = Longitude, y = Latitude), color = "purple") +
        geom_spatial_point(data = bird, aes(x = ColLong[1], y = ColLat[1]), color = "blue")
      
    }
    
    g <- paste0('MAP_', bt, '.png')
    ggsave(m, filename = g, bg = "transparent", path = path, width = w, height = h, units = units) 
    
    ColonyPos <- NULL
    
  } 
  
  if(pmap == TRUE){return(m + ggtitle(g))}
  
}

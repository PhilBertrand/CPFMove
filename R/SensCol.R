#' Sensitivity of colony radius size on trip delineation
#'
#' This function was developped to give basic visualization tools to facilitate the selection
#' of the colony radius size and the effect of this variation on the number of trip generated
#'
#' @param pathF path leading to your tracks
#' @param pathM path leading to your metadata file
#' @param metname a character string corresponding to the metadata file's name
#' @param nbn character. Vector of "names" matching your raw GPS files (i.e. which are linked by the pathF argument). See details for more details
#' @param timezone time zone in which data are recorded
#' @param iter the number of bootstrapping iterations (default = 50)
#' @param param vector of radius sizes to be tested. These correspond to the different scenarios to be tested. Radii are here in km format
#' @param gpst a character string corresponding to the metadata's column name containing the GPS type
#' @param ddep a character string corresponding to the metadata's column name containing the date of departure
#' @param drecap a character string corresponding to the metadata's column name containing the date of retrieval
#' @param colony a character string corresponding to the metadata's column name containing individual colony attribution
#' @param year a character string corresponding to the metadata's column name containing the year of sampling
#' @param ring a character string corresponding to the metadata's column name containing the individual ring number
#' @param tdep a character string corresponding to the metadata's column name containing the time of departure
#' @param trecap a character string corresponding to the metadata's column name containing the time of retrieval
#' @param speedTresh numeric treshold used as speed cutoff for speed filtering (km/h)
#' @param FIX a character string corresponding to the metadata's column name containing the numeric interval separating two
#' locations in the GPS track
#' @param Interpolate logical; if TRUE, tracks are interpolated
#' @param FixInt numeric interval that should separate two locations in GPS tracks for interpolation (minutes; e.g. 2, 10)
#' @param sorc numeric, proportion (0:1) treshold used for the second-order-rate of change
#' @details Raw GPS data (located via *pathF*) should be as .csv format. This version only include 4 types of GPS format 1) Catlog, 2)
#' IGotU, 3) PathTracks and 4) Ecotone. If your file doesn't have any specific format, the *gpst* could be specified as IGotU (one line header,
#' and the raw data in the second lines. However, one should make sure that the *Latitude*, *Longitude*, *Date* and *Time* are specified.
#' *Date* could be supplied as character, as YYYY-MM-DD or MM/DD/YYYY and *Time* also as character as HH:MM:SS). Raw GPS data are expected to be in lat long coordinate system
#' Distance between points are calculated via the Great Circle distance, computed via *sp* package
#' @return a list of object containing a data frame witht the main results of the sensitivity analysis and
#' the associated figures. Figures are also showing the "second-order-rate of change" for each scenario. For the
#' moment, the SORC is fixed to the 5% absolute variaiton of the total magnitude of the response (i.e. max(average(nTrip))).
#' This is seen as a measure of "stability " of the response *t* among scenario *t-1* and *t+1*.
#' @keywords trips movement central-place-forager cpf delineating colony movement-ecology
#' @examples
#' \dontrun{
#' pathF <- c("C:/Users/philip/OneDrive/NP_Kontrakt/StromTracks/files/plomvi/")
#' pathM <- c("C:/Users/philip/OneDrive/NP_Kontrakt/StromTracks/metadata/")
#' metname <- c("meta_bjorn.csv")
#' timezone <- c("GMT")
#' param <- seq(0, 1, by = 0.1)
#'
#' sc <- sensCol(pathF, pathM, metname, nbn = c("year", "colony", "ring", "recapture"),
#'          timezone, iter = 300, param = param, speedTresh = 90,
#'          gpst = "GPSType", ddep = "deployment", drecap = "recapture", colony = "colony",
#'          year = "year", ring = "ring", FIX = "FIX", tdep = "utc_deployment", trecap = "utc_retrieval",
#'          Clong = "Clongitude", Clat = "Clatitude", FixInt = 2, Interpolate = T, sorc=0.01)
#'          }
#' @references
#' - Freitas, C., Lydersen, C., Ims, R.A., Fedak, M.A. and Kovacs, K.M. (2008) A simple new algorithm to filter marine mammal Argos locations Marine Mammal Science 24:315-325.
#' - McConnell, B.J., Chambers, C. and Fedak, M.A. (1992) Foraging ecology of southern elephant seals in relation to the bathymetry and productivity of the Southern Ocean. Antarctic Science 4:393-398.
#' @export
#'
sensCol <- function(pathF = ..., pathM = ..., nbn = c("NULL"), iter = 50,
                 metname = NULL, param = NULL, gpst = NULL, FIX = NULL, ddep = NULL, drecap = NULL,
                 colony = NULL, year = NULL, ring = NULL, tdep = NULL, trecap = NULL, timezone = NULL,
                 Clong = NULL, Clat = NULL, speedTresh = NULL, FixInt = NULL, Interpolate = FALSE, sorc=0.05) {

  if (class(metname) != "character")
    stop("metname should be a character")
  if (class(nbn) != "character")
    stop("nbn should be a (vector of) character")
  if (!is.vector(param))
    stop("param should be a vector")
  if (class(gpst) != "character")
    stop("gpst should be a character")
  if (class(ddep) != "character")
    stop("ddep should be a character")
  if (class(drecap) != "character")
    stop("drecap should be a character")
  if (class(colony) != "character")
    stop("colony should be a character")
  if (class(year) != "character")
    stop("year should be a character")
  if (class(ring) != "character")
    stop("ring should be a character")
  if (class(tdep) != "character")
    stop("tdep should be a character")
  if (class(trecap) != "character")
    stop("trecap should be a character")
  if (class(timezone) != "character")
    stop("the timezone should be a character")
  if (class(FIX) != "character")
    stop("the FIX should be a character")
  if (class(Clong) != "character")
    stop("the Clong should be a character")
  if (class(Clat) != "character")
    stop("the Clat should be a character")
  if (!is.null(speedTresh) & class(speedTresh) != "numeric")
    stop("the speed treshold should be numeric")
  if (!is.null(FixInt) & class(FixInt) != "numeric")
    stop("the time interval between successive fixes should be numeric")
  if (!is.null(FixInt) & class(FixInt) != "numeric")
    stop("the time interval between successive fixes should be numeric")
  if (class(sorc) != "numeric" | c(sorc < 0 | sorc > 1))
    stop("sorc treshold needs a proportion as treshold; 0-1")

  ## Importing file's names
  Filext <- ".csv"
  file.name <- list.files(pathF)[grep(Filext,list.files(pathF))]; rm(Filext)
  files   <- length(file.name)

  ## Importing metadata; need to remove path to pathM, no permission is denied
  metafile <- read.csv(paste(pathM, metname, sep=""), sep = ",", header=T, na.strings=c("","NA"))

  ## define from when the tracks should begin and end: when logger was deployed/retrieved
  metafile$start <- as.POSIXct(strptime(paste (as.character(metafile[[ddep]]),
                                               as.character(metafile[[tdep]])),"%Y-%m-%d %H:%M", tz=timezone), timezone)
  metafile$end   <- as.POSIXct(strptime(paste (as.character(metafile[[drecap]]),
                                               as.character(metafile[[trecap]])),"%Y-%m-%d %H:%M", tz=timezone), timezone)

  ## create a track ID synonymous with how the files are named:
  dfn <- list()
  for (m in 1:length(unique(nbn))){
    tmpname <- nbn[m]
    dfn[[m]] <- paste(metafile[[tmpname]])
  }

  metafile$ID <- do.call(paste, c(as.data.frame(do.call(cbind, dfn))[, c(1:ncol(as.data.frame(do.call(cbind, dfn))))], sep="_"))

  Date.diff <-  metafile$end - metafile$start; if( length(metafile$ID[which(Date.diff <= 0)]) != 0 )
    stop('Date of retrieval should be later than date of deployment')

  scol.list <- list()
  arS <- list()

for (i in 1:length(param)) {

  p <- param[i]

   for (f in 1:iter) {

   r <- sample(1:files, 1)

   ## Partitionning between two types of GPS-based file
   ## Need to have a column specifying GPS type in the metadata file
   GPSType <- metafile[, gpst][which(metafile$ID == gsub(".csv", "", file.name[r]))]
   if(identical(GPSType, character(0))) {stop("You need to have a valid GPS type. Referring to gpst")}

   ## This line remove the 6 first unecassary lines in the CHIP-PATCH GPS Type
   ## The 1GEN is read as it is
   if(GPSType != "Catlog" & GPSType != "IGotU" & GPSType != "Pathtrack" & GPSType != "Ecotone") {stop("the GPS type should be either Catlog, IGotU, Ecotone or Pathtrack")}
   if(GPSType == "Catlog") { bird <- read.csv(paste(pathF, file.name[r], sep=""), sep = ",", skip = 6) }
   if(GPSType == "IGotU"){ bird <- read.csv(paste(pathF, file.name[r], sep=""), sep = ",") }
   if(GPSType == "Ecotone"){ bird <- read.csv(paste(pathF, file.name[r], sep=""), sep = ",")
      bird$Date <- as.Date(with(bird, paste(Year, Month, Day, sep="/")), "%Y/%m/%d")
      bird$Time <- sprintf("%s:%s:%s", bird$Hour, bird$Minute, bird$Second)
      bird <- subset(bird, !is.na(bird$Latitude) & !is.na(bird$Longitude))}
   if(GPSType == "Pathtrack"){
     bird <- read.csv(paste(pathF, file.name[r], sep=""), sep = ",", skip =5)
     colnames(bird) <- c("Day", "Month", "Year", "Hour", "Minute", "Second", "SecondDay",
                         "NumberSats", "Latitude", "Longitude", "Altitude", "ClockOff",
                         "Accuracy", "BatLevel")
     bird$Year <- bird$Year + 2000
     bird$Date <- as.Date(with(bird, paste(Year, Month, Day, sep="/")), "%Y/%m/%d")
     bird$Time <- sprintf("%s:%s:%s", bird$Hour, bird$Minute, bird$Second)
     bird <- subset(bird, bird$Latitude > 0 & bird$Longitude > 0)}

   ## add track ID
   bird$trackID <- unlist(strsplit(file.name[r], "[.]"))[1]

   # extract start and end times for each file
   start <- metafile$start[which(metafile$ID == gsub(".csv", "", file.name[r]))]
   end <- metafile$end[which(metafile$ID == gsub(".csv", "", file.name[r]))]
   int <- lubridate::interval(start, end, tzone = timezone)

   ## Extract colony corrdinates and building of a data.frame
   ## Will be used later in the script for colony-location distance
   CLong <- metafile[[Clong]][which(metafile$ID == gsub(".csv", "", file.name[r]))]
   CLat <- metafile[[Clat]][which(metafile$ID == gsub(".csv", "", file.name[r]))]
   CLL <- cbind(CLong, CLat)

   ## make sure date in the right format - two different conversion methods cos multiple date formats
   mdy <- lubridate::mdy(bird$Date, quiet = TRUE)
   ymd <- lubridate::ymd(bird$Date, quiet = TRUE)
   mdy[is.na(mdy)] <- ymd[!is.na(ymd)] # give the working one precedence
   bird$Date <- format(mdy, "%Y/%m/%d")
   bird$Time <- chron::chron(times=bird$Time)
   bird$datetime <- strptime(paste(gsub("/", "-", bird$Date), bird$Time), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
   bird <- bird[order(bird$datetime , decreasing = FALSE ),] ## assuring the chronological order of fixes

   ## now subset to points within this time start/end window
   bird <- bird[bird$datetime >= start & bird$datetime <= end, ]

   ## overwrite device speed data, in m/s
   # trip.matrix <- data.matrix(bird[,c("Longitude","Latitude")], rownames.force = NA) #creates two column matrix of lat and long for trip trackDistance function
   # between.point.distances <- trip::trackDistance(trip.matrix, longlat = TRUE) #calculate distance in km between each GPS point, into vector
   # bird$PointDist <- c(0, between.point.distances) #dist in km
   # bird$TimeElapsed <- 0 #create empty column to fill with for loop
   #
   # for (k in 2:NROW(bird)){
   #   bird$TimeElapsed[k] <- difftime(bird$datetime[k], bird$datetime[k-1], units = "secs") #time diff in secs
   # }
   #
   # bird$Speed <- (bird$PointDist * 1000)/bird$TimeElapsed ## m/s
   #
   # if (!is.null(speedTresh)) {
   #   bird <- subset(bird, bird$Speed < speedTresh)
   # }

   ## Extracting GPS interval, determined from the metadata
   Sres <- (metafile[, FIX][which(metafile$ID == gsub(".csv","",file.name[r]))])*60
   Mres <- (metafile[, FIX][which(metafile$ID == gsub(".csv","",file.name[r]))])

   if(Interpolate == TRUE) {

     refda <- min(bird$datetime)
     Nbird <- adehabitatLT::as.ltraj(xy = data.frame(bird$Longitude, bird$Latitude), date = as.POSIXct(bird$datetime), id = bird$trackID)
     Nbird[[1]]$sf <- suppressWarnings(trip::sda(x=trip::as.trip(Nbird), smax= speedTresh))
     Nbird <- adehabitatLT::ld(Nbird)
     Nbird <- subset(Nbird, Nbird$sf == TRUE)
     Nbird <- adehabitatLT::as.ltraj(xy = data.frame(Nbird$x, Nbird$y), date = as.POSIXct(Nbird$date), id = Nbird$id)

     wost_NA <- adehabitatLT::setNA(Nbird,refda,Mres,units="min")
     wost_demo <- adehabitatLT::sett0(wost_NA,refda,Mres,units="min")
     Nbird <- adehabitatLT::redisltraj(na.omit(wost_demo), u = Sres, type = "time")
     Nbird[[1]]$nbNA <- ifelse(Nbird[[1]]$x %in% wost_demo[[1]]$x, 0, 1)

     tmpN <- adehabitatLT::ld(Nbird)
     Nbird <- adehabitatLT::as.ltraj(xy = data.frame(tmpN$x, tmpN$y), date = as.POSIXct(tmpN$date), id = tmpN$id, infolocs = tmpN[, c(11, 12)])

     if(!is.null(FixInt)) {
       if(nrow(Nbird[[1]]) > 1){ ### Standardized as matter of time treshold
         if(Mres != FixInt) {
           if( FixInt < max(metafile$FIX, na.rm = TRUE)  ) stop("your selected treshold for ", print(bird$trackID[1]), " is smaller than the greater interval in your sampled tracks - meaning that your are creating points")
           Nbird <- adehabitatLT::subsample(Nbird, FixInt, units = c("min"))
         }
       }
     }

     Nbird <- adehabitatLT::ld(Nbird)
     bird <- data.frame(datetime = Nbird$date, Latitude = Nbird$y, Longitude = Nbird$x, trackID = Nbird$id, nbNA = Nbird$nbNA)

   } else { bird <- bird[, c("datetime", "Latitude", "Longitude", "trackID")]; bird$nbNA <- 0 }

   ## get distance flown from the colony
   trip.matrix <- data.matrix(bird[,c("Longitude","Latitude")], rownames.force = NA) #creates two column matrix of lat and long for trip trackDistance function
   bird$ColonyDist <- sp::spDistsN1(trip.matrix, CLL, longlat = TRUE) #calculate distances between GPS points and initial GPS point in km
   between.point.distances <- trip::trackDistance(trip.matrix, longlat = TRUE) #calculate distance in km between each GPS point, into vector
   bird$PointDist <- c(0, between.point.distances) #dist in km

     ## Setting buffer treshold
     bird$ColonyorTrip <- ifelse(bird$ColonyDist > p, "trip","colony") #in km

     ## set all colony points to 0
     bird$ColonyDist[which(bird$ColonyorTrip == "colony")] <- 0

     ## add trip number
     tripID <- 1
     nRows <- length(bird$ColonyorTrip) - 1
     bird$tripID <- 0

     ### this big for loop numbers all trips, including the one colony point before
     ### and after the last trip point
     if(bird$ColonyorTrip[1] == "colony" && bird$ColonyorTrip[2] == "trip") {bird$tripID[1] <- tripID}  else {bird$tripID[1] <- 0}
     if(bird$ColonyorTrip[1] == "trip") {bird$tripID[1] <- tripID}

     for (j in 2:nRows) {
       if(bird$ColonyorTrip[j] == "colony" && bird$ColonyorTrip[j] != bird$ColonyorTrip[j+1] &&
          bird$ColonyorTrip[j-1] != bird$ColonyorTrip[j+1]) {
         tripID <- tripID + 1
       } else {bird$tripID[j] <- tripID}
       if(bird$ColonyorTrip[j] == "trip" | bird$ColonyorTrip[j] != bird$ColonyorTrip[j-1]
          | bird$ColonyorTrip[j] != bird$ColonyorTrip[j+1]) {
         bird$tripID[j] <- tripID
       } else {
         bird$tripID[j] <- 0}
       if(bird$ColonyorTrip[nRows+1] == "trip") {
         bird$tripID[nRows+1] <- tripID ## set the last row correctly
       }
     }

   ## Creating unique ID variable
   bird$birdTrip <- paste(bird$trackID, bird$tripID, sep = "_")

   ## Getting rid of colony points; perhaps YES or NO in the function()
   bird <- subset(bird, bird$tripID > 0)

  lall <- bird[!duplicated(bird$birdTrip),]
  lall$nbTrips <- length(unique(lall$birdTrip))
  lall <- lall[!duplicated(lall$trackID),]
  lall$random <- r

  scol.list[[f]] <- lall # put all of these cut down frames in a list
  rm(bird)
  allbirds <- do.call(rbind, scol.list) # convert the list into one big dataframe

  cat("nRep =", f, "\n")

  }

cat("Scenario =", p, "\n")
allbirds$Scenario <- paste(p*1000)
arS[[i]] <- allbirds
rm(allbirds)

  }

d <- do.call(rbind, arS)
d$Scenario <- as.numeric(d$Scenario)
p1 <- ggplot2::ggplot(d, ggplot2::aes(x = Scenario, y = nbTrips)) + ggplot2::stat_summary(fun = mean, geom = "line")

gdf <- ggplot2::ggplot_build(p1)$data[[1]]
df <- gdf[, c(1, 3)]
colnames(df) <- c("Scenario", "Mean")

l <- list()
y <- unique(df$Scenario)
x <- nrow(df)
C <- rep(NA, nrow(df))
Ci <- rep(NA, nrow(df)-1)
val <- NULL

  for (i in 2:nrow(df)){
    C[i] <- df$Mean[[i]] - df$Mean[[i - 1]]
  }

  for (i in 2:nrow(df)-1){
    Ci[i] <- abs(C[i + 1] - C[i])
  }

  Ci[nrow(df)] <- NA
  df$SORC <- Ci

  df[,4] <- df$SORC < max(df$Mean)*sorc
  tresh <- df$Scenario[which(df$V4 == TRUE)[1]]

p2 <- ggplot2::ggplot(d,ggplot2::aes(x = Scenario, y = nbTrips)) +
  ggplot2::stat_summary(fun.data = "mean_cl_boot") +
  ggplot2::stat_summary(fun = mean, geom = "line") + ggplot2::theme_bw() +
  ggplot2::theme(axis.title.x=ggplot2::element_blank(), axis.title.y=ggplot2::element_blank()) +
  ggplot2::geom_vline(xintercept = tresh, alpha = 0.1, col = "blue", size = 5)

p3 <- ggplot2::ggplot(df, ggplot2::aes(x = Scenario, y = SORC)) +
  ggplot2::geom_point() + ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::theme(legend.position = "none",
    axis.title.x=ggplot2::element_blank(), axis.title.y=ggplot2::element_blank()) +
  ggplot2::geom_vline(xintercept = tresh, alpha = 0.1, col = "blue", size = 5)

g <- suppressWarnings(gridExtra::grid.arrange(p2, p3, ncol=2,left = "Number of trips per individual", bottom = "Scenario (m)"))

rlist <- suppressWarnings(list("detSens" = d[, c(4:5, 11, 13)], "detSORC" = df[, c(1:3)], fig = g))

return(rlist)

}

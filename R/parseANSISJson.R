
# parseANSISJson
#
#' Parses an ANSIS JSON response into an R ANSIS object
#' @param ansisResponse The path to the ANSIS JSON file to parse

#' @examples parseANSISJson()

#' @details  This function parses an ANSIS JSON response into a form that is more amenable to further massage
#' @author Ross Searle
#' @return list
#' @export

parseANSISJson <- function(ansisResponse, numCPUs=NULL){
  
  if(class(ansisResponse)=='list'){
  #  print('using R list')
    r <- ansisResponse
  } else{
  #  cat('Reading the JSON data .....\n\n')
    sl <- jsonlite::fromJSON(ansisResponse , simplifyDataFrame = F)
    r <- sl
    
    isansis <- r$`$schema`
    if(is.null(isansis)){
      stop('This is not a valid ANSIS JSON response')
    }else if (isansis!='https://anzsoildata.github.io/def-au-schema-json/schema/domain/2023-07-31/ansis.json'){
      stop('This is not a valid ANSIS JSON response')
    }
  }
  
  
  cat('\nParsing the ANSIS JSON Response .....\n\n')
  
  
  if(length(r$data) <= 1){
    return(parseANSISJsonSerial(r))
  }else{
    return(parseANSISJsonParallel(r, numCPUs))
  }
}


parseANSISJson2 <- function(jsnDir, numCPUs=NULL){
  
  # if(class(ansisResponse)=='list'){
  #   #  print('using R list')
  #   r <- ansisResponse
  # } else{
  #   #  cat('Reading the JSON data .....\n\n')
  #  # sl <- jsonlite::fromJSON(ansisResponse , simplifyDataFrame = F)
  #  #  r <- sl
  #   
  #   # isansis <- r$`$schema`
  #   # if(is.null(isansis)){
  #   #   stop('This is not a valid ANSIS JSON response')
  #   # }else if (isansis!='https://anzsoildata.github.io/def-au-schema-json/schema/domain/2023-07-31/ansis.json'){
  #   #   stop('This is not a valid ANSIS JSON response')
  #   # }
  # }
  
  
  cat('\nParsing the ANSIS JSON Response .....\n\n')
  
  
  # if(length(r$data) <= 1){
  #   return(parseANSISJsonSerial(r))
  # }else{
    return(parseANSISJsonParallel2(jsnDir, numCPUs))
  # }
}


parseANSISJsonSerial <- function(r){


  sol <- list()
  nsites <- length(r$data)

  pb <-progress::progress_bar$new(
    format = "  Parsing ANSIS response :what [:bar] :percent in :elapsed",
    total = nsites, clear = FALSE, width= 100, show_after=1)


  for (k in  1:nsites) {

      s <- r$data[[k]]
      sid <- getSiteID(siteAsList=s, projects = r$included$projects)

      pb$tick(tokens = list(what = stringr::str_pad(sid, 20, 'left')))
      layersTable <- parseANSISSiteLayersToDenormalisedTable(siteAsList=s)
      siteVistTable <- parseANSISSiteVistToDenormalisedTable(siteAsList=s)

      loc <- getSiteLocation(siteAsList=s)
      pl <- list()
      pl$Site=sid
      pl$X=loc$X
      pl$Y=loc$Y
      dt <- getSiteDate(siteAsList=s)
      pl$Date=dt
      pl$data <-  layersTable
      pl$siteVisitTable <- siteVistTable

      sol[[sid]] <- pl

    }


#  cat('\nCreating the ANSIS Data Object .....\n\n')
  locsDF <- makeSitesLocationTableFromDataList(sol)
  jL <- list()
  jL$dfDenorm <- sol
  jL$locsDF <- locsDF
  jL$jsonList <- r

  jL$CSV <- makeAllDataCSV(sol)

  return(jL)
}



parseANSISJsonParallel <- function(r, numCPUs=NULL){
  
  nsites <- length(r$data)
  
  if(is.null(numCPUs)){
    numCPUs = min(nsites, parallel::detectCores()-1)
  }else{
    
  }
  numCPUs <- min(20, numCPUs)
  cat(paste0('\n\nNumber of CPUs = ', numCPUs, '. A maximum of 20 allowed.'))
  
  cl <- parallel::makePSOCKcluster(numCPUs)
  doSNOW::registerDoSNOW(cl)
  
 mps <- DataSets@mps
 CodesTable <- DataSets@CodesTable
  
 `%dopar%` <- foreach::`%dopar%`
  
  pb <- txtProgressBar(max=nsites, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  pout <-  foreach::foreach(k=1:nsites, .options.snow=opts, .packages=c('stringr'), 
               .export = c('getSiteID', 'parseANSISSiteLayersToDenormalisedTable', 'parseANSISSiteVistToDenormalisedTable', 'getSiteLocation', 
                           'mps', 'CodesTable', 'isLabProperty', 'getMorphVals', 'CodesTable', 'getLabVals', 'getSiteVisitVals', 'getSlope', 'getSlopeUnit')) %dopar% {
    
    sol2 <- list()
    s <- r$data[[k]]
    sid <- getSiteID(siteAsList=s)
    
    layersTable <- parseANSISSiteLayersToDenormalisedTable(siteAsList=s)
    siteVistTable <- parseANSISSiteVistToDenormalisedTable(siteAsList=s)
    
    loc <- getSiteLocation(siteAsList=s)
    pl <- list()
    pl$Site=sid
    pl$X=loc$X
    pl$Y=loc$Y
    dt <- getSiteDate(siteAsList=s)
    pl$Date=dt
    pl$data <-  layersTable
    pl$siteVisitTable <- siteVistTable
    
    sol2[[sid]] <- pl
    return(sol2)
    
    }
  
  close(pb)
  parallel::stopCluster(cl)


  sol <- do.call(c, pout)

  
  cat('\nCreating the ANSIS Data Object .....\n\n')
  
  cat('\nMaking the Site Locations table .....\n\n')
  locsDF <- makeSitesLocationTableFromDataList(dl=sol)
  jL <- list()

  jL$locsDF <- locsDF
  jL$jsonList <- r
  cat('\nMaking the CSV data table .....\n\n')
  jL$CSV <- makeAllDataCSV(allsites=sol)
  
  
  return(jL)
}



parseANSISJsonParallel2 <- function(jsnDir, numCPUs=NULL){
  
  #fls <- list.files(jsnDir, pattern = '.jsn$')
  fls <- list.files(jsnDir, pattern = '.json$', full.names = T)
  
  
  nfiles <- length(fls)
  
  if(is.null(numCPUs)){
    numCPUs = min(nfiles, parallel::detectCores()-1)
  }else{
    
  }
  numCPUs <- min(20, numCPUs)
  cat(paste0('\n\nNumber of CPUs = ', numCPUs, '. A maximum of 20 allowed.'))
  
  cl <- parallel::makePSOCKcluster(numCPUs)
  doSNOW::registerDoSNOW(cl)
  
  mps <- DataSets@mps
  CodesTable <- DataSets@CodesTable
  
  `%dopar%` <- foreach::`%dopar%`
  
  pb <- txtProgressBar(max=nfiles, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  pout <-  foreach::foreach(k=1:nfiles,  .options.snow=opts, .packages=c('stringr'), 
                            .export = c('normaliseSite', 'getSiteID', 'parseANSISSiteLayersToDenormalisedTable', 'parseANSISSiteVistToDenormalisedTable', 'getSiteLocation', 
                                        'mps', 'CodesTable', 'isLabProperty', 'getMorphVals', 'CodesTable', 'getLabVals', 'getSiteVisitVals', 'getSlope', 'getSlopeUnit')) %dopar% {
                                          
                                          
                                          r <- jsonlite::fromJSON(fls[k], simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F)
                                          # tictoc::tic()
                                          # sol2 <- lapply(r$data, normaliseSite)
                                          # tictoc::toc()
                                          
                                        #for (i in 1:length(fls)) {
                                        #  r <- jsonlite::fromJSON(fls[k], simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F)
                                        #ol$data <- append(ol$data,jl$data)
                                            
                                          sol2 <- list()

                                         for (i in 1:length(r$data )) {
                                             try({
                                                s <- r$data[[i]]
                                                p <- r$included$projects
                                                sid <- getSiteID(siteAsList=s, projects=p)
      
                                                layersTable <- parseANSISSiteLayersToDenormalisedTable(siteAsList=s)
                                                siteVistTable <- parseANSISSiteVistToDenormalisedTable(siteAsList=s)
      
                                                loc <- getSiteLocation(siteAsList=s)
                                                
                                                pl <- list()
                                                pl$Site=sid
                                                pl$X=loc$X
                                                pl$Y=loc$Y
                                                dt <- getSiteDate(siteAsList=s)
                                                pl$Date=dt
                                                pl$data <-  layersTable
                                                pl$siteVisitTable <- siteVistTable
                                            
                                              sol2[[sid]] <- pl
                                              })

                                         }

                                          return(sol2)
                                       
                                        }
  
  close(pb)
  parallel::stopCluster(cl)
  
  
  sol <- do.call(c, pout)
  
  
  cat('\nCreating the ANSIS Data Object .....\n\n')
  
  cat('\nMaking the Site Locations table .....\n\n')
  locsDF <- makeSitesLocationTableFromDataList(dl=sol)
  jL <- list()
  
  jL$locsDF <- locsDF
  #jL$jsonList <- r
  cat('\nMaking the CSV data table .....\n\n')
  jL$CSV <- makeAllDataCSV(allsites=sol)
  
  
  return(jL)
}





#' # getAO
#' #
#' #' Example Data
#' #' @export
#' getAO <- function(){
#'   ao <- ansisExampleObject
#'   return(ao)
#' }


# makeWideTable
#
#' Format the ANSIS response into a wide table 
#' @param anisObject An ANSIS R object
#' @param propertyType Return data for either 'SiteVisit', 'Horizons' or 'Lab'
#' @param propertyType Return data for a specific list of soil properties
#' @param decode return either coded or decoded values (default=F)

#' @details  You need to specify a parameter value for either 'propertyType' or 'properties'. 
#' The data frame contains SiteID, location, depths and soil property fields
#' @author Ross Searle
#' @return dataframe
#' @export

makeWideTable <- function(ansisObject, propertyType=NULL, labcodes=NULL, decode=F){


  if(is.null(propertyType) & is.null(labcodes)){
    stop('You have to specify a value for either the propertyType or properties parameter')
  }
  
  alldf <- ansisObject$CSV
  
  alldf$Longitude <- as.numeric(alldf$Longitude)
  alldf$Latitude <- as.numeric(alldf$Latitude)
  alldf$UpperDepth <- as.numeric(alldf$UpperDepth)
  alldf$LowerDepth <- as.numeric(alldf$LowerDepth)
  
 
  if(!is.null(labcodes)){
    idxs <- which(!labcodes %in% unique(alldf$Property))
    if(length(idxs) > 0){
      stop('One or more supplied properties are not available in the ANSIS response. Use the "getAvailableProperties" function to see what properties are available')
    }else{
      cols <- labcodes
      alldf <- alldf[alldf$Property %in% labcodes,]
    }
  }else if(!is.null(propertyType)){
    alldf <- alldf[alldf$PropertyType==propertyType,]
    cols <- unique(alldf$Property)
    
  }

  baseCols <- unique(alldf[c('Site','Longitude', 'Latitude',  'UpperDepth', 'LowerDepth')])
  baseCols$UpperDepth <- as.numeric(baseCols$UpperDepth)
  baseCols$LowerDepth <- as.numeric(baseCols$LowerDepth)
  baseCols <- baseCols[with(baseCols, order(Site, UpperDepth, LowerDepth)), ]
  
  
  
  for (i in 1:length(cols)) {
    c <- cols[i]
    baseCols[c] <- rep('', nrow(baseCols))
  }

  bt <- baseCols
  nt <- alldf
  
 
  
  pb <- progress::progress_bar$new(total = nrow(bt))
  for (i in 1:nrow(bt)) {
    rec <- bt[i, ]
    sid <- rec$Site
    ud <- rec$UpperDepth
    ld <- rec$LowerDepth
    for (j in 1:length(cols)) {
      att <- cols[j]

      # if(decode){
      #   v <- nt[nt$Site==sid & nt$UpperDepth==ud & nt$LowerDepth==ld & nt$Property==att, ]$Description
      # }else{
        v <- nt[nt$Site==sid & nt$UpperDepth==ud & nt$LowerDepth==ld & nt$Property==att, ]$Value
     # }

      vc <- paste(v, sep = " ", collapse = '; ')
      bt[i, ][att] <- vc
    }
    pb$tick()
  }
  return(bt)
}

 # rows <- pbapply:::pbapply(bt[1:100,], 1, FUN = makeWideRow, nt, simplify = T )
 # z <- do.call(rbind, rows)
# 
# makeWideRow <- function(rec, nt){
#   #rec <- bt[i, ]
#   #print(str(rec))
#      sid <- rec[['Site']]
#      ud <- rec[['UpperDepth']]
#      ld <- rec[['LowerDepth']]
#     for (j in 1:length(cols)) {
#       att <- cols[j]
# 
#       # if(decode){
#       #   v <- nt[nt$Site==sid & nt$UpperDepth==ud & nt$LowerDepth==ld & nt$Property==att, ]$Description
#       # }else{
#         v <- nt[nt$Site==sid & nt$UpperDepth==ud & nt$LowerDepth==ld & nt$Property==att, ]$Value
#      # }
# 
#       vc <- paste(v, sep = " ", collapse = '; ')
#       rec[att] <- vc
#     }
#      return(as.data.frame(rec))
# }


makeDSMTable <- function(ansisObject, propertyCode=NULL){
  
  adf <- ansisObject$CSV
  idxs <- which(adf$Property == propertyCode )
  odf <- adf[idxs,]
  return(odf)
}
  
  

# getAvailableProperties
#
#' Return all data for a site
#' @param anisObject An ANSIS R object
#' @param propertyType  Return data for either 'SiteVisit', 'Horizons' or 'Lab'

#' @details Return all of the soil properties names that are available in the ANSIS R object
#' @author Ross Searle

#' @return dataframe
#' @export

getAvailableProperties <- function(anisObject, propertyType=NULL){

 # alldf <- makeAllDataCSV(allsites=anisObject$dfDenorm)
  
  alldf <- anisObject$CSV

  if(is.null(propertyType)){
    r <- unique(alldf$Property)
  }else{
    df <- alldf[alldf$PropertyType==propertyType,]
    r <- unique(df$Property)
  }
  return(r)
}


# getSiteData
#
#' Return all data for a site
#' @param anisObject An ANSIS R object
#' @param siteID Site ID

#' @details Return all data for a site from an ANSIS R object as a dataframe
#' @author Ross Searle
#' @return dataframe
#' @export

getSiteData <- function(ansisObject, siteID){
 
 s <- ansisObject$CSV[ansisObject$CSV$Site==siteID,] 
 return(s)
  
}


# getSiteLocations
#
#' Return all site locations
#' @param anisObject An ANSIS R object

#' @details Return all site locations from an ANSIS R object as a dataframe
#' @author Ross Searle
#' @export

getSiteLocations <- function(ansisObject){
  
  sites <- ansisObject$locsDF
  colnames(sites) <- c( 'Site', 'Longitude', 'Latitude')
  return(sites)
}


# getPropertiesSummary
#
#' Return a summary of the number of soil properties
#' @param anisObject An ANSIS R object

#' @details Return a summary of the number of soil properties in an ANSIS R object as a dataframe
#' @author Ross Searle
#' @return dataframe
#' @export

getPropertiesSummary<- function(ansisObject){
  
  grp = as.data.frame(ansisObject$CSV %>% group_by(Property) %>%
    summarise(Count = n(), 
   .groups = 'drop'))
  return(grp)
}










getSitesSummary <- function(){
  
  as.data.frame(ansisObject$CSV %>% group_by(Site, PropertyType) %>%
                        summarise(Count = n(), 
                                  .groups = 'drop'))
  
}


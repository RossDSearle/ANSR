



# parseANSISJson
#
#' Parses an ANSIS JSON response into an R ANSIS object
#' @param jsnFile The path to the ANSIS JSON file to parse

#' @examples parseANSISJson()

#' @details  This function parses an ANSIS JSON response into a form that is more amenable to further massage
#' @author Ross Searle
#' @return list
#' @export

parseANSISJson <- function(jsnFile, outDir=NULL){

 
  
cat('Reading the JSON data .....\n\n')
  sl <- jsonlite::fromJSON(jsnFile , simplifyDataFrame = F)
  r <- sl
  
  isansis <- r$`$schema`
  if(is.null(isansis)){
    stop('This is not a valid ANSIS JSON response')
  }else if (isansis!='https://anzsoildata.github.io/def-au-schema-json/schema/domain/2023-07-31/ansis.json'){
    stop('This is not a valid ANSIS JSON response')
  }
  
  sol <- list()
  nsites <- length(r$data)
  
  pb <-progress::progress_bar$new(
    format = "  Parsing ANSIS response :what [:bar] :percent in :elapsed",
    total = nsites, clear = FALSE, width= 100, show_after=1)

  
  for (k in  1:nsites) {
      
      s <- r$data[[k]]
      sid <- getSiteID(siteAsList=s)
      
        pb$tick(tokens = list(what = stringr::str_pad(sid, 20, 'left')))
     
      layersTable <- parseANSISSiteLayersToDenormalisedTable(siteAsList=s)
      siteVistTable <- parseANSISSiteVistToDenormalisedTable(siteAsList=s)

      loc <- getSiteLocation(siteAsList=s)
      pl <- list()
      pl$Site=sid
      pl$X=loc$X
      pl$Y=loc$Y
      pl$data <-  layersTable
      pl$siteVisitTable <- siteVistTable

      sol[[sid]] <- pl

    }

  
  cat('\nCreating the ANSIS Data Object .....\n\n')
  locsDF <- makeSitesLocationTableFromDataList(sol)
  jL <- list()
  jL$dfDenorm <- sol
  jL$locsDF <- locsDF
  jL$jsonList <- r
  
  jL$CSV <- makeAllDataCSV(sol)
  
  if(!is.null(outDir)){
   bn <- basename(tools::file_path_sans_ext(jsnFile))
   if(!dir.exists(outDir)){
     cat('Specified output directory does not exist so it will be created.\n')
     dir.create(outDir, recursive = T)
   }
   saveRDS(jL, paste0(outDir, '/', bn, '.rds'))
   cat(paste0('ANSIS Data Object saved to - ', outDir, '/', bn, '.rds', '.\n'))
  }

  return(jL)
}


# getAO
#
#' Example Data
#' @export
getAO <- function(){
  ao <- ansisExampleObject
  return(ao)
}


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
#' @examples 
#' ao <- getAO()
#' makeWideTable(ansisObject=ao, propertyType='Horizons')
#' @return dataframe
#' @export

makeWideTable <- function(ansisObject, propertyType=NULL, properties=NULL, decode=F){


  if(is.null(propertyType) & is.null(properties)){
    stop('You have to specify a value for either the propertyType or properties parameter')
  }
  
  alldf <- ansisObject$CSV
  
  alldf$Longitude <- as.numeric(alldf$Longitude)
  alldf$Latitude <- as.numeric(alldf$Latitude)
  alldf$UpperDepth <- as.numeric(alldf$UpperDepth)
  alldf$LowerDepth <- as.numeric(alldf$LowerDepth)
  
 
  if(!is.null(properties)){
    idxs <- which(!properties %in% unique(alldf$Property))
    if(length(idxs) > 0){
      stop('One or more supplied properties are not available in the ANSIS response. Use the "getAvailableProperties" function to see what properties are available')
    }else{
      cols <- properties
      alldf <- alldf[alldf$Property %in% properties,]
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
  for (i in 1:nrow(bt)) {
    rec <- bt[i, ]
    sid <- rec$Site
    ud <- rec$UpperDepth
    ld <- rec$LowerDepth
    for (j in 1:length(cols)) {
      att <- cols[j]

      if(decode){
        v <- nt[nt$Site==sid & nt$UpperDepth==ud & nt$LowerDepth==ld & nt$Property==att, ]$Description
      }else{
        v <- nt[nt$Site==sid & nt$UpperDepth==ud & nt$LowerDepth==ld & nt$Property==att, ]$Value
      }

      vc <- paste(v, sep = " ", collapse = '; ')
      bt[i, ][att] <- vc
    }

  }
  return(bt)
}


# getAvailableProperties
#
#' Return all data for a site
#' @param anisObject An ANSIS R object
#' @param propertyType  Return data for either 'SiteVisit', 'Horizons' or 'Lab'

#' @details Return all of the soil properties names that are available in the ANSIS R object
#' @author Ross Searle
#' @examples 
#' ao <- getAO()
#' getSiteData(ansisObject=ao, propertyType='Lab')
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
#' @examples 
#' ao <- getAO()
#' getSiteData(ansisObject=ao, siteID='CSIRO+503+BIF+DIEM40')
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
#' @examples 
#' ao <- getAO()
#' getSiteLocations(ansisObject=ao)
#' @return dataframe
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
#' @examples 
#' ao <- getAO()
#' getPropertiesSummary(ansisObject=ao)
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


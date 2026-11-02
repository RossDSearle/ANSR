
#' parseANSISJson
#'
#'  Parses an ANSIS JSON response into an R ANSISDataObject
#' @param zipFile The path to the ANSIS query JSON file to parse
#' @param parseAll If false only the ANSIS properties listed will be parsed to the flat file structure. This improves the parsing speed. If True all fields in in the ASNSI query respnse are parsed to the flat file
#' @param propertiesToParse If parseAll=false this is the list of ANSIS properties that will be output to the falt file
#' @param numCPUs The number of cpus to use for the json parsing. If NULL all available cpus minus one are used
#' @param verbose Keep extra, but not often used in the flat file output
#' @param viewSummaries If True a summary of the propertu counts will be displayed when the parsing is completed

#'
# #' @examples parseANSISJson()

#' @details  This function parses an ANSIS JSON response into a flat file format that is more amenable to further massage
#' @author Ross Searle
#' @return list
#' @export


parseANSISJsonParallel3 <- function(zipFile, numCPUs=NULL, propertiesToParse=NULL, verbose=F, parseAll=T, viewSummaries=F){

  outDir <- stringr::str_split(paste0(tempdir(), '/tmp_', as.numeric(Sys.time())), '[.]')[[1]][1]
  if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}

 # cat(paste0('\nExtracting files from the zip archive into - ', outDir, '\n'))
  cat(paste0('\nParsing the ANSI json into a flat structure...\n'))

  unzip(
    zipfile = zipFile,
    exdir = outDir
  )

    ansisData <- ANSIS_JSON_parsing_All(inDir=outDir, target_fields=propertiesToParse)

  # Post-processing and export
  if (nrow(ansisData) > 0) {

    if(!verbose){
      ansisData <- ansisData[, -c(12,19,20,21)]
    }

    ansisData <- ansisData |> dplyr::arrange(SiteID, endAtTime, fieldDepth, Property , depthUpper, depthLower )

    d1 <- stringr::str_replace(zipFile, 'RawJSONResponses', 'ANSISDataObjects')
    ofData <- stringr::str_replace(d1, '.zip$', '.rds')

    cat('\nCreating the ANSIS Data Object .....\n\n')

    cat('\nMaking the Site Locations table .....\n\n')
    locsDF <- makeSitesLocationTable(df=ansisData)
    summ <- summariseOutputs(df=ansisData)

    jL <- list()
    jL$locations <- locsDF
    jL$Data <- ansisData
    jL$DataSummary <- summ

    saveRDS(jL, ofData)

    unlink(outDir, recursive = TRUE)
    message(sprintf("Success! Exported %s rows to %s", nrow(ansisData), ofData))
    print(head(jL$DataSummary$Totals))
    if(viewSummaries){
      print(listviewer::jsonedit(jL$DataSummary))
    }

    return(jL)

    } else {
      message("No records found for any of the target fields. No output created.")
      return(NULL)
  }

}


parseANSISJsonSingleSite3 <- function(jsnFileName, propertiesToParse=NULL, verbose=F, parseAll=T, viewSummaries=F){

  ansisData <- ANSIS_JSON_parsing_SingleSite(jsnFileName, target_fields=propertiesToParse)

  # Post-processing and export
  if (nrow(ansisData) > 0) {

    if(!verbose){
      ansisData <- ansisData[, -c(12,19,20,21)]
    }

    ansisData <- ansisData |> dplyr::arrange(SiteID, endAtTime, fieldDepth, Property , depthUpper, depthLower )

    # d1 <- stringr::str_replace(zipFile, 'RawJSONResponses', 'ANSISDataObjects')
    # ofData <- stringr::str_replace(d1, '.zip$', '.rds')
    #
    # cat('\nCreating the ANSIS Data Object .....\n\n')

    #cat('\nMaking the Site Locations table .....\n\n')
    locsDF <- makeSitesLocationTable(df=ansisData)
    summ <- summariseOutputs(df=ansisData)

    jL <- list()
    jL$locations <- locsDF
    jL$Data <- ansisData
    jL$DataSummary <- summ

    #saveRDS(jL, ofData)

    # unlink(outDir, recursive = TRUE)
    # message(sprintf("Success! Exported %s rows to %s", nrow(ansisData), ofData))
    # print(head(jL$DataSummary$Totals))
    # if(viewSummaries){
    #   print(listviewer::jsonedit(jL$DataSummary))
    # }

    return(jL)

  } else {
    message("No records found for any of the target fields. No output created.")
    return(NULL)
  }

}





# makeWideTable <- function(ansisObject, propertyType=NULL, labcodes=NULL, decode=F){
#
#
#   if(is.null(propertyType) & is.null(labcodes)){
#     stop('You have to specify a value for either the propertyType or properties parameter')
#   }
#
#   alldf <- ansisObject$CSV
#
#   alldf$Longitude <- as.numeric(alldf$Longitude)
#   alldf$Latitude <- as.numeric(alldf$Latitude)
#   alldf$UpperDepth <- as.numeric(alldf$UpperDepth)
#   alldf$LowerDepth <- as.numeric(alldf$LowerDepth)
#
#
#   if(!is.null(labcodes)){
#     idxs <- which(!labcodes %in% unique(alldf$Property))
#     if(length(idxs) > 0){
#       stop('One or more supplied properties are not available in the ANSIS response. Use the "getAvailableProperties" function to see what properties are available')
#     }else{
#       cols <- labcodes
#       alldf <- alldf[alldf$Property %in% labcodes,]
#     }
#   }else if(!is.null(propertyType)){
#     alldf <- alldf[alldf$PropertyType==propertyType,]
#     cols <- unique(alldf$Property)
#
#   }
#
#   baseCols <- unique(alldf[c('Site','Longitude', 'Latitude',  'UpperDepth', 'LowerDepth')])
#   baseCols$UpperDepth <- as.numeric(baseCols$UpperDepth)
#   baseCols$LowerDepth <- as.numeric(baseCols$LowerDepth)
#   baseCols <- baseCols[with(baseCols, order(Site, UpperDepth, LowerDepth)), ]
#
#
#
#   for (i in 1:length(cols)) {
#     c <- cols[i]
#     baseCols[c] <- rep('', nrow(baseCols))
#   }
#
#   bt <- baseCols
#   nt <- alldf
#
#
#
#   pb <- progress::progress_bar$new(total = nrow(bt))
#   for (i in 1:nrow(bt)) {
#     rec <- bt[i, ]
#     sid <- rec$Site
#     ud <- rec$UpperDepth
#     ld <- rec$LowerDepth
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
#       bt[i, ][att] <- vc
#     }
#     pb$tick()
#   }
#   return(bt)
# }



#' makeDSMTable <- function(ansisObject, propertyCode=NULL){
#'
#'   adf <- ansisObject$CSV
#'   idxs <- which(adf$Property == propertyCode )
#'   odf <- adf[idxs,]
#'   return(odf)
#' }
#'
#'
#'
#' # getAvailableProperties
#' #
#' #' Return all data for a site
#' #' @param anisObject An ANSIS R object
#' #' @param propertyType  Return data for either 'SiteVisit', 'Horizons' or 'Lab'
#'
#' #' @details Return all of the soil properties names that are available in the ANSIS R object
#' #' @author Ross Searle
#'
#' #' @return dataframe
#' #' @export
#'
#' getAvailableProperties <- function(anisObject, propertyType=NULL){
#'
#'  # alldf <- makeAllDataCSV(allsites=anisObject$dfDenorm)
#'
#'   alldf <- anisObject$CSV
#'
#'   if(is.null(propertyType)){
#'     r <- unique(alldf$Property)
#'   }else{
#'     df <- alldf[alldf$PropertyType==propertyType,]
#'     r <- unique(df$Property)
#'   }
#'   return(r)
#' }
#'
#'
#' # getSiteData
#' #
#' #' Return all data for a site
#' #' @param anisObject An ANSIS R object
#' #' @param siteID Site ID
#'
#' #' @details Return all data for a site from an ANSIS R object as a dataframe
#' #' @author Ross Searle
#' #' @return dataframe
#' #' @export
#'
#' getSiteData <- function(ansisObject, siteID){
#'
#'  s <- ansisObject$CSV[ansisObject$CSV$Site==siteID,]
#'  return(s)
#'
#' }
#'
#'
#' # getSiteLocations
#' #
#' #' Return all site locations
#' #' @param anisObject An ANSIS R object
#'
#' #' @details Return all site locations from an ANSIS R object as a dataframe
#' #' @author Ross Searle
#' #' @export
#'
#' getSiteLocations <- function(ansisObject){
#'
#'   sites <- ansisObject$locsDF
#'   colnames(sites) <- c( 'Site', 'Longitude', 'Latitude')
#'   return(sites)
#' }
#'








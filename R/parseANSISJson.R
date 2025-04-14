
parseANSISJson <- function(jsnFile){

  sl <- fromJSON(jsnFile , simplifyDataFrame = F)
  r <- sl
  sol <- list()
  nsites <- length(r$data)


    for (k in  1:nsites) {

      s <- r$data[[k]]
      sid <- getSiteID(siteAsList=s)
      # parseANSISSiteVistToDenormalisedTable
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

      pr <-(k/nsites) * 100
      if (pr < 99) {
        status <- "Loading..."
      }else{
        status <- "Finished loading data"
      }
    }

  #dfDenorm <- sol
  locsDF <- makeSitesLocationTableFromDataList(sol)
  jL <- list()
  jL$dfDenorm <- sol
  jL$locsDF <- locsDF
  jL$jsonList <- r

  ######  Morphology data   #####

  # morphDF <- data.frame()
  # for (i in 1:length(jL$dfDenorm)) {
  #
  #   site <- jL$dfDenorm[[i]]$data
  #   morphData <- site[site$propType=='Horizons', ]
  #   bt <- generateBlankTable(dfDenorm=morphData)
  #   morph <- populateTable(blankTable=bt, dfDenorm=morphData, decode=F)
  #   sInfo <- jL$locsDF
  #
  #
  # }


  return(jL)
}


makeWideTable <- function(anisObject, propertyType=NULL, properties=NULL, decode=NULL){

  alldf <- makeAllDataCSV(anisObject$dfDenorm)
  alldf <- alldf[alldf$PropertyType==propertyType,]
  alldf$Longitude <- as.numeric(alldf$Longitude)
  alldf$Latitude <- as.numeric(alldf$Latitude)
  alldf$UpperDepth <- as.numeric(alldf$UpperDepth)
  alldf$LowerDepth <- as.numeric(alldf$LowerDepth)
  cols <- unique(alldf$Property)

  if(!is.null(properties)){
    idxs <- which(!properties %in% cols)
    if(length(idxs) > 0){
      stop('One or more supplied properties are not available in the ANSIS response. Use the "getAvailableProperties" function to see what properties are available')
    }
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

getAvailableProperties <- function(anisObject, propertyType=NULL){

  alldf <- makeAllDataCSV(allsites=anisObject$dfDenorm)

  if(is.null(propertyType)){
    r <- unique(alldf$Property)
  }else{
    df <- alldf[alldf$PropertyType==propertyType,]
    r <- unique(df$Property)
  }
  return(r)
}


getSiteID <- function(siteAsList){

  scopeID <- siteAsList$scopedIdentifier[[1]]$value
  if(is.null(scopeID)){
    sid <- siteAsList$id
  }else{
    auth <- siteAsList$scopedIdentifier[[1]]$authority
    sid <- paste0(auth, '+', scopeID)
  }
  return(sid)
}


dumpCSV <- function(anisObject){
  makeAllDataCSV(allsites=anisObject$dfDenorm)
}


makeAllDataCSV <- function(allsites){

  alldf <- data.frame()
  for (i in 1:length(allsites)) {
    s <- allsites[[i]]
    sitedf<- makeSiteCSV(sl=s)
    alldf <- rbind(alldf, sitedf)
  }

  return(alldf)
}

makeSiteCSV <- function(sl){

  s<-sl
  odf <- data.frame()

  sid <- s$Site
  #head(s$data)
 # sdf <- data.frame(ud='', ld='', property='Location', propType='SiteVisit', field=c('Longitude, Latitude'),  value=c(s$X, s$Y), desc='')
 # odf <- rbind(odf, sdf)
  svdf <- data.frame(ud='', ld='', property='SiteVisit', propType='SiteVisit',field=s$siteVisitTable$property, value=s$siteVisitTable$value, desc=s$siteVisitTable$desc)
  odf <- rbind(odf, svdf)
  odf <- rbind(odf, s$data)
  sitedf <- data.frame(site=sid, Longitude=s$X, Latitude=s$Y, odf)
  colnames(sitedf) <- c("Site", "Longitude", "Latitude", "UpperDepth", "LowerDepth", "Group", "PropertyType", "Property", "Value", "Description" )
  return(sitedf)
}




parseANSISSiteLayersToDenormalisedTable <- function(siteAsList){

  slsl <- siteAsList[['siteVisit']][[1]]$soilProfile[[1]]$soilLayer

  if(!is.null(slsl)){

    alldf <- data.frame(ud=numeric(), ld=numeric(), property=character(), propType=character(), field=character(), value=character(), desc=character() )
    for (i in 1:length(slsl)) {

      l = slsl[[i]]

      ns <- names(l)

      ud <- l$depthUpper$result$value
      ld <- l$depthLower$result$value

      for (j in 1:length(ns)) {

        att <- ns[j]
        if(att %in% mp){
          alldf <- getMorphVals(layer=l, att=att, mps=mps, ud, ld, alldf)
        }else if(isLabProperty(layer=l, prop=att)){
          alldf <- getLabVals(layer=l, prop=att, mps=mps, ud, ld, alldf)
        }
      }
    }
    return(alldf)
  }
}


isLabProperty <- function(layer, prop){

  tryCatch({
    if(length(layer[[prop]][[1]]$usedProcedure) > -1 & !is.null(layer[[prop]][[1]]$usedProcedure))
      return(T)
  }, error = function(e) {
    return(F)
  })
  return(F)

}


getMorphVals <- function(layer, att, mps, ud, ld, alldf){

  l=layer

  schRoot <- '/SoilSite/data/siteVisit/soilProfile/soilLayer/'

  flds <- mps[mps$Domain==att & mps$Fields!='', ]

  for (i in 1:nrow(flds)) {

    fld = str_split(flds[i, ]$Fields, ':')[[1]]
    arrayed=flds[i, ]$Arrayed

    if(length(fld)==1 & arrayed=='No'){
      vv = layer[[att]][[fld[1]]]
    }else if(length(fld)==1 & arrayed=='Yes'){
      vv = layer[[att]][[1]][[fld[1]]]
    }else if(length(fld)==2 & arrayed=='No'){
      vv = layer[[att]][[fld[1]]][[fld[2]]]
    }else if(length(fld)==2 & arrayed=='Yes'){
      vv = layer[[att]][[1]]  [[fld[[1]][[1]]]] [1][[fld[2]]]
    }
    repStr = flds[i, ]$Replace
    if(repStr==''){
      v <- vv
    }else{
      v <- str_remove(vv, repStr)
    }
    if(length(v)==0){v=''}


    desc=''
    of <- paste0(flds[i, ]$Property)

    domain=paste0('C_', of)
    cds <- CodesTable[CodesTable$code_domain==domain,]

    dec <- cds[cds$code_value==v,]
    if(nrow(cds>0)){

      if(nrow(dec)==1){
        desc <- dec$code_desc
      }else{
        desc <- v
      }


    }else{
      desc <- v
    }


    #if(v!=''){
    #of <- paste0(att, '_', flds[i, ]$Fields)

    hrr <- data.frame(ud=ud, ld=ld, property=att, propType='Horizons', field=of, value=v, desc=desc)
    alldf <- rbind(alldf, hrr)
    #}
  }

  return(alldf)
}

getLabVals <- function(layer, prop, mps, ud, ld, alldf){

  desc=''
  l=layer
  up <- l[[prop]][[1]]$usedProcedure

  p <- str_remove(up, 'scm:')
  v <- l[[prop]][[1]]$result$value
  u <- l[[prop]][[1]]$result$unit
  r <- data.frame(ud=ud, ld=ld, property='LabResults', propType='Lab', field=p, value=v, desc=desc)
  alldf <- rbind(alldf, r)

  return(alldf)
}


parseANSISSiteVistToDenormalisedTable <- function(siteAsList){

  sv <- siteAsList

  alldf <- data.frame(property=character(), propType=character(), schemaPath=character(), value=character(), desc=character() )

  alldf <- getSiteVisitVals(val=sv$scopedIdentifier[[1]]$value, att='S_ID', alldf, schemaPath='/SoilSite/data/scopedIdentifier/value')
  alldf <- getSiteVisitVals(val=sv$scopedIdentifier[[1]]$authority, att='AGENCY_CODE', alldf, schemaPath='/SoilSite/data/scopedIdentifier/authority')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$usedProcedure, att='O_TYPE', alldf, schemaPath='/SoilSite/data/siteVisit/soilProfile/usedProcedure')

  dt <-  sv$siteVisit[[1]]$startedAtTime

  ###  not sure if it is a required field and needs a bit of massaging
  if(!is.null(dt)){
    sDate <- str_split(dt, 'T')[[1]][1]
  }else{
    sDate <- ''
  }
  hrr <- data.frame(property='S_DATE_DESC', propType='SiteVisit', schemaPath='/SoilSite/data/siteVisit/endedAtTime', value=sDate, desc=sDate)
  alldf <- rbind(alldf, hrr)



  #  disturbance
  alldf <- getSiteVisitVals(val=sv$disturbance[[1]]$result, att='O_SOIL_DISTURB', alldf, schemaPath = '/SoilSite/data/disturbance/result')

  #  landformElement
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]]$ansisType, att='S_ELEM_TYPE', alldf, schemaPath='/SoilSite/data/siteVisit/landform/landformElement/ansisType')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]]$height$result$value, att='S_ELEM_HEIGHT', alldf, schemaPath = '/SoilSite/data/siteVisit/landform/landformElement/height/result/value')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]]$height$result$unit, att='S_ELEM_HEIGHT_UNIT', alldf, schemaPath = '/SoilSite/data/siteVisit/landform/landformElement/height/result/unit')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]]$morphologicalType$result, att='S_MORPH_TYPE', alldf, schemaPath = '/SoilSite/data/siteVisit/landform/landformElement/morphologicalType/result')

  # v<-NULL

  ##### This can probably be replaced by the standard getSiteVisitVals function with the try ctach added
  slopeVal <- getSlope(sv)
  slopeUnit <- getSlopeUnit(sv)

  alldf <- getSiteVisitVals(val=paste0(slopeVal, ' ', slopeUnit), att='S_SLOPE', alldf)

  # if(!is.null(v)){
  #   print('isnotnull')
  #   slopeVal <- v
  # }else{
  #   slopeVal <- ''
  # }
  #
  # v2<<-NULL
  # try( v2 <<- sv$siteVisit[[1]][['landform']][['landformElement']][[1]][['slope']][[1]][['Result']][['unit']])
  # if(!is.null(v2)){
  #   slopeUnit <- v2
  # }else{
  #   slopeUnit <- ''
  # }

  # # if(length(sv$siteVisit[[1]]$landform$landformElement[[1]]$slope[[1]])>0){
  #    if('unit' %in% names(sv$siteVisit[[1]]$landform$landformElement[[1]]$slope[[1]]$Result)){
  #    slopeUnit <- getElement(sv$siteVisit[[1]]$landform$landformElement[[1]]$slope[[1]]$Result, 'unit')
  #   # slopeUnit <- sv$siteVisit[[1]]$landform$landformElement[[1]]$slope[[1]]$Result$unit
  #  }else{
  #    slopeUnit =''
  #  }

  #slopeUnit <- sv$siteVisit[[1]]$landform$landformElement[[1]]$slope[[1]]$Result$unit
  #slopeUnit<-'Test'

  #alldf <- getSiteVisitVals(val=paste0(slopeVal, ' ', str_remove(slopeUnit, 'unit:')), att='S_SLOPE', alldf)




  #print(alldf)

  # alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]]$slope[[1]]$Result$unit, att='S_SLOPE_UNIT', alldf)
  #  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]]$slope[[1]]$usedProcedure, att='S_SLOPE_EVAL', alldf)

  #  landformPattern
  # alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformPattern[[1]]$ansisType, att='S_PATT_TYPE', alldf, schemaPath = '/SoilSite/data/siteVisit/landform/landformPattern/ansisType')



  #  Elevation
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landSurface$elevation$result$value, att='O_ELEVATION', alldf, schemaPath = '/SoilSite/data/siteVisit/landSurface/elevation/result/value')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landSurface$elevation$result$unit, att='O_ELEVATION_UNIT', alldf, schemaPath = '/SoilSite/data/siteVisit/landSurface/elevation/result/unit')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landSurface$elevation$usedProcedure, att='O_ELEVATION_EVAL', alldf, schemaPath = '/SoilSite/data/siteVisit/landSurface/elevation/usedProcedure')

  #  Rock Outcrop
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landSurface$outcrop[[1]]$abundance$result , att='RO_ABUN', alldf, schemaPath = '/SoilSite/data/siteVisit/landSurface/outcrop/abundance/result')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landSurface$outcrop[[1]]$lithology$result , att='RO_LITH', alldf, schemaPath = '/SoilSite/data/siteVisit/landSurface/outcrop/lithology/result')

  #  Runoff
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landSurface$runoff$result , att='O_RUNOFF', alldf, schemaPath = '/SoilSite/data/siteVisit/landSurface/runoff/result')

  #  Classification
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$classification[[1]]$result$value , att='O_CLASSIFICATION', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/classification/result/value')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$classification[[1]]$result$source , att='O_CLASSIFICATION_SRC', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/classification/result/source')

  #  Drainage
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$drainage$result , att='O_DRAINAGE', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/drainage/result')

  #  Permeability
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$permeability$result , att='O_PERMEABILITY', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/permeability/result')

  #  Surface Coarse Fragments
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$soilSurface$coarseFragments[[1]]$abundance$result , att='SCF_ABUN', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/soilSurface/coarseFragments/abundance/result')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$soilSurface$coarseFragments[[1]]$size$result  , att='SCF_SIZE', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/soilSurface/coarseFragments/size/result')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$soilSurface$coarseFragments[[1]]$lithology$result  , att='SCF_LITH', alldf, domain = 'C_LITHOLOGY', schemaPath = '/SoilSite/data/siteVisit/soilProfile/soilSurface/coarseFragments/lithology/result')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$soilSurface$coarseFragments[[1]]$shape$result  , att='SCF_SHAPE', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/soilSurface/coarseFragments/shape/result')

  #  Surface Condition
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$soilSurface$conditionWhenDry$result , att='SCON_STAT', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/soilSurface/conditionWhenDry/result')

  #  Substrate
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$substrate$lithology$result , att='O_SB_LITH', alldf, domain = 'C_LITHOLOGY', schemaPath = '/SoilSite/data/siteVisit/soilProfile/substrate/lithology/result')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$substrate$depth$result$value , att='O_SB_DEPTH', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/substrate/depth/result/value')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$substrate$depth$result$unit , att='O_SB_DEPTH_UNIT', alldf, schemaPath =  '/SoilSite/data/siteVisit/soilProfile/substrate/depth/result/value')

  return(alldf)

}


getSiteVisitVals <- function(val, att, alldf, domain=NULL, schemaPath='' ){


  # if(is.null(testObject(val))){
  #
  #   hrr <- data.frame(property=att, propType='SiteVisit', field=of, value='', desc='')
  #   alldf <- rbind(alldf, hrr)
  #   return(alldf)
  # }


  fld <- mps[mps$Property==att, ]
  of <- paste0(fld$Property)

  if(is.null(val)){
    hrr <- data.frame(property=att, propType='SiteVisit', schemaPath=schemaPath, value='', desc='')
    alldf <- rbind(alldf, hrr)
    return(alldf)
  }
  vv <- val



  repStr = fld$Replace
  if(repStr==''){
    v <- vv
  }else{
    v <- str_remove(vv, repStr)
  }
  if(length(v)==0){v=''}


  desc=''


  if(is.null(domain)){
    domain=paste0('C_', of)
  }else{
    domain=paste0(domain)
  }
  cds <- CodesTable[CodesTable$code_domain==domain,]

  dec <- cds[cds$code_value==v,]
  if(nrow(cds>0)){

    if(nrow(dec)==1){
      desc <- dec$code_desc
    }else{
      desc <- v
    }
  }else{
    desc <- v
  }

  hrr <- data.frame(property=att, propType='SiteVisit', schemaPath=schemaPath,  value=v, desc=desc)
  alldf <- rbind(alldf, hrr)

  return(alldf)

}



getSlope <- function(sv){

  tryCatch(
    {
      v <- sv$siteVisit[[1]]$landform$landformElement[[1]]$slope[[1]]$Result$value
      return(v)},

    error = function(msg){
      return('')
    }
  )
}

getSlopeUnit <- function(sv){

  tryCatch(
    {
      v <- sv$siteVisit[[1]]$landform$landformElement[[1]]$slope[[1]]$Result$unit
      v <- str_remove(v, 'unit:')
      return(v)},

    error = function(msg){
      return('')
    }
  )
}


getSiteLocation <- function(siteAsList){

  srid <- siteAsList$geometry[[1]]
  bits <- str_split(srid, '[(]')
  bits2 <- str_remove(bits[[1]][2], '[)]')
  bits3 <- str_split(bits2, ' ')[[1]]
  ol <- list()
  ol$X <- as.numeric(bits3[1])
  ol$Y <- as.numeric(bits3[2])
  return(ol)
}

makeSitesLocationTableFromDataList <- function(dl){

  locDF <- data.frame(sid=character(), X=numeric(), Y=numeric())

  for (i in 1:length(dl)) {
    sid <- dl[[i]]$Site
    x <- dl[[i]]$X
    y <- dl[[i]]$Y
    df <-  data.frame(sid=sid, X=x, Y=y)
    locDF <- rbind(locDF, df)
  }
  return(locDF)
}










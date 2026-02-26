# 
# 
# 


normaliseSite <- function(s){
  #s <- r$data[[i]]
  sid <- getSiteID(siteAsList=s)
  
  layersTable <- parseANSISSiteLayersToDenormalisedTable(siteAsList=s)
  siteVistTable <- parseANSISSiteVistToDenormalisedTable(siteAsList=s)
  
  loc <- getSiteLocation(siteAsList=s)
  pl <- list()
  pl$Site=sid
  pl$X=loc$X
  pl$Y=loc$Y
  pl$data <-  layersTable
  pl$siteVisitTable <- siteVistTable
  return(pl)
}


getSiteID <- function(siteAsList, projects){

  scopeID <- siteAsList$scopedIdentifier[[1]]$value
  relproj <- siteAsList$relatedProject
  
  pids <- sapply(projects, function (x) x[['id']])
  idx <- which(pids==relproj)
  proj <- projects[[idx]]$label
  
  if(is.null(scopeID)){
    sid <- paste0( proj, '+', siteAsList$id)
  }else{
    auth <- siteAsList$scopedIdentifier[[1]]$authority
    sid <- paste0(auth, '+', proj, '+', scopeID)
  }
  return(sid)
}
# 
# 
# 
dumpCSV <- function(anisObject){
  makeAllDataCSV(allsites=anisObject$dfDenorm)
}
# 
# 
makeAllDataCSV <- function(allsites){
  # tic()
  alldf <- data.frame()
  for (i in 1:length(allsites)) {
    s <- allsites[[i]]
    sitedf<- makeSiteCSV(sl=s)
    alldf <- rbind(alldf, sitedf)
  }
  # toc()
  

  sites <- pbapply::pbsapply(allsites, makeSiteCSV, simplify = F)
 # sites <- sapply(allsites, makeSiteCSV, simplify = F)
  alldf <- dplyr::bind_rows(sites)

  return(alldf)
}
# 
makeSiteCSV <- function(sl){

  sv<-sl
  odf <- data.frame()

  sid <- sv$Site
  dt <- sv$Date
  bits <- stringr::str_split(dt, 'T')
  thedate <- bits[[1]][1]
  svdf <- data.frame(ud='', ld='',  property='SiteVisit', propType='SiteVisit',field=sv$siteVisitTable$property, value=sv$siteVisitTable$value, desc=sv$siteVisitTable$desc)
  odf <- rbind(odf, svdf)
  hors <- sv$data
  #hors <- data.frame(ud = hors$ud, ld= hors$ld,Date=dt, hors[,3:ncol(hors)]  )
  
  odf <- rbind(odf, hors)
  sitedf <- data.frame(site=sid, Longitude=sv$X, Latitude=sv$Y, Date=thedate, odf)
  colnames(sitedf) <- c("Site", "Longitude", "Latitude", "Date", "UpperDepth", "LowerDepth",  "Group", "PropertyType", "Property", "Value", "Description" )
  return(sitedf)
}





parseANSISSiteLayersToDenormalisedTable <- function(siteAsList){

  
 # mps <- DataSets@mps
 # CodesTable <- DataSets@CodesTable
  mp <- unique(mps[mps$Domain!='' & mps$SchemaLocation=='Horizons', ]$Domain )

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
# 
# 
isLabProperty <- function(layer, prop){

  tryCatch({
    if(length(layer[[prop]][[1]]$usedProcedure) > -1 & !is.null(layer[[prop]][[1]]$usedProcedure))
      return(T)
  }, error = function(e) {
    return(F)
  })
  return(F)

}
# 
# 
getMorphVals <- function(layer, att, mps, ud, ld, alldf){

  l=layer
  
  # CodesTable <- DataSets@CodesTable

  schRoot <- '/SoilSite/data/siteVisit/soilProfile/soilLayer/'

  flds <- mps[mps$Domain==att & mps$Fields!='', ]

  for (i in 1:nrow(flds)) {

    fld = stringr::str_split(flds[i, ]$Fields, ':')[[1]]
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
      v <- stringr::str_remove(vv, repStr)
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
# 
getLabVals <- function(layer, prop, mps, ud, ld, alldf){

  desc=''
  l=layer
  up <- l[[prop]][[1]]$usedProcedure

  p <- stringr::str_remove(up, 'scm:')
  v <- l[[prop]][[1]]$result$value
  u <- l[[prop]][[1]]$result$unit
  r <- data.frame(ud=ud, ld=ld, property='LabResults', propType='Lab', field=p, value=v, desc=desc)
  alldf <- rbind(alldf, r)

  return(alldf)
}
# 
# 
parseANSISSiteVistToDenormalisedTable <- function(siteAsList){

  
  #mps <- DataSets@mps
  sv <- siteAsList

  alldf <- data.frame(property=character(), propType=character(), schemaPath=character(), value=character(), desc=character() )

  alldf <- getSiteVisitVals(val=sv$scopedIdentifier[[1]]$value, att='S_ID', alldf, schemaPath='/SoilSite/data/scopedIdentifier/value', mps=mps)
  alldf <- getSiteVisitVals(val=sv$scopedIdentifier[[1]]$authority, att='AGENCY_CODE', alldf, schemaPath='/SoilSite/data/scopedIdentifier/authority', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$usedProcedure, att='O_TYPE', alldf, schemaPath='/SoilSite/data/siteVisit/soilProfile/usedProcedure', mps=mps)

  dt <-  sv$siteVisit[[1]]$startedAtTime

  ###  not sure if it is a required field and needs a bit of massaging
  if(!is.null(dt)){
    sDate <- stringr::str_split(dt, 'T')[[1]][1]
  }else{
    sDate <- ''
  }
  hrr <- data.frame(property='S_DATE_DESC', propType='SiteVisit', schemaPath='/SoilSite/data/siteVisit/endedAtTime', value=sDate, desc=sDate)
  alldf <- rbind(alldf, hrr)



  #  disturbance
  alldf <- getSiteVisitVals(val=sv$disturbance[[1]]$result, att='O_SOIL_DISTURB', alldf, schemaPath = '/SoilSite/data/disturbance/result', mps=mps)

  #  landformElement
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]]$ansisType, att='S_ELEM_TYPE', alldf, schemaPath='/SoilSite/data/siteVisit/landform/landformElement/ansisType', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]]$height$result$value, att='S_ELEM_HEIGHT', alldf, schemaPath = '/SoilSite/data/siteVisit/landform/landformElement/height/result/value', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]]$height$result$unit, att='S_ELEM_HEIGHT_UNIT', alldf, schemaPath = '/SoilSite/data/siteVisit/landform/landformElement/height/result/unit', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]]$morphologicalType$result, att='S_MORPH_TYPE', alldf, schemaPath = '/SoilSite/data/siteVisit/landform/landformElement/morphologicalType/result', mps=mps)

  # v<-NULL

  ##### This can probably be replaced by the standard getSiteVisitVals function with the try ctach added
  slopeVal <- getSlope(sv)
  slopeUnit <- getSlopeUnit(sv)

  alldf <- getSiteVisitVals(val=paste0(slopeVal, ' ', slopeUnit), att='S_SLOPE', alldf, mps=mps)

  #  Elevation
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landSurface$elevation$result$value, att='O_ELEVATION', alldf, schemaPath = '/SoilSite/data/siteVisit/landSurface/elevation/result/value', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landSurface$elevation$result$unit, att='O_ELEVATION_UNIT', alldf, schemaPath = '/SoilSite/data/siteVisit/landSurface/elevation/result/unit', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landSurface$elevation$usedProcedure, att='O_ELEVATION_EVAL', alldf, schemaPath = '/SoilSite/data/siteVisit/landSurface/elevation/usedProcedure', mps=mps)

  #  Rock Outcrop
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landSurface$outcrop[[1]]$abundance$result , att='RO_ABUN', alldf, schemaPath = '/SoilSite/data/siteVisit/landSurface/outcrop/abundance/result', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landSurface$outcrop[[1]]$lithology$result , att='RO_LITH', alldf, schemaPath = '/SoilSite/data/siteVisit/landSurface/outcrop/lithology/result', mps=mps)

  #  Runoff
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$landSurface$runoff$result , att='O_RUNOFF', alldf, schemaPath = '/SoilSite/data/siteVisit/landSurface/runoff/result', mps=mps)

  #  Classification
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$classification[[1]]$result$value , att='O_CLASSIFICATION', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/classification/result/value', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$classification[[1]]$result$source , att='O_CLASSIFICATION_SRC', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/classification/result/source', mps=mps)

  #  Drainage
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$drainage$result , att='O_DRAINAGE', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/drainage/result', mps=mps)

  #  Permeability
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$permeability$result , att='O_PERMEABILITY', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/permeability/result', mps=mps)

  #  Surface Coarse Fragments
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$soilSurface$coarseFragments[[1]]$abundance$result , att='SCF_ABUN', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/soilSurface/coarseFragments/abundance/result', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$soilSurface$coarseFragments[[1]]$size$result  , att='SCF_SIZE', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/soilSurface/coarseFragments/size/result', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$soilSurface$coarseFragments[[1]]$lithology$result  , att='SCF_LITH', alldf, domain = 'C_LITHOLOGY', schemaPath = '/SoilSite/data/siteVisit/soilProfile/soilSurface/coarseFragments/lithology/result', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$soilSurface$coarseFragments[[1]]$shape$result  , att='SCF_SHAPE', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/soilSurface/coarseFragments/shape/result', mps=mps)

  #  Surface Condition
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$soilSurface$conditionWhenDry$result , att='SCON_STAT', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/soilSurface/conditionWhenDry/result', mps=mps)

  #  Substrate
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$substrate$lithology$result , att='O_SB_LITH', alldf, domain = 'C_LITHOLOGY', schemaPath = '/SoilSite/data/siteVisit/soilProfile/substrate/lithology/result', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$substrate$depth$result$value , att='O_SB_DEPTH', alldf, schemaPath = '/SoilSite/data/siteVisit/soilProfile/substrate/depth/result/value', mps=mps)
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$substrate$depth$result$unit , att='O_SB_DEPTH_UNIT', alldf, schemaPath =  '/SoilSite/data/siteVisit/soilProfile/substrate/depth/result/value', mps=mps)

  return(alldf)

}


getSiteVisitVals <- function(val, att, alldf, domain=NULL, schemaPath='', mps ){

 # CodesTable <- DataSets@CodesTable

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
    v <- stringr::str_remove(vv, repStr)
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
      v <- stringr::str_remove(v, 'unit:')
      return(v)},

    error = function(msg){
      return('')
    }
  )
}


getSiteLocation <- function(siteAsList){

  srid <- siteAsList$geometry[[1]]
  bits <- stringr::str_split(srid, '[(]')
  bits2 <- stringr::str_remove(bits[[1]][2], '[)]')
  bits3 <- stringr::str_split(bits2, ' ')[[1]]
  ol <- list()
  ol$X <- as.numeric(bits3[1])
  ol$Y <- as.numeric(bits3[2])
  return(ol)
}

getSiteDate <- function(siteAsList){
  return(siteAsList$siteVisit[[1]]$startedAtTime)
}


getSiteLocation2 <- function(dl){
  sid <- dl$Site
  x <- dl$X
  y <- dl$Y
  df <-  data.frame(sid=sid, X=x, Y=y)
  return(df)
}
 
makeSitesLocationTableFromDataList <- function(dl){

  # locDF <- data.frame(sid=character(), X=numeric(), Y=numeric())
  # 
  # for (i in 1:length(dl)) {
  #   sid <- dl[[i]]$Site
  #   x <- dl[[i]]$X
  #   y <- dl[[i]]$Y
  #   df <-  data.frame(sid=sid, X=x, Y=y)
  #   locDF <- rbind(locDF, df)
  # }
  
  sites <- pbapply::pbsapply(dl, getSiteLocation2, simplify = F)
  locDF <- dplyr::bind_rows(sites)
  
  return(locDF)
}


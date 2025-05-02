library(stringr)

generateSiteDescription <- function(ado){

  layersTable = ado$dfDenorm[[1]]$data
  siteVisitTable <- ado$dfDenorm[[1]]$siteVisitTable

#  if(!is.null(dfDenorm)){

   st <- generateSiteTableHTML(siteVisitTable=siteVisitTable)
   lt <- generateLabTableHTML(layersTable)
   ht <- generateHorizonsTableHTML(layersTable)

   html <- paste0('<html><body>', st, ht, lt, '</body></html>')
   return(html)

#  }
}

generateSiteTableHTML <- function(siteVisitTable){


  sv <- siteVisitTable
    t <- '<H3>Site Visit</H3> <table style="width: 100%;">'

    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Site Name : ', fields1 ='S_ID', title2 = 'Organisation : ', fields2 ='AGENCY_CODE'))
    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Date Described : ', fields1 ='S_DATE_DESC', title2 = 'Observation Type : ', fields2 ='O_TYPE'))
    #t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Landform Pattern : ', fields1 ='S_PATT_TYPE', title2 = 'Landform Element : ', fields2 ='S_ELEM_TYPE'))
    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Slope : ', fields1 ='S_SLOPE', title2 = 'Landform Element : ', fields2 ='S_ELEM_TYPE'))
    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Element Height : ', fields1 ='S_ELEM_HEIGHT;S_ELEM_HEIGHT_UNIT', title2 = 'Morphological Type : ', fields2 ='S_MORPH_TYPE'))
    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Site Height : ', fields1 ='O_ELEVATION;O_ELEVATION_UNIT;O_ELEVATION_EVAL', title2 = 'Runoff : ', fields2 ='O_RUNOFF'))
    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Rock Outcrop : ', fields1 ='RO_ABUN;RO_LITH', title2 = 'Substrate : ', fields2 ='O_SB_LITH;O_SB_DEPTH;O_SB_DEPTH_UNIT'))
    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Surface Coarse Frags : ', fields1 ='SCF_ABUN;SCF_SIZE;SCF_SHAPE;SCF_LITH', title2 = 'Soil Classification : ', fields2 ='O_CLASSIFICATION'))

    t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Surface Condition : ', fields1 ='SCON_STAT', title2 = 'Soil Disturbance : ', fields2 ='O_SOIL_DISTURB'))


    # t <- paste0(t, getSVHTMLText(sv = sv, title1 = 'Substrate', fields1 ='O_SB_LITH;O_SB_DEPTH;O_SB_DEPTH_UNIT', title2 = '', fields2 =''))


    t <- paste0(t, '</table>')

    return(t)

}

generateLabTableHTML <- function(layersTable){

      labData <- layersTable[layersTable$propType=='Lab', ]
      if(nrow(labData)==0)
      {
        labsTable=''
      }else{
        bt <- generateBlankTable(dfDenorm=labData)
        labs <- populateTable(blankTable=bt, dfDenorm=labData, decode=F)
        labsTable <- labs
      }
     html <-  print(xtable::xtable(labsTable), type="html", file="example.html")
      return(html)
}


generateHorizonsTableHTML <- function(layersTable){

  indf <- layersTable[layersTable$propType=='Horizons', ]

      bt <- generateBlankTable(dfDenorm=indf)
      hs <- populateTable(blankTable=bt, dfDenorm=indf, decode=T)


      t <- '<H3>Horizons</H3><table>'
      for (i in 1:nrow(hs)) {

        rec <- hs[i,]

        hname <- getHTMLText(rec, 'H_DESIG_MASTER', bold=T)
        tex <- getHTMLText(rec, 'H_TEXTURE', suffix =';')
        col <- getHTMLText(rec, 'COL_HUE_VAL_CHROM', suffix =';')

        str_grade <- getHTMLText(rec, 'STR_PED_GRADE', suffix =' ')
        str_size <- getHTMLText(rec, 'STR_PED_SIZE', suffix =' ')
        str_type <- getHTMLText(rec, 'H_TEXSTR_PED_TYPETURE', suffix ='; ')
        hstructure <- paste0(' ', str_grade,  str_size, str_type)
        if(hstructure !=' '){hstructure <- paste0(hstructure, 'structure; ')}

        cf_abun <- getHTMLText(rec, 'CF_ABUN', suffix =' ')
        cf_size <- getHTMLText(rec, 'CF_SIZE', suffix =' ')
        cf_shape <- getHTMLText(rec, 'CF_SHAPE', suffix =' ')
        cf_lith <- getHTMLText(rec, 'CF_LITH', suffix =' ')
        cfs <- paste0(' ', cf_abun,  cf_size, cf_shape, cf_lith)
        if(cfs!=' '){cfs <- paste0(cfs, 'coarse fragments; ')}

        SEG_ABUN <- getHTMLText(rec, 'SEG_ABUN', suffix =' ')
        SEG_NATURE <- getHTMLText(rec, 'SEG_NATURE', suffix =' ')
        SEG_FORM <- getHTMLText(rec, 'SEG_FORM', suffix =' ')
        SEG_SIZE <- getHTMLText(rec, 'SEG_SIZE', suffix =' ')
        SEG_STRENGTH <- getHTMLText(rec, 'SEG_STRENGTH', suffix =' ')
        segs <- paste0(' ', SEG_ABUN,  SEG_NATURE, SEG_FORM, SEG_SIZE, SEG_STRENGTH)
        if(segs!=' '){segs <- paste0(segs, 'segregations; ')}

        PORE_ABUN <- getHTMLText(rec, 'PORE_ABUN', suffix =' ')
        PORE_DIAMETER <- getHTMLText(rec, 'PORE_DIAMETER', suffix =' ')
        pores<- paste0(' ', PORE_ABUN,  PORE_DIAMETER)
        if(pores!=' '){pores <- paste0(pores, 'pores; ')}

        ROOT_ABUN <- getHTMLText(rec, 'ROOT_ABUN', suffix =' ')
        ROOT_SIZE <- getHTMLText(rec, 'ROOT_SIZE', suffix =' ')
        roots<- paste0(' ', ROOT_ABUN,  ROOT_SIZE)
        if(roots!=' '){roots <- paste0(roots, 'roots; ')}

        STRG_CLASS <- getHTMLText(rec, 'STRG_CLASS', suffix =' ')
        strength <- paste0(' ', STRG_CLASS)
        if(strength!=' '){strength <- paste0(strength, 'consistence; ')}

        MOTT_ABUN <- getHTMLText(rec, 'MOTT_ABUN', suffix =' ')
        MOTT_SIZE <- getHTMLText(rec, 'MOTT_SIZE', suffix =' ')
        MOTT_CONTRAST <- getHTMLText(rec, 'MOTT_CONTRAST', suffix =' ')
        MOTT_COLOUR <- getHTMLText(rec, 'MOTT_COLOUR', suffix =' ')
        motts <- paste0(' ', MOTT_ABUN,  MOTT_SIZE, MOTT_CONTRAST, MOTT_COLOUR)
        if(motts!=' '){motts <- paste0(motts, 'mottles; ')}


        #desc <- paste0(hname, ' ', col, colcode, tex, hstructure, ph, ec, ocf, oseg, omot, bdy)
        desc <- paste0(hname, ' : ',  tex, ' ', col, strength, hstructure, cfs, motts, segs, pores, roots )
        td <- '<td style="padding: 8px;">'
        t <- paste0(t, '<tr>', td, rec['ud'], '</td>', td, ' to </td>', td, rec['ld'], '</td><td>', desc, '</td></tr>' )

      }
      t <- paste0(t, '</table>')

     return(t)


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

makeSitesLocationTable <- function(respAsList){

  locDF <- data.frame(sid=character(), X=numeric(), Y=numeric())

 for (i in 1:length(dl)) {
    sid <- getSiteID(respAsList$data[[i]])
    loc <- getSiteLocation(respAsList$data[[i]])
    df <-  data.frame(sid=sid, X=loc$X, Y=loc$Y)
    locDF <- rbind(locDF, df)
 }
  return(locDF)
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

# makeSitesLocationTableFromDataList <- function(dl){
#
#   locDF <- data.frame(sid=character(), X=numeric(), Y=numeric())
#
#   for (i in 1:length(dl)) {
#     sid <- dl[[i]]$Site
#     x <- dl[[i]]$X
#     y <- dl[[i]]$Y
#     df <-  data.frame(sid=sid, X=x, Y=y)
#     locDF <- rbind(locDF, df)
#   }
#   return(locDF)
# }


parseSites <- function(respAsList){

  mps <- read.csv('C:/Projects/ANSIS/ANSISAPI/schemaFieldMapping2.csv')
  mp <- unique(mps[mps$SchemaLocation=='Horizons' & mps$Domain!='',]$Domain)

  r <- respAsList

  sol <- list()
  for (k in  1:length(r$data)) {
    print(k)
    s <- r$data[[k]]
    sid <- getSiteID(siteAsList=s)

    siteTable <- parseANSISSiteToDenormalisedTable(siteAsList=s)

    loc <- getSiteLocation(siteAsList=s)
    pl <- list()
    pl$Site=sid
    pl$X=loc$X
    pl$Y=loc$Y
    pl$data <-  siteTable

    sol[[sid]] <- pl

  }

  return(sol)
}






parseANSISSiteLayersToDenormalisedTable <- function(siteAsList){

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


#getLabVals(layer=l, prop='electricalConductivity', mps, ud=1, ld=2, alldf)

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


isLabProperty <- function(layer, prop){

  tryCatch({
    if(length(layer[[prop]][[1]]$usedProcedure) > -1 & !is.null(layer[[prop]][[1]]$usedProcedure))
      return(T)
  }, error = function(e) {
    return(F)
  })
  return(F)

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



parseANSISSiteVistToDenormalisedTable <- function(siteAsList){

  sv <- siteAsList

  alldf <- data.frame(property=character(), propType=character(), schemaPath=character(), value=character(), desc=character() )

  alldf <- getSiteVisitVals(val=sv$scopedIdentifier[[1]]$value, att='S_ID', alldf, schemaPath='/SoilSite/data/scopedIdentifier/value')
  alldf <- getSiteVisitVals(val=sv$scopedIdentifier[[1]]$authority, att='AGENCY_CODE', alldf, schemaPath='/SoilSite/data/scopedIdentifier/authority')
  alldf <- getSiteVisitVals(val=sv$siteVisit[[1]]$soilProfile[[1]]$usedProcedure, att='O_TYPE', alldf, schemaPath='/SoilSite/data/siteVisit/soilProfile/usedProcedure')

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

# getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]]$slope[[1]]$Result$value, att='S_SLOPE', alldf)


testObject <- function(val){
  tryCatch(
    #try to do this
    {
      #r <- x/y
      p=val
    },
    #if an error occurs, tell me the error
    error=function(e) {
      # message('An Error Occurred')
      return(NULL)
    },
    #if a warning occurs, tell me the warning
    warning=function(w) {
      # message('A Warning Occurred')
      return(NULL)
    }
    # , finally = {
    #   return(NULL)}
  )
  return(val)
}

# getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]][['morphologicalType']][['result']], att='S_MORPH_TYPE', alldf)
# getSiteVisitVals(val=sv$siteVisit[[1]]$landform$landformElement[[1]][['slope']][[1]][['Result']][['value']], att='S_SLOPE', alldf)

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


getMorphVals <- function(layer, att, mps, ud, ld, alldf){

  l=layer

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

generateBlankTable <- function(dfDenorm){

  cols <- unique(dfDenorm$field)

  depths <- unique(dfDenorm[c('ud', 'ld')])
  df <- data.frame(ud=depths$ud, ld=depths$ld)
  df <- df[with(df, order(ud, ld)), ]

  for (i in 1:length(cols)) {
    c <- cols[i]
    df[c] <- rep('', nrow(df))
  }
  return(df)
}

populateTable <- function(blankTable, dfDenorm, decode=F){

  bt <- blankTable
  nt <- dfDenorm
  cols <- unique(nt$field)

  for (i in 1:nrow(bt)) {
    rec <- bt[i, ]
    ud <- rec$ud
    ld <- rec$ld
    for (j in 1:length(cols)) {
      att <- cols[j]

      if(decode){
        v <- nt[nt$ud==ud & nt$ld==ld & nt$field==att, ]$desc
      }else{
        v <- nt[nt$ud==ud & nt$ld==ld & nt$field==att, ]$value
      }

      vc <- paste(v, sep = " ", collapse = '; ')
      bt[i, ][att] <- vc
    }

  }
  return(bt)
}


getHTMLText <- function(rec, att, bold=F, suffix=''){

  t <- ''
  hbs <- ''
  hbe <- ''

  if(bold){
    hbs <- '<B>'
    hbe <- '</B>'
  }
  if(att %in% colnames(rec))  {

    if(rec[att] == ''){
      t=''
    }else{
      t=paste0(hbs, rec[att], hbe, suffix )
    }
  }
}


getSVHTMLText <- function(sv, title1, title2, fields1, fields2){

  pad='2'
  #b <-' border: 1px solid black;'
  b <-''
  tcWidth='20'
  vcWidth = '20'

  vs1 <- stringr::str_split(fields1,';')[[1]]
  ov1 <- ''
  for (i in 1:length(vs1)) {
    rec <- sv[sv$property==vs1[i], ]
    ov1 <- paste0(ov1, " ", rec$desc)
  }

  vs2 <- stringr::str_split(fields2,';')[[1]]
  ov2 <- ''
  for (i in 1:length(vs2)) {
    rec <- sv[sv$property==vs2[i], ]
    ov2 <- paste0(ov2, " ", rec$desc)
  }

  t <- ''
  t <- paste0(t, '<tr><td style="padding: ', pad, 'px; text-align: left; width: ', tcWidth, '%; ',b, '"><B>', title1, '</B></td><td style="padding: ', pad, 'px; text-align: left; width: ', vcWidth, '%;">', ov1, '</td>
                      <td style="width: 5%"></td>
                      <td style="padding: ', pad, 'px; text-align: left;  width: ', tcWidth, '%; ',b, '"><B>', title2, '</B></td style="padding: ', pad, 'px; text-align: left;  width: ', vcWidth, '%;"><td>', ov2, '</td>')
  return(t)
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


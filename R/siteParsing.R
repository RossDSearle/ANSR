# library(stringr)
#
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
#
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
#
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

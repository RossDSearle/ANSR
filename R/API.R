################################################################# #
#####       Author : Ross Searle                              ###
#####       Date :  Fri May  2 09:29:32 2025                  ###
#####       Purpose : Interact with the ANSIS Web API         ###
#####       Comments :                                        ###
################################################################# #


#' Authorise ANSIS Session
#' @param username ANSIS account username
#' @param password ANSIS account password
#' @examples AuthoriseMe(username='Bob', password='123')
#' @details  Before you can use this package you need to provide your ANSIS account credentials. If you do not already have a valid ANSIS user account please go to the ANSI website to creat an account.
#' @author Ross Searle
#' @return logical
#' @export
#'
#'
apiAuthoriseMe <- function(username, password){
  
  tkn <-apiGenerateToken(username, password)
  
  if(is.null(tkn)){
    cat(paste0('\nThese are not valid ANSIS account credentials.\nPlease check your username and password supplied.\n\n',
               'If you do not already have a valid ANSIS user account please go to ', constants$ANSISguiURL, '\n\n' ))
   # return(F)
  }else{
    
    authANSIS <<- list()
    authANSIS$usr <<- username
    authANSIS$pwd <<- password
    authANSIS$Authorised <<- T
    
    
    # pkg.env <- new.env()
    # pkg.env$usr <- username
    # pkg.env$pwd <- password
    # pkg.env$Authorised <- T
    
    cat(paste0('Authorisation successful\n\n' ))
  }
}


#' Generate an Authorisation token
#' @param user ANSIS account username
#' @param pwd ANSIS account password
#' @details  The ANSI API requires users to authenticate with a token.
#' @author Ross Searle
#' @return logical
#' @export
#'
#'

apiGenerateToken <- function(user, pwd, verbose=F){
  
  if(verbose){cat('Authorising user....\n')}
  
  url <- 'https://b2cansishrmtestae.b2clogin.com/b2cansishrmtestae.onmicrosoft.com/B2C_1A_ROPC_AUTH/oauth2/v2.0/token?grant_type=password'
  bdyL <- list()
  bdyL$username = user
  bdyL$password = pwd
  bdyL$scope = 'openid 44040d13-769e-4567-8bbe-b2109b42c939'
  bdyL$client_id = '44040d13-769e-4567-8bbe-b2109b42c939'
  bdyL$response_id = 'token id_token'
  
  resp <- httr::POST(url=url, body=bdyL, encode='form')
  jsn <- httr::content(resp, 'text', encoding = 'UTF8')
  auth = jsonlite::fromJSON(jsn)
  
  return(auth$access_token)
}

apiCatalogueSummary  <- function(){
  
  url <- paste0('https://apim-ansis-hrm-test-ae.azure-api.net/site-catalogue/v1/catalogue-summary')
  resp <- httr::GET(url=url)
  jsn <- httr::content(resp, 'text', encoding = 'UTF8')
  smry <- jsonlite::fromJSON(jsn, simplifyDataFrame = T)
  return(smry)
}


apiProviderCatalogue   <- function(poviderNames=NULL, includeProperties=F){
  
  if(is.null(poviderNames)){
    prvs <- apiCatalogueSummary()
    poviderNames <- prvs$name
  }
  
  cdf <- data.frame()
  
    for (i in 1:length(poviderNames)) {
        
      p <- poviderNames[i]
      cat(paste0('Getting catalogue information for ', p, '....\n'))
      
      url <- paste0('https://apim-ansis-hrm-test-ae.azure-api.net/site-catalogue/v1/provider/', p)
      resp <- httr::GET(url=url)
      jsn <- httr::content(resp, 'text', encoding = 'UTF8')
      jdf <- jsonlite::fromJSON(jsn)
      
      if(!is.null(nrow(jdf))){
        
          n<-nrow(jdf)
          odf <- data.frame(Provider=character(n),Project=character(n), SiteID=character(n), Restricted=logical(n), Cached=logical(n)
                            , StartYear=numeric(n), EndYear=numeric(n), Longitude=numeric(n), Latitude=numeric(n))
          yrs <- jdf$properties$years
          sy <- sapply(yrs, function (x) x[1])
          ey <-sapply(yrs, function (x) x[2])
          locs <- jdf$geometry$coordinates
          lat <- sapply(locs, function (x) x[2])
          lng <- sapply(locs, function (x) x[1])
          
          props <- jdf$properties$properties
        
          odf$Provider <- jdf$properties$provider
          odf$Project <- jdf$properties$project
          odf$SiteID <- jdf$properties$site
          odf$Restricted <- jdf$properties$isRestricted
          odf$Cached <- jdf$properties$isHydrated
          odf$StartYear <- sy
          odf$EndYear <- ey
          odf$Longitude <- lng
          odf$Latitude <- lat
          
          if(includeProperties){
            ps <- sapply(props, function (x) paste0(x, collapse = ', '))
            odf$Properties <- ps
          }
      
      cdf <- rbind(cdf, odf)
      }
      
    }
  return(cdf)
}



#' ANSIS Property Definitions
#' 
#' Returns a data frame of ANSIS property codes and their definitions
#' @examples head(apiPropertyDefinitions())
#' @author Ross Searle
#' @return data frame
#' @export
#'
#'

apiPropertyDefinitions <- function(){
  
  resp <- GET('https://apim-ansis-hrm-test-ae.azure-api.net/site-catalogue/v1/definitions')
  resp
  jsn <- content(resp, 'text', encoding = 'UTF8')
  
  defs <- fromJSON(jsn)
  # defs[grepl("^0-0", names(defs))]
  # defs[grepl("^3-2", names(defs))]
  
  odf<- data.frame()
  
  for (i in 0:4) {
    print(i)
    n1 <- defs[as.character(i)]
    propType=n1[[1]]$name
    # need to deal with n2 null
    for (j in 1:length(n1[[1]]$members)) {
      print(j) 
      n2 <- defs[n1[[1]]$members[j]]
      prop = n2[[1]]$name
      
      if(!is.null(n2[[1]]$members)){
        for (k in 1:length(n2[[1]]$members)) {
          n3 <- defs[n2[[1]]$members[k]]
          
          df<- data.frame('PropType'=propType, 'Property'=prop, 'Name'=n3[[1]]$name, 'ANSISCode'=n2[[1]]$members[k] )
          odf <- rbind(odf, df)
        }
      }
    }
  }
 return(odf)
}





apiCreateQuery <- function(qryJSON){
  
  if(!checkIfAuthorised()){return(cat(''))}
  tkn <- apiGenerateToken(user = authANSIS$usr, pwd=authANSIS$pwd)
  
  cat('Sending query to the ANSIS server....\n')
  
  resp <- httr::POST(url='https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/create-query-request',
               body=qryJSON,
               httr::add_headers(Authorization = paste0("Bearer ", tkn)))
  jsn <- httr::content(resp, 'text', encoding = 'UTF8')
  reqID <- stringr::str_split(jsn, ': ')[[1]][2]
  return(reqID)
}



apiMonitorQueryStatus <- function(reqID){
  
  if(!checkIfAuthorised()){return(cat(''))}
  tkn <- apiGenerateToken(user = authANSIS$usr, pwd=authANSIS$pwd)
  
    inProgress=T  
    pb <- c('\\', '|', '/', '-')
    iter = 1

    while (inProgress) {
      Sys.sleep(1)
      cat(paste0('\rQuerying ANSIS Server  ', pb[iter] ))   
      iter = iter+1
        if(iter > 4){
          iter=1
          
          resp <- httr::GET(url=paste0('https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/get-query-request-status?requestId=', reqID),
                            httr::add_headers(Authorization = paste0("Bearer ", tkn)))
          jsn <- httr::content(resp, 'text', encoding = 'UTF8')
          status <- jsonlite::fromJSON(content(resp, 'text', encoding = 'UTF8'), simplifyDataFrame = F)
          print(length(status$sdrRequest$files))

          if(status$sdrRequest$status=='Completed'){
            cat('\n\nQuery completed...\n\n')
            inProgress=F
          }
        }
    }
    return(status$sdrRequest$files)
}

apiAllQueryStatus <- function(statusTypes=NULL){
  
  if(!checkIfAuthorised()){return(cat(''))}
  tkn <- apiGenerateToken(user = authANSIS$usr, pwd=authANSIS$pwd)
  
  stats <- c('Processing', 'Completed', 'Queued', 'Error')
  
  stat <- paste0('https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/get-all-query-requests-statuses')
  resp <- httr::GET(url=stat,  httr::add_headers(Authorization = paste0("Bearer ", tkn)))
  jsn <- httr::content(resp, 'text', encoding = 'UTF8')
  rq <- jsonlite::fromJSON(jsn, simplifyDataFrame = T, simplifyVector = T)
  
  df <- data.frame(id= character(nrow(rq)), status=character(nrow(rq)))
  for (i in 1:nrow(rq)) {
    rec <- rq[i,]
    df[i,1] <- rec$id
    df[i,2] <- rec$sdrRequest$status
  }
  
  if(!is.null(statusTypes)){
    
    if((statusTypes %in% stats)){
      idxs <- which(df$status %in% statusTypes)
      odf <- df[idxs,]
      return(odf)
    }else{
      cat(paste0('Valid status filters are (', paste0(stats, collapse = ', ' ), ')\n\n'))
      return(df)
    }
  }else{
    return(df)
  }
}


apiSingleQueryStatus <- function(reqID){
  
  if(!checkIfAuthorised()){return(cat(''))}
  tkn <- apiGenerateToken(user = authANSIS$usr, pwd=authANSIS$pwd)
  
  stat <- paste0('https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/get-query-request-status?requestId=', reqID)
  resp <- httr::GET(url=stat, httr::add_headers(Authorization = paste0("Bearer ", tkn)))
  jsn <- httr::content(resp, 'text', encoding = 'UTF8')
  rq <- jsonlite::fromJSON(jsn, simplifyDataFrame = T, simplifyVector = T)
  
  return(rq$sdrRequest$status)
}




apiDownloadResponse <- function(fls){
  
  if(!checkIfAuthorised()){return(cat(''))}
  tkn <- apiGenerateToken(user = authANSIS$usr, pwd=authANSIS$pwd)
  
  cat('Downloading data from ANSIS server....\n')
  
  outDir <- paste0(tempdir(), '/tmp_', as.numeric(Sys.time()))
  print(outDir)
  if(!dir.exists(outDir)){ dir.create(outDir, recursive = T)}
  files <- vector(mode = 'character', length = length(fls))
  for (i in 1:length(fls)) {
    resp <- httr::GET(url=paste0('https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/download-response?fileId=', fls[i] ),
                      httr::add_headers(Authorization = paste0("Bearer ", tkn)))
    jsn <- httr::content(resp, 'text', encoding = 'UTF8')
    files[[i]] <- jsn
    fn <- paste0(outDir, '/res_', i, '.json')
    cat(jsn, file = fn)
  }
  
 jl <-  mergeResponseFiles(outDir)
 
 return(jl)
}

apiRetrieveData <- function(reqID){

  fls <- apiQueryStatus(reqID)
  ad <- apiDownloadResponse(fls)
  return(ad)
}



queryAttributeTable <- function(minx, maxx, miny, maxy, startYear=1900, endYear=NULL, properties){
  
  if(!checkIfAuthorised()){return(cat(''))}
  #tkn <- apiGenerateToken(user = authANSIS$usr, pwd=authANSIS$pwd)
  
  qry <- makeQuery(minx, maxx, miny, maxy, startYear=1900, endYear=NULL)
  qryID <- apiCreateQuery(qryJSON=qry)
  ANSISData <- apiRetrieveData(reqID=qryID)
  
  ado <- parseANSISJson(ANSISData)
  makeWideTable(ado, properties = properties)
  
}



#' Get a single site
#' @param username ANSIS account username
#' @param password ANSIS account password
#' @examples AuthoriseMe(username='Bob', password='123')
#' @details  Before you can use this package you need to provide your ANSIS account credentials. If you do not already have a valid ANSIS user account please go to the ANSI website to creat an account.
#' @author Ross Searle
#' @return logical
#' @export
#'
#'
queryQuerySingleSite <- function(providerID, siteID, format='ANSISDataObject'){
  
  if(!checkIfAuthorised()){return(cat(''))}
  tkn <- apiGenerateToken(user = authANSIS$usr, pwd=authANSIS$pwd)
  
  url <- paste0('https://apim-ansis-hrm-test-ae.azure-api.net/sdr-public/v1/SingleSite?provider=' , providerID, '&site=', siteID )
  resp <- httr::GET(url=url, httr::add_headers(Authorization = paste0("Bearer ", tkn)))
  jsn <- httr::content(resp, 'text', encoding = 'UTF8')
  if(format=='JSON'){
    return(jsn)
  }else if(format=='ANSISDataObject'){
    ado <- parseANSISJson(jsn)
    return(ado)
  }else if(format=='CSV'){
    ado <- parseANSISJson(ansisResponse = jsn)
    return(ado$CSV)
  }else if(format=='HTML'){
    ado <- parseANSISJson(ansisResponse = jsn)
    html <- generateSiteDescription(ado)
    tf <- tempfile(pattern = 'ANSIS_', fileext = '.html')
    cat(html, file = tf)
    browseURL(paste('file://', tf, sep='/'))
    
  }
  
}




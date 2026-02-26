################################################################# #
#####       Author : Ross Searle                              ###
#####       Date :  Fri May  2 09:29:32 2025                  ###
#####       Purpose : Interact with the ANSIS Web API         ###
#####       Comments :                                        ###
################################################################# #


#' Get ANSIS API Documentation
#' @details  Opens up the documentation of the ANSIS API upon wich this package has been developed
#' @author Ross Searle
#' @return file
#' @export
ANSIS_APIDocs <- function(){

  if(Sys.info()["sysname"]=='Linux'){
    cat("\nWe can't open the pdf file for you in this Linux environment but you can find it at - ", crayon::red( paste0(system.file(package = "ANSR"), "/extdata/ANSIS_HTTP_API.pdf\n")))
  }else{
    shell(paste0(system.file(package = "ANSR"), "/extdata/ANSIS_HTTP_API.pdf"), wait = F)
  }
}

#' Open a demo R Script
#' @details  Opens up a script in RStudio which demonstrates the functionality in the 'ANSR' package.
#' @author Ross Searle
#' @return R script
#' @export
ANSIS_Demos <- function(){

  if(Sys.info()["sysname"]=='Linux'){
   lns <-  readLines(paste0(system.file(package = "ANSR"), "/extdata/ANSR_Demo.R"))
   lns <- paste0(lns, "\n")
   cat(lns)
   cat(crayon::bold(crayon::red('\nCopy and paste the text above into an R script file\n\n')))
  }else{

      shell(paste0(system.file(package = "ANSR"), "/extdata/ANSR_Demo.R"), wait = F)
  }
}


#' Opens the ANSIS Website
#' @details  Opens up the ANSIS Website
#' @author Ross Searle
#' @return url
#' @export
#'
ANSIS_OpenWebsite <- function(){
  browseURL('https://ansis.net/')
}





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
apiAuthoriseMe <- function(username, password, DataStorePath){

  tkn <-apiGenerateToken(username, password, verbose=T)

  if(is.null(tkn)){
    cat(paste0('\n', crayon::bold(crayon::red('Authorisation was not successful')), '\n\nThese are not valid ANSIS account credentials.\nPlease check your username and password supplied.\n\n',
               'If you do not already have a valid ANSIS user account please go to ', Constants@ANSISguiURL, '\n\n' ))

  }else if(!is.null(tkn$error)){
    cat(paste0('\n', crayon::bold(crayon::red(tkn$error)), '\n\n', tkn$error_description, '\nPlease check your username and password supplied.\n\n',
               'If you do not already have a valid ANSIS user account please go to ', crayon::bold(crayon::red('portal.ansis.net')), '\n\n' ))

  }else{

    expire <- Sys.time() + as.numeric(tkn$expires_in)
    if(!dir.exists(DataStorePath)){dir.create(DataStorePath, recursive = T)}
    if(!dir.exists(paste0(DataStorePath, '/ANSISDataObjects'))){dir.create(paste0(DataStorePath, '/ANSISDataObjects'), recursive = T)}
    if(!dir.exists(paste0(DataStorePath, '/RawJSONResponses'))){dir.create(paste0(DataStorePath, '/RawJSONResponses'), recursive = T)}

    authANSIS <<- new("Authorisation",
                      usr = username,
                      pwd = password,
                      Authorised = T,
                      Token = tkn$access_token,
                      TokenExpiry=expire,
                      DataStorePath=DataStorePath)

    tokenTime <- authANSIS@TokenExpiry - Sys.time()
    cat(paste0('\n', crayon::bold(crayon::green('Authorisation successful')), '\n\nThis authorisation with ANSIS will remain valid for about ' , round(tokenTime), ' hours. You might have to reauthorise youself at some stage.\n' ))

    dbp <- paste0(authANSIS@DataStorePath, '/ANSISQueryCache.db')
    if(!file.exists(dbp)){
      createQueryDB(dbp)
    }

    cacheDescribe()

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

  # url <- 'https://b2cansishrmtestae.b2clogin.com/b2cansishrmtestae.onmicrosoft.com/B2C_1A_ROPC_AUTH/oauth2/v2.0/token?grant_type=password'
  #
  #
  # bdyL <- list()
  # bdyL$username = user
  # bdyL$password = URLencode(pwd, reserved = T)
  # bdyL$scope = 'openid 44040d13-769e-4567-8bbe-b2109b42c939'
  # bdyL$client_id = '44040d13-769e-4567-8bbe-b2109b42c939'
  # bdyL$response_id = 'token id_token'
  #
  # #jsnb = jsonlite::toJSON(bdyL)
  #
  # resp <- httr::POST(url=url, body=bdyL, encode='form')

  resp <- httr::GET(paste0(Constants@ANSISAPIurlProd, '/ansis-external-api/query-requests/v2/Authorise'), httr::add_headers(username=user, password=pwd))
  jsn <- httr::content(resp, 'text', encoding = 'UTF-8')
  auth = jsonlite::fromJSON(jsn)

  if(verbose){
    return(auth)
  }else{
    return(auth$access_token)
  }

}


#' ANSIS Catalogue Summary
#' @details  Lists the ANSIS data providers
#' @author Ross Searle
#' @return data frame
#' @export

apiCatalogueSummary  <- function(){

  url <- paste0(Constants@ANSISAPIurlProd,'/site-catalogue/v1/catalogue-summary')
  resp <- httr::GET(url=url)
  jsn <- httr::content(resp, 'text', encoding = 'UTF-8')
  smry <- jsonlite::fromJSON(jsn, simplifyDataFrame = T)
  return(smry)
}


#' ANSIS Provider Catalogue
#'
#' Shows details for specified ANSIS data providers
#' @param poviderNames A list of ANSIS Provider names.
#' @param includeProperties Include the list of ANSIS Soil properties associated with each site.
#' @param outputFormat Either a dataframe or JSON
#' @param ViewIt View the raw json output.
#' @details Returns the site data catalogues for the providers specified with the poviderNames list. Valid IDs are provided via the “name” field of the apiCatalogueSummary() function.
#' Returns a list of ANSIS site numbers along with locations and a list of properties for each site.
#'
#' @author Ross Searle
#' @return dataframe
#' @export
#'
apiProviderCatalogue   <- function(poviderNames=NULL, includeProperties=F, outputFormat='dataframe', ViewIt=F){

  if(is.null(poviderNames)){
    prvs <- apiCatalogueSummary()
    poviderNames <- prvs$id
  }

  cdf <- data.frame()

    for (i in 1:length(poviderNames)) {

      p <- poviderNames[i]
      cat(paste0('Getting catalogue information for ', p, '....\n'))

      url <- paste0(Constants@ANSISAPIurlProd,'/site-catalogue/v1/provider/', p)
      resp <- httr::GET(url=url)
      jsn <- httr::content(resp, 'text', encoding = 'UTF-8')
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

  if(ViewIt){
    listviewer::jsonedit(jsn)  ### probably need to smoosh the jsn together if there os more than one provider specified
  }

  if(outputFormat=='dataframe'){
    return(cdf)
  }else{
    jsn
  }

}



#' ANSIS Property Definitions
#'
#' Returns a data frame of ANSIS property codes and their definitions
#' @examples head(apiPropertyDefinitions())
#' @author Ross Searle
#' @return data frame
#' @export


apiPropertyDefinitions <- function(){

  resp <- httr::GET(paste0(Constants@ANSISAPIurlProd, '/site-catalogue/v1/definitions'))

  jsn <- httr::content(resp, 'text', encoding = 'UTF-8')

  defs <- jsonlite::fromJSON(jsn)
  # defs[grepl("^0-0", names(defs))]
  # defs[grepl("^3-2", names(defs))]

  listviewer::jsonedit(defs)

  odf<- data.frame()

  for (i in 0:4) {

    n1 <- defs[as.character(i)]
    propType=n1[[1]]$name

    if(propType == "Layer/Horizon" || propType=="Profile"){

      for (k in 1:length(n1[[1]]$members)) {
        n2 <- defs[n1[[1]]$members[k]]

        df<- data.frame('PropType'=propType, 'Property'=propType, 'Name'=n2[[1]]$name, 'ANSISCode'=names(n2) )
        odf <- rbind(odf, df)
      }

    }else{

        for (j in 1:length(n1[[1]]$members)) {

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
  }
 return(odf)
}



#' Get ANSIS Data
#' @param Name A name for the query. Default is NULL.
#' @param Description A description of the query.
#' @param minx Western extent of the bounding box of the query. CRS WGS84. Default is NULL.
#' @param maxx Eastern extent of the bounding box of the query. CRS WGS84. Default is NULL.
#' @param miny Sothern extent of the bounding box of the query. CRS WGS84. Default is NULL.
#' @param maxy Northern extent of the bounding box of the query. CRS WGS84. Default is NULL.
#' @param soilProperty Soil property element to query on. This is the broadest ANSIS attribute grouping.
#' @param propertyName A specific form of the soil property.
#' @param labCode Green Book standard lab method code.
#' @param startYear Starting year of date range to return data for.
#' @param endYear Finishing year of date range to return data for.
#' @param provider The Data Provider to return data for.
#' @param sites A list of ANSIS Site IDs to return data for.
#' @details This is the main function to use to retrieve data form the ANSIS Data System. It wraps a number of individual functions contained in this package to process a query of the ANSIS Data System from start to finish. THis function sends a query to the ANSIS Data System, retrieves the data and massages the response into and ANSIS Data Object.
#' @author Ross Searle
#' @return ANSIS Data Object
#' @export

apiGetANSISData <- function(Name=NULL, Description=NULL, minx=minx, maxx=NULL, miny=NULL, maxy=NULL,
                         soilProperty=NULL, propertyName=NULL, labCode=NULL,
                         startYear=NULL, endYear=NULL, provider=NULL, sites=NULL, numCPUs=NULL ){

# labcode parameter removed
# apiGetANSISData <- function(Name=NULL, Description=NULL, minx=minx, maxx=NULL, miny=NULL, maxy=NULL,
#                             soilProperty=NULL, propertyName=NULL,
#                             startYear=NULL, endYear=NULL, provider=NULL, sites=NULL, numCPUs=NULL ){
#
  if(!checkIfAuthorised()){return(cat(''))}

      qo <- makeQueryObject(Name, Description, minx, maxx, miny, maxy, soilProperty, propertyName, labCode, startYear, endYear, provider, sites)
      dataInCache <- checkCache(authANSIS=authANSIS, qObj=qo)

  if(!is.null(dataInCache)){
    if(dataInCache == 'NewNameRequired'){
      return(NULL)
    }else{
      cat(paste0('\nRetrieving ANSIS data from the local cache....\n\n'))
      ado <- cacheRetrieveData(dataInCache)
      return(ado)
    }


  }else{

      reqID <- apiSendQuery(minx, maxx, miny, maxy, soilProperty, propertyName, labCode, startYear, endYear, provider, sites)
      apiQueryStatus_Monitor(reqID)
      outDir <- paste0(authANSIS@DataStorePath, '/RawJSONResponses/', Name)
      if(!dir.exists(outDir)){dir.create(outDir, recursive = T, showWarnings = F)}
      #sitesJSN <- apiDownloadQueryData(reqID, outDir = outDir )
      od <- apiDownloadQueryData2(reqID, outDir = outDir )

      if(is.null(od)){
        cat(crayon::red(paste0('\nNo data returned by the query.\n\n')))
        return(NULL)
      }
      ado <- parseANSISJson2(jsnDir = outDir, numCPUs=numCPUs)

        if(!is.null(authANSIS@DataStorePath) & !is.null(Name)){
            addQueryToCache(authANSIS, qObj=qo, ANSISObj=ado)
        }
      return(ado)
  }
}



#' Send a Query to the ANSIS API
#' @param Name A name for the query. Default is NULL.
#' @param Description A description of the query.
#' @param minx Western extent of the bounding box of the query. CRS WGS84. Default is NULL.
#' @param maxx Eastern extent of the bounding box of the query. CRS WGS84. Default is NULL.
#' @param miny Sothern extent of the bounding box of the query. CRS WGS84. Default is NULL.
#' @param maxy Northern extent of the bounding box of the query. CRS WGS84. Default is NULL.
#' @param soilProperty Soil property element to query on. This is the broadest ANSIS attribute grouping.
#' @param propertyName A specific form of the soil property.
#' @param labCode Green Book standard lab method code.
#' @param startYear Starting year of date range to return data for.
#' @param endYear Finishing year of date range to return data for.
#' @param provider The Data Provider to return data for.
#' @param sites A list of ANSIS Site IDs to return data for.
#' @details Sends a query to the ANSIS Data System.
#' @author Ross Searle
#' @return Request ID
#' @export
apiSendQuery <- function(minx=minx, maxx=NULL, miny=NULL, maxy=NULL,
                         soilProperty=NULL, propertyName=NULL, labCode=NULL,
                         startYear=NULL, endYear=NULL, provider=NULL, sites=NULL ){

  if(!checkIfAuthorised()){return(cat(''))}

  cat('Sending query to the ANSIS server....\n')

  ansisProperties = getAnsisPropertyCodes( soilProperty, propertyName, labCode)

  qryJSON <- makeQuery(minx=minx, maxx = maxx, miny = miny, maxy = maxy, ansisProperties=ansisProperties,
                              startYear=startYear, endYear=endYear, provider=provider, sites=sites)

  resp <- httr::POST(url=paste0(Constants@ANSISAPIurlProd, '/ansis-external-api/query-requests/v2/create-query-request'),
               body=qryJSON,
               httr::add_headers(Authorization = paste0("Bearer ", authANSIS@Token)))
  jsn <- httr::content(resp, 'text', encoding = 'UTF-8')
  reqID <- stringr::str_split(jsn, ': ')[[1]][2]
  if(is.null(reqID)){
    stop(paste0('There is a problem with your query - ', qryJSON))
  }
  return(reqID)
}

# Was testing to see if this approach would speed up reponses from the ANSIS Server but alas it did not. The ANSIS system seems to be optimised to process queries at its end

# apiSendBigQuery <- function(minx=minx, maxx = maxx, miny = miny, maxy = maxy, slice=2){
#
#   if(!checkIfAuthorised()){return(cat(''))}
#
#   cat('Sending query to the ANSIS server....\n')
#
#   ql <- vector(mode = 'list', length = slice*slice)
#
#   inc <- (maxx - minx) / slice
#   minsx <- minx
#   minsy <- miny
#
#   cnt = 1
#
#     for (i in 1:slice) {
#       #increment y
#       maxsy = minsy + inc
#
#       for (j in 1:slice) {
#         #increment x
#         minsx = minsx
#         maxsx = minsx + inc
#         #print(paste0(minsx, " ", maxsx, " ", minsy, " ", maxsy))
#
#        q <-  makeQuery(minx = minsx, maxx = maxsx, miny = minsy, maxy = maxsy)
#        ql[cnt] <- q
#        cnt=cnt+1
#
#         minsx=maxsx
#       }
#       minsy=maxsy
#       minsx = minx
#     }
#
#
#   reqIDs <- vector(mode = 'list', length = slice*slice)
#       for (i in 1:length(reqIDs)) {
#         cat('\rSubmitting query ',i, " of ", length(reqIDs))
#         resp <- httr::POST(url=paste0(Constants@ANSISAPIurlV2, '/create-query-request'),
#                            body=ql[[i]],
#                            httr::add_headers(Authorization = paste0("Bearer ", authANSIS@Token)))
#         jsn <- httr::content(resp, 'text', encoding = 'UTF-8')
#         reqID <- stringr::str_split(jsn, ': ')[[1]][2]
#         if(is.null(reqID)){
#           stop(paste0('There is a problem with your query - ', qryJSON))
#         }
#         reqIDs[i] <- reqID
#       }
#   return(reqIDs)
# }



#' Monitor an ANSIS Query Status
#' @param reqID get the status for this request ID
#' @details  Monitors the status of an ANSIS query until it is finished
#' @author Ross Searle
#' @return ANSIS query status
#' @export

apiQueryStatus_Monitor <- function(reqID){

  if(!checkIfAuthorised()){return(cat(''))}

    inProgress=T
    pb <- c('\\', '|', '/', '-')
    iter = 1

    cat(paste0('\rQuerying ANSIS Server  [-] : Number of data chunks generated = 0' ))

    while (inProgress) {
      Sys.sleep(1)
      cat(paste0('\rQuerying ANSIS Server  [', pb[iter], '] : Number of data chunks generated = ' ))
      iter = iter+1
        if(iter > 4){
          iter=1

          resp <- httr::GET(url=paste0(Constants@ANSISAPIurlProd, '/ansis-external-api/query-requests/v2/get-query-request-status?requestId=', reqID),
                            httr::add_headers(Authorization = paste0("Bearer ", authANSIS@Token)))
          jsn <- httr::content(resp, 'text', encoding = 'UTF-8')
          status <- jsonlite::fromJSON(jsn, simplifyDataFrame = F)
          cat(length(status$sdrRequest$files))

          if(status$sdrRequest$status=='Completed'){
            cat('\n\nQuery completed...\n\n')
            inProgress=F
          }
        }
    }
    return(reqID)
}


#' Get the Statuts of all ANSIS Queries
#' @param statusTypes If specified just statuses of the given types are returned.
#' @details Gets the status of all of your ANSIS Data System queries.
#' @author Ross Searle
#' @return data frame
#' @export
#'
apiQueryStatus_All <- function(statusTypes=NULL){

  if(!checkIfAuthorised()){return(cat(''))}

  stats <- c('Processing', 'Completed', 'Queued', 'Error')

  stat <- paste0(Constants@ANSISAPIurlProd, '/ansis-external-api/query-requests/v2/get-all-query-requests-statuses')
  resp <- httr::GET(url=stat,  httr::add_headers(Authorization = paste0("Bearer ", authANSIS@Token)))
  jsn <- httr::content(resp, 'text', encoding = 'UTF-8')

  if(jsn=='[]'){
    cat('\nNo queries in the queue.\n\n')
    return()
  }
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


#' Get an ANSIS Query Status
#' @param reqID Request ID
#' @details  Returns the status of a specific requiest ID.
#' @author Ross Searle
#' @return ANSIS query status
#' @export
#'
apiQueryStatus_Single <- function(reqID, verbose=F){

  if(!checkIfAuthorised()){return(cat(''))}

  stat <- paste0(Constants@ANSISAPIurlProd, '/ansis-external-api/query-requests/v2/get-query-request-status?requestId=', reqID)
  resp <- httr::GET(url=stat, httr::add_headers(Authorization = paste0("Bearer ", authANSIS@Token)))
  jsn <- httr::content(resp, 'text', encoding = 'UTF-8')
  rq <- jsonlite::fromJSON(jsn, simplifyDataFrame = T, simplifyVector = T)

  if(verbose){
    return(rq)
  }else{
    return(rq$sdrRequest$status)
  }

}


#' Delete an ANSIS Query
#' @param reqID Request ID
#' @details  Deletes (stops) a specific ANSIS Data System query
#' @author Ross Searle
#' @return ANSIS query status
#' @export
#'
apiDeleteQuery <- function(reqID){

  if(!checkIfAuthorised()){return(cat(''))}
  url=paste0(Constants@ANSISAPIurlProd, '/ansis-external-api/query-requests/v2/cancel-query-request?requestId=', reqID)
  resp <- httr::POST(url=url, httr::add_headers(Authorization = paste0("Bearer ", authANSIS@Token)))
  jsn <- httr::content(resp, 'text', encoding = 'UTF-8')
  return(jsn)

}

#' Delete all ANSIS Queries
#' @details  Deletes (stops) all of your ANSIS Data System queries
#' @author Ross Searle
#' @return ANSIS query status
#' @export
#'
apiDeleteAllQueries <- function(){

  queries <- apiQueryStatus_All()
  if(is.null(queries)){
    return()
  }
  if(nrow(queries)==0){
    cat('\nNo queries to delete\n')
    return()
  }

  pb <-progress::progress_bar$new(
    format = "Deleting queries :what [:bar] :percent in :elapsed",
    total = nrow(queries), clear = FALSE, width= 100, show_after=1)

  for (i in 1:nrow(queries)) {
    pb$tick(tokens = list(what = stringr::str_pad(i, 20, 'left')))
    rec <- queries[i,]
    apiDeleteQuery(rec$id)
  }
cat(paste0('\nDeleted ', nrow(queries), ' queries\n'))
}


#' Download Query Data
#' @param reqID Request ID
#' @param outDir A directory path that the data will be downloaded to.
#' @details Downloads the json query response files from the ANSIS server to your local machine. It also merges the single ANSIS response files into a single json file
#' @author Ross Searle
#' @return json ANSIS data
#' @export
#'

apiDownloadQueryData <- function(reqID, outDir=NULL){

  if(!checkIfAuthorised()){return(cat(''))}

  qStat <- apiQueryStatus_Single(reqID = reqID,  verbose=T)
  fls <- qStat$sdrRequest$files

  cat('Downloading data from ANSIS server....\n')


  if(is.null(outDir)){
    outDir <- paste0(tempdir(), '/tmp_', as.numeric(Sys.time()))
  }else{
    if(!dir.exists(outDir)){
      dir.create(outDir, recursive = T)
    }else{
        prevfls <- list.files(outDir, recursive = T, full.names = T)
        unlink(prevfls)
      }
  }
  print(outDir)

  files <- vector(mode = 'character', length = length(fls))
  for (i in 1:length(fls)) {

    if(length(fls)>10){
      cat(paste0('\rDownloading ', i, ' of ', length(fls) ))
    }
    fn <- paste0(outDir, '/',fls[i], '.json')
    if(!file.exists(fn)){

        resp <- httr::GET(url=paste0(Constants@ANSISAPIurlProd, '/ansis-external-api/query-requests/v2/download-response?fileId=', fls[i] ),
                          httr::add_headers(Authorization = paste0("Bearer ", authANSIS@Token)))
        jsn <- httr::content(resp, 'text', encoding = 'UTF-8')
        cat(jsn, file = fn)
    }
  }

 jl <-  mergeResponseFiles(outDir)

 return(jl)
}


apiDownloadQueryData2 <- function(reqID, outDir=NULL){

  if(!checkIfAuthorised()){return(cat(''))}

  qStat <- apiQueryStatus_Single(reqID = reqID,  verbose=T)
  fls <- qStat$sdrRequest$files

  if(length(fls)==0){
    return(NULL)
  }

  cat('Downloading data from ANSIS server....\n')


  if(is.null(outDir)){
    outDir <- paste0(tempdir(), '/tmp_', as.numeric(Sys.time()))
  }else{
    if(!dir.exists(outDir)){
      dir.create(outDir, recursive = T)
    }else{
      prevfls <- list.files(outDir, recursive = T, full.names = T)
      unlink(prevfls)
    }
  }
  print(outDir)

  files <- vector(mode = 'character', length = length(fls))
  for (i in 1:length(fls)) {

    if(length(fls)>10){
      cat(paste0('\rDownloading ', i, ' of ', length(fls) ))
    }
    fn <- paste0(outDir, '/',fls[i], '.json')
    if(!file.exists(fn)){

      resp <- httr::GET(url=paste0(Constants@ANSISAPIurlProd, '/ansis-external-api/query-requests/v2/download-response?fileId=', fls[i] ),
                        httr::add_headers(Authorization = paste0("Bearer ", authANSIS@Token)))
      jsn <- httr::content(resp, 'text', encoding = 'UTF-8')
      cat(jsn, file = fn)
    }
  }

 # jl <-  mergeResponseFiles(outDir)

  return(outDir)
}


#' Get a DSM Suitable Data Table
#' @param ansisObject an ANSIS data object.
#' @param propertyCode Soil property code.
#' @details This is a convenience function which returns data from An ANSIS query in a format for typical Digital Soil Mapping workflows. ie Site IDs, Locations, Dates, Depths and Soil Property values in a wide table format.
#' @author Ross Searle
#' @return logical
#' @export
#'
#'
getDSMtable <- function(ansisObject=NULL, propertyCode=NULL){

  if(is.null(propertyCode)){
    stop('You must specify a property code.')
  }

  if(is.null(ansisObject)){
    stop('You must supply an ANSIS object.')
  }
    cat('\nGenerating DSM Table....\n')
    dsm <- makeDSMTable(ansisObject, propertyCode)
    return(dsm)
}
#
# getDSMtable <- function(Name=NULL, Description=NULL, minx, maxx, miny, maxy,soilProperty=NULL, propertyName=NULL, labCode=NULL, startYear=1900, endYear=NULL, numCPUs=NULL){
# #getDSMtable <- function(Name=NULL, Description=NULL, minx, maxx, miny, maxy,soilProperty=NULL, propertyName=NULL,  startYear=1900, endYear=NULL, numCPUs=NULL){
#
#   if(!checkIfAuthorised()){return(cat(''))}
#     ado <- apiGetANSISData(Name, Description,minx=minx, maxx=maxx, miny=miny, maxy=maxy, soilProperty=soilProperty, propertyName=propertyName, labCode=labCode, startYear=1900, endYear=NULL, numCPUs=numCPUs)
#
# if(is.null(labCode)){
#   lc=NULL
# }else{
#   lc = labCode
# }
#
#     if(!is.null(ado)){
#       cat('\nGenerating DSM Table....\n')
#         makeWideTable(ansisObject=ado, propertyType = 'Lab', labcodes = lc)
#     }
# }



#' Get a single site
#' @param providerID ANSIS Data Provider ID
#' @param siteID ANSIS site ID

#' @details Returns all the data for a single site in a number of formats. These are, raw json (JSON), a parsed object for easier access to data (ANSISDataObject), a flat csv file (CSV) or a HTML site description (HTML)
#' @author Ross Searle
#' @return logical
#' @export

getSingleSite <- function(providerID, siteID, format='ANSISDataObject'){

  if(!checkIfAuthorised()){return(cat(''))}

  url <- paste0(Constants@ANSISAPIurlProd, '/sdr-public/v1/SingleSite?provider=' , providerID, '&site=', siteID )
  resp <- httr::GET(url=url, httr::add_headers(Authorization = paste0("Bearer ", authANSIS@Token)))
  jsn <- httr::content(resp, 'text', encoding = 'UTF-8')
  if(format=='JSON'){
    return(jsn)
  }else if(format=='ANSISDataObject'){
    ado <- parseANSISJson(ansisResponse = jsn)
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
    return(html)

  }

}


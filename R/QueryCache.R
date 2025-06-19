library(DBI)


makeQueryObject <- function(Name=NULL, Description=NULL, minx=minx, maxx=NULL, miny=NULL, maxy=NULL, 
                            soilProperty=NULL, propertyName=NULL, labCode=NULL,
                            startYear=NULL, endYear=NULL, provider=NULL, sites=NULL ){
  
  
  queryObject <- list()
  queryObject$Name <- Name
  queryObject$Description <- Description
  queryObject$Time <- Sys.time()
  queryObject$User <- authANSIS@usr
  queryObject$Query$Bdy$Minx <- if(is.null(minx)){minx=''}else{minx=minx}
  queryObject$Query$Bdy$Maxx <- if(is.null(maxx)){maxx=''}else{maxx=maxx}
  queryObject$Query$Bdy$Miny <- if(is.null(miny)){miny=''}else{miny=miny}
  queryObject$Query$Bdy$Maxy <-  if(is.null(maxy)){maxy=''}else{maxy=maxy}
  queryObject$Query$SoilProperty = if(is.null(soilProperty)){soilProperty=''}else{soilProperty=soilProperty }
  queryObject$Query$PropertyName =  if(is.null(propertyName)){propertyName=''}else{propertyName=propertyName}
  queryObject$Query$LabCode =  if(is.null(labCode)){labCode=''}else{labCode=labCode}
  queryObject$Query$StartYear =  if(is.null(startYear)){startYear=''}else{startYear=startYear}
  queryObject$Query$EndYear = if(is.null(endYear)){endYear=''}else{endYear=endYear}
  queryObject$Query$Provider =  if(is.null(provider)){provider=''}else{provider=provider}
  queryObject$Query$Sites =  if(is.null(sites)){sites=''}else{sites=sites}
  return(queryObject)
}


checkCache <- function(authANSIS, qObj){
  
   dbp <- paste0(authANSIS@DataStorePath, '/ANSISQueryCache.db')
   
   sql <- paste0("SELECT * From ANSISQueries where Name='", qObj$Name , "'" )
   qres <- doQuery(dbp, sql)
   if(nrow(qres)==1){
    cat(crayon::red('\nThis query name exists already. Please choose a different name.'))
     return('NewNameRequired')
   }

  sql <- paste0("SELECT * From ANSISQueries where Minx='", qObj$Query$Bdy$Minx , "' and Maxx='",  qObj$Query$Bdy$Maxx, "' and Miny='",  qObj$Query$Bdy$Miny, "' and Maxy='",  qObj$Query$Bdy$Maxy,
                "' and SoilProperty = '",  qObj$Query$SoilProperty, "' and PropertyName='",  qObj$Query$propertyName,  "' and LabCode='",  qObj$Query$LabCode 
                , "' and StartYear='",  qObj$Query$StartYear, "' and EndYear='",  qObj$Query$EndYear, "' and Provider='",  qObj$Query$Provider, "' and Sites='",  qObj$Query$Sites, "'"
  )
  qres <- doQuery(dbp, sql)
  if(nrow(qres)==1){
    f <- paste0(authANSIS@DataStorePath, '/ANSISDataObjects/', qres$Name, '.rds')
    if(file.exists(f)){
      cat('\nData exists in the cache.\n\n')
      return(qres$Name)
    }else{
      sql <- paste0("DELETE from ANSISQueries where Name = '", qres$Name, "'")
      doExecute(dbp, sql)
      return(NULL)
    }
  }else{
    return(NULL)
  }
}


#' Retrieve Data From Local Cache 
#' @param qName The name of the query to retrieve data for
#' @description When you do a query on the ANSIS data system using the apiGetANSISData() funtion, the data returned from ANSIS is cached locally. Data is returned from the local cache if you send a query with exactly the same parameters again.
#' @author Ross Searle
#' @return ANSIS Data Object - List
#' @export
cacheRetrieveData <- function(qName){
  
  f <- paste0(authANSIS@DataStorePath, '/ANSISDataObjects/', qName, '.rds')
  qo <- readRDS(f)
  return(qo)
}



addQueryToCache <- function(authANSIS, qObj, ANSISObj){
  
  dbp <- paste0(authANSIS@DataStorePath, '/ANSISQueryCache.db') 
  
  sql <- paste0("INSERT into ANSISQueries Values('", qObj$Name, "', '", qObj$Description, "', '", qObj$Time, "', '",  qObj$User, "', '",  
                qObj$Query$Bdy$Minx, "', '",  qObj$Query$Bdy$Maxx, "', '",  qObj$Query$Bdy$Miny, "', '",  qObj$Query$Bdy$Maxy, "', '", 
                qObj$Query$SoilProperty, "', '",  qObj$Query$propertyName, "', '",  qObj$Query$LabCode, "', '", 
                qObj$Query$StartYear, "', '",  qObj$Query$EndYear, "', '",  qObj$Query$Provider, "', '",  qObj$Query$Sites, "')"
                )
 res <- doExecute(dbp, sql)
 
 qp <- paste0(authANSIS@DataStorePath, '/ANSISDataObjects/',qObj$Name, '.rds' ) 
 saveRDS(ANSISObj, paste0(qp))
 cat(paste0('\nQuery data saved to - ', qp, '\n'))
 
}


createQueryDB <- function(dbp){
  
 sql <- "CREATE TABLE ANSISQueries (
    Name         TEXT,
    Description  TEXT,
    Time         TEXT,
    User         TEXT,
    Minx         TEXT,
    Maxx         TEXT,
    Miny         TEXT,
    Maxy         TEXT,
    SoilProperty TEXT,
    PropertyName TEXT,
    LabCode      TEXT,
    StartYear    INTEGER,
    EndYear      INTEGER,
    Provider     TEXT,
    Sites        TEXT
  );"
  
 doExecute(dbp, sql)
  
}







doQuery <- function(dbp,sql){
  
  tcon <- DBI::dbConnect(RSQLite::SQLite(), dbp)
  res <- DBI::dbSendQuery(tcon, sql)
  rows <- DBI::dbFetch(res)
  DBI::dbClearResult(res)
  DBI::dbDisconnect(tcon)
  
  return(rows)
}

doExecute <- function(dbp, sql){

  tcon <- DBI::dbConnect(RSQLite::SQLite(), dbp)
  res <- DBI::dbExecute(tcon, sql)
  DBI::dbDisconnect(tcon)
  return(res)
}

doAppend <- function(df, tableName=NULL){
  
  tcon <- DBI::dbConnect(RSQLite::SQLite(), dbp)
  res <- DBI::dbAppendTable(tcon, tableName, df)
  DBI::dbDisconnect(tcon)
  return(paste0(nrow(df), ' rows added'))
}

#' Show Local Data Cache QueryNames 
#' @description A list of the query names currently stored in the local data cache
#' @author Ross Searle
#' @return list
#' @export
#' 
cacheShowQueryNames <- function(){
  
  dbp <- paste0(authANSIS@DataStorePath, '/ANSISQueryCache.db') 
  sql <- paste0("SELECT Name From ANSISQueries" )
  df <- doQuery(dbp, sql)
  return(df$Name)
}


#' Show Cache File Sizes
#' @param siteCount If true this function will also show the number of sites returned by each query
#' @description Shows a list of the files stored in the local data cahe and their file size
#' @author Ross Searle
#' @return ANSIS Data Object - List
#' @export
#' 
cacheShowFileSizes <- function(siteCount=F){
  
  pao <- paste0(authANSIS@DataStorePath, '/ANSISDataObjects') 
  fls <- list.files(pao, recursive = F, full.names = T)
  
  if(length(fls)==0){
     cat(paste0('\nThere is no data in the local ANSIS data cache - ', authANSIS@DataStorePath, '\n\n'))
  }else{
  
  siteCnts <- vector(mode = 'numeric', length = length(fls))
  
  odf <- data.frame(QueryName=character(), FileSizeMb=numeric())
  for (i in 1:length(fls)) {
    f <- fls[i]
    fs <- as.numeric(sprintf("%.2f",file.size(f)/1000000))
    nme <- stringr::str_remove(basename(f), '.rds')
    df <- data.frame(QueryName=nme, FileSizeMb=fs)
    odf <- rbind(odf, df)
    if(siteCount){
      d <- readRDS(f)
      siteCnts[i] <- length(d$jsonList$data)
    }
  }
  
  if(siteCount){
    odf$SiteCnt <- siteCnts
  }
  
  return(odf)
  }
 
}


#' Show Cached Queries
#' @param queryName If specified the function only returns information for that specific query
#' @param verbose Show all information
#' @description Shows information about cached queries
#' @author Ross Searle
#' @return data frame
#' @export
#' 
cacheShowCachedQueries <- function(queryName=NULL, verbose=T){
  
  dbp <- paste0(authANSIS@DataStorePath, '/ANSISQueryCache.db') 
  
  if(verbose){
    flds <- '*'
  }else{
    flds <- 'Name, Description, Time '
  }
  
  sql <- paste0("SELECT ", flds, " From ANSISQueries" )
  if(!is.null(queryName)){
    sql <- paste0(sql, " where Name='", queryName, "'")
  }
  df <- doQuery(dbp, sql)
  
  if(nrow(df)==0){
    paste0('There is no data in the local ANSIS data cache - ', authANSIS@DataStorePath)
  }else{
  return(df)
  }
}

#' Empty the Local Data Cache
#' @param queryName If specified the function only deletes data for that specific query
#' @description Deletes cached queries
#' @author Ross Searle
#' @return data frame
#' @export
cacheEmpty <- function(queryName=NULL){
  
  dbp <- paste0(authANSIS@DataStorePath, '/ANSISQueryCache.db') 

  if(is.null(queryName)){
    pao <- paste0(authANSIS@DataStorePath, '/ANSISDataObjects') 
    fls <- list.files(pao, recursive = F, full.names = T)
    unlink(fls, recursive = F)
    pjsn <- paste0(authANSIS@DataStorePath, '/RawJSONResponses') 
    fls <- list.files(pjsn, recursive = T, full.names = T, include.dirs = T)
    unlink(fls, recursive = T)
  }else{
    f <-  paste0(authANSIS@DataStorePath, '/ANSISDataObjects/', queryName, '.rds')
    unlink(f)
    pjsn <- paste0(authANSIS@DataStorePath, '/RawJSONResponses/', queryName) 
    fls <- list.files(pjsn, recursive = T, full.names = T, include.dirs = T)
    unlink(fls)
  }
  
  sql <- 'DELETE From ANSISQueries'
  
  if(!is.null(queryName)){
    sql <- paste0(sql, " where Name ='", queryName, "'")
  }
  
  doExecute(dbp, sql)
  
}

dir_size <- function(path, recursive = TRUE) {
  stopifnot(is.character(path))
  files <- list.files(path, full.names = T, recursive = recursive)
  vect_size <- sapply(files, function(x) file.size(x))
  size_files <- sum(vect_size)
  size_files
}


#' Describe Cache
#' @description Summary information about the local data cache
#' @author Ross Searle
#' @return character
#' @export
cacheDescribe <- function(){
  fls <- list.files(paste0(authANSIS@DataStorePath, '/ANSISDataObjects'), pattern = '.rds$', recursive = F)
  cat(paste0('\nLocal Datastore contains ', length(fls), ' query responses stored. Size = ', round(dir_size(authANSIS@DataStorePath)/10**6), " MB : Location - ", authANSIS@DataStorePath, "\n"))
}










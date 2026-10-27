

#' Get Properties in an ANSIS Response
#' @param ANSISObject An ANSIS data object
#' @param PropertyType Used to filter the response on a property
#' @details  Returns the status of a specific requiest ID.
#' @author Ross Searle
#' @return ANSIS query status
#' @export
#'

getPropertiesInANSISResponse <- function(ANSISObject, PropertyType=NULL){

  d <- ANSISObject$CSV
  if(!is.null(PropertyType)){
    return(unique(d$Property))
  }else{
    d2 <- d[d$PropertyType==PropertyType, ]
    return(unique(d2$Property))
  }
}


validateQueryParameters <- function(ansisProperties, minx, maxx, miny, maxy,  startYear, endYear, provider, sites){

  ### Not sure if ANSIS lets you dump the whole DB by not having any properties but leaving this constraint just in case
  if(is.null(ansisProperties)){
    stop(crayon::red('\nYou need to supply at least one value for the ansisProperties parameter.'))
  }

  idxs <- which(!ansisProperties %in% DataSets@mappingInfo$ANSIS_Property)
  if(length(idxs)>0){

    vals <- paste0( ansisProperties[idxs], collapse = ", ")
    msg <- crayon::red('\nThe value/s for at least one of the ansisProperties you supplied is not a valid ANSIS Property. These values are case sensitive.\n
                        Have a look at the values in the ', crayon::blue('getAnsisPropertyInfo()'),  'function to see the valid values.\n\nThe problematic values are ',crayon::cyan(vals))
    x <- stringr::str_replace_all(msg, "[[:blank:]]+", " ")
    stop(x)
  }

  if(!is.null(startYear)){is_valid_year(startYear)}
  if(!is.null(endYear)){is_valid_year(endYear)}
  if(!is.null(provider)){
    p <- DataSets@Providers
    idxs <- which(!provider %in% p$name)
    if(length(idxs)>0){
      stop(crayon::red('\nThe data provider parameter value is not a valid ANSIS Data Provider name.'))
    }
  }


return(T)

}


is_valid_year <- function(x) {
  # Convert to numeric (silently converts invalid strings to NA)
  num <- suppressWarnings(as.numeric(x))
  if(is.na(num)){
    stop(crayon::red(paste0('\n\nThe year values needs to be a numeric value')))
  }

  maxYr <- as.numeric(lubridate::year(Sys.Date()))
  # Check it isn't NA, is a whole number, and falls in range
   if(!(!is.na(num) & (num %% 1 == 0) & (num >= 1900) & (num <= maxYr))){
     stop(crayon::red(paste0('\n\nThe year values have to be between 1900 and ', maxYr)))
   }
    return(T)
}


checkIfAuthorised <- function(){


  if(!exists('authANSIS')){
    stop(paste0('\nUser not authorised. Please use the apiAuthoriseMe() function to set up your ANSIS authorisation.\n\n'))
    #return(F)
  }


  if(!authANSIS@Authorised){
    stop(paste0('\nUser not authorised. Please use the apiAuthoriseMe() function to set up your ANSIS authorisation.\n\n'))
    #return(F)
  }

  ### reauthorise if the token has expired
  if(authANSIS@TokenExpiry <= Sys.time()){
    cat('\nReauthorising with ANSIS...\n')
    apiAuthoriseMe(authANSIS@usr, authANSIS@pwd, authANSIS@DataStorePath)
  }

  return(T)
}

makeQuery <- function(minx=NULL, maxx=NULL, miny=NULL, maxy=NULL, ansisIDs=NULL,
                      sites=NULL, provider=NULL, startYear=1900, endYear=NULL){

  query <- list()

  if(is.null(endYear)){
    endYear<-lubridate::year(Sys.Date())
  }

  if(!is.null(sites)){

    so <- vector(mode='list', length = 1)

    s <- list()
    s$provider = jsonlite::unbox(provider)
    s$sites <- sites
    so[[1]] <- s

   query$sites <- so
  }

  if(!(is.null(minx))){
      bds <- makeBoundingBox(minx, maxx, miny, maxy)
      query$bounds[[1]] <- bds
  }

  query$minYear=jsonlite::unbox(startYear)
  query$maxYear=jsonlite::unbox(endYear)
  query$useSDR=jsonlite::unbox(T)
  query$propertyGroups <- ansisIDs



  jsnQry <- jsonlite::toJSON(query, auto_unbox = F)

  qfields <- list('minx','maxx','miny','maxy','ansisIDs','startYear','endYear','provider','sites', 'ANSISQueryJSON')
  qvals <-    list(minx,  maxx,  miny,  maxy,  ansisIDs,  startYear,  endYear,  provider, sites, jsnQry)
  idxs <- which(sapply(qvals, is.null))
  if(length(idxs)>0){
    qvals[idxs] <- NA
  }
  qdf <- data.frame(QueryField=unlist(qfields), QueryValue=sapply(qvals, function(x) paste(x, collapse = ", ")))

  return(qdf)
}




makeBoundingBox <- function(minx, maxx, miny, maxy){

  bds <- vector(mode = 'list', length = 5)
  bds[[1]] <- c(minx, maxy)
  bds[[2]] <- c(minx, miny)
  bds[[3]] <- c(maxx, miny)
  bds[[4]] <- c(maxx, maxy)
  bds[[5]] <- c(minx, maxy)
  return(bds)
}



mergeResponseFiles <- function(outDir, pattern=NULL){


  fls <- list.files(outDir, pattern = pattern, full.names = T)

  ol <- list()
  ol['$schema'] <- "https://anzsoildata.github.io/def-au-schema-json/schema/domain/2023-07-31/ansis.json"
  ol$data <-list()
  ol$included <- list()
  ol$included$organizations <- list()
  ol$included$persons <- list()
  ol$included$projects <- list()
  ol$meta <- list()

  persons <- list()
  orgs <- list()
  projs <- list()

  for (i in 1:length(fls)) {
   # for (i in 1:10) {

    print(paste0(i , ' of ', length(fls)))

    jl <- jsonlite::fromJSON(fls[i], simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F)
    ol$data <- append(ol$data,jl$data)

   if(length(jl$included$persons))
   {
      for (j in 1:length(jl$included$persons)) {
        pid <- jl$included$persons[[j]]$id
        if(!pid %in% persons){
          pnl <- vector(mode = 'list', length=1)
          pnl[[1]] <- jl$included$persons[[j]]
          ol$included$persons <- append(ol$included$persons, pnl)
          persons <- append(persons, pid)
        }
      }
   }


    if(length(jl$included$organizations))
    {
      for (j in 1:length(jl$included$organizations)) {
        oid <- jl$included$organizations[[j]]$id
        if(!oid %in% orgs){
          onl <- vector(mode = 'list', length=1)
          onl[[1]] <- jl$included$organizations[[j]]
          ol$included$organizations <- append(ol$included$organizations, onl)
          orgs <- append(orgs, oid)
        }
      }
    }


    if(length(jl$included$projects))
    {
      for (j in 1:length(jl$included$projects)) {
        prid <- jl$included$projects[[j]]$scopedIdentifier$value

        if(!prid %in% projs){
          prnl <- vector(mode = 'list', length=1)
          prnl[[1]] <- jl$included$projects[[j]]
          ol$included$projects <- append(ol$included$projects, prnl)
          projs <- append(projs, prid)
        }
      }
    }


  }

  ol$meta$timeStamp <-  jl$meta$timeStamp
  ol$meta$numberReturned <- length(ol$data)
  ol$meta$numberMatched <- length(ol$data)
  ol$meta$numberWithheld <- 0
  ol$meta$curiPrefix <- jl$meta$curiPrefix

return(ol)

}





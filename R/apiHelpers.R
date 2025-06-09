
checkIfAuthorised <- function(){
  
  #if(!rlang::is_environment('pkg.env')){
  if(!exists('authANSIS')){
    cat(paste0('\nUser not authorised. Please use the apiAuthoriseMe() function to set up your ANSIS authorisation.\n\n'))
    return(F)
  }

  #if(!pkg.env$Authorised){
  if(!authANSIS$Authorised){
    cat(paste0('\nUser not authorised. Please use the apiAuthoriseMe() function to set up your ANSIS authorisation.\n\n'))
    return(F)
  }
  
  return(T)
}

makeQuery <- function(minx=NULL, maxx=NULL, miny=NULL, maxy=NULL, properties=NULL, sites=NULL, provider=NULL, startYear=1900, endYear=NULL){
  
  query <- list()
  
  if(is.null(endYear)){
    endYear<-year(Sys.Date())
  }
  
  if(!is.null(sites)){
    
    so <- vector(mode='list', length = 1)
    
    s <- list()
    s$provider = unbox(provider)
    #s$sites <- vector(mode='list', length = 1)
    s$sites <- sites
    #s$sites[[1]] <- sites
    so[[1]] <- s
    
   query$sites <- so
  }
  
  if(!(is.null(minx))){
      bds <- makeBoundingBox(minx, maxx, miny, maxy)
      query$bounds[[1]] <- bds
  }
  

  query$minYear=unbox(startYear)
  query$maxYear=unbox(endYear)
  query$useSDR=unbox(T)
  query$propertyGroups <- properties
  
 
  
  jsnQry <- toJSON(query, auto_unbox = F)
  #cat(jsnQry, file = 'c:/temp/query.json')
  
  return(jsnQry) 
}




makeBoundingBox <- function(minx, maxx, miny, maxy){
  
  bds <- vector(mode = 'list', length = 5)
  bds[[1]] <- c(minx, miny)
  bds[[2]] <- c(minx, maxy)
  bds[[3]] <- c(maxx, maxy)
  bds[[4]] <- c(maxx, miny)
  bds[[5]] <- c(minx, miny)
  return(bds)
}


mergeResponseFiles <- function(outDir){
  
  
  fls <- list.files(outDir, pattern = '.json$', full.names = T)
  
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
    jl <- fromJSON(fls[i], simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F)
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

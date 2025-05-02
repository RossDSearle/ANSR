
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

makeQuery <- function(minx=NULL, maxx=NULL, miny=NULL, maxy=NULL, startYear=1900, endYear=NULL){
  
  if(is.null(endYear)){
    endYear<-year(Sys.Date())
  }
  
  bds <- makeBoundingBox(minx, maxx, miny, maxy)
  
  query <- list()
  query$minYear=unbox(startYear)
  query$maxYear=unbox(2025)
  query$useSDR=unbox(T)
  query$propertyGroups <- c("3-0-0")
  
 # query$bounds[[1]] <- bds
  
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
  
  for (i in 1:length(fls)) {
    jl <- fromJSON(fls[i], simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F)
    ol$data <- append(ol$data,jl$data)
  }

print(  str(ol, max.level = 1))

return(ol)
  
}

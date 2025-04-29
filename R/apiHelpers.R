


makeQuery <- function(minx, maxx, miny, maxy, startYear=1900, endYear=NULL){
  
  if(is.null(endYear)){
    endYear<-year(Sys.Date())
  }
  
  bds <- makeBoundingBox(minx, maxx, miny, maxy)
  
  query <- list()
  query$minYear=startYear
  query$maxYear=2025
  query$useSDR=T
  
  query$bounds[[1]] <- bds
  
  jsnQry <- toJSON(query, auto_unbox = T)
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

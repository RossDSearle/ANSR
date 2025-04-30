


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

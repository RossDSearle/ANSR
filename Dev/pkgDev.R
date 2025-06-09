library(httr)
library(jsonlite)
library(jsonview)
library(stringr)
library(lubridate)


username = 'ross.searle@gmail.com'
password = 'RossTest29'

NP <- function(jsn){
  
  tf <- tempfile(fileext = '.json')
  cat(jsn, file = tf)
  shell(paste0('C:/LocalProgs/Notepad++/notepad++.exe ', tf), wait=F)
  Sys.sleep(5)
  #unlink(tf)
}



ANSR::apiAuthoriseMe(username = 'ross.searle@gmail.com', password = 'RossTest29')


####   Return a single site   #######
ao <- queryQuerySingleSite(providerID='CSIRO_CSIS', siteID=1, format = "ANSISDataObject")
ao <- queryQuerySingleSite(providerID='CSIRO_CSIS', siteID=1, format = "HTML") # need to be finished


##########################  ANSIS Metadata  ###################################

#### Show provider info
apiCatalogueSummary()

#### Provider Catalogue 
### Returns the catalogue for the provider specified in the {ID} parameter in JSON format. Valid IDs are provided via the “name” field of the Catalogue Summary methods response. 
piDF <- apiProviderCatalogue(c('CSIRO_CSIS'))
min(piDF$StartYear)
max(piDF$EndYear)

#### get property definitions
apiPropertyDefinitions()


apiAllQueryStatus()

## Big
minx=14
maxx=152
miny=-20
maxy=-27

## Medium
minx=151
maxx=152
miny=-26
maxy=-27


## Small
minx=151.8
maxx=152
miny=-26.8
maxy=-27

properties <- c('3-0-0', '3-1-0')
properties <- c('3-0-0')

qry <- makeQuery(minx=minx, maxx = maxx, miny = miny, maxy = maxy, properties, startYear = 1999)
qry <- makeQuery(minx=minx, maxx = maxx, miny = miny, maxy = maxy, properties, startYear = 1979, endYear = 1981 )
qry <- makeQuery(provider = c('Tasmania'), sites = c('5246', '5124'))
#NP(jsn=qry)

reqID <- apiSendQuery(qry = qry)

#reqID <- '1cbe5407-0fce-411c-8734-c3ee1cd7893d'
qStat <- apiQueryStatus_Single(reqID = reqID,  verbose=T)
qStat
sites <- apiDownloadQueryData(qStat$sdrRequest$files)
listviewer::jsonedit(sites)

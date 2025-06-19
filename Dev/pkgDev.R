library(httr)
library(jsonlite)
library(jsonview)
library(stringr)
library(lubridate)


username = 'ross.searle@gmail.com'
password = 'RossTest29'


username = 'ross.searle@csiro.au'
password = 'Rossiscool01'

NP <- function(jsn){
  
  tf <- tempfile(fileext = '.json')
  cat(jsn, file = tf)
  shell(paste0('C:/LocalProgs/Notepad++/notepad++.exe ', tf), wait=F)
  Sys.sleep(5)
  #unlink(tf)
}



#apiAuthoriseMe(username = 'ross.searle@gmail.com', password = pc, DataStorePath = 'c:/temp/Ansis/Data')
apiAuthoriseMe(username = 'ross.searle@csiro.au', password = pc, DataStorePath = 'c:/temp/Ansis/Data')



####   Return a single site   #######
ao <- getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "ANSISDataObject")
ao <- getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "HTML") # need to be finished


ao <- getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "JSON")

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


#####  show all queries  
queries <- apiQueryStatus_All()

apiDeleteAllQueries()



##Big minx=148;maxx=152;miny=-28;maxy=-24
##Medium minx=151;maxx=152;miny=-27;maxy=-26
##Small minx=151.95;maxx=152;miny=-27;maxy=-26.95
##Aust minx=112;maxx=155;miny=--44;maxy=-10

minx=151.95;maxx=152;miny=-27;maxy=-26.9527

tictoc::tic()
qry <- makeQuery(minx=minx, maxx = maxx, miny = miny, maxy = maxy)
reqID <- apiSendQuery(qryJSON = qry)
apiQueryStatus_Monitor(reqID)
tictoc::toc()

sitesJSN <- apiDownloadQueryData(reqID, outDir = 'c:/temp/ANSISDownloads')

sitesJSN$data <- sitesJSN$data[1:10]


ado <- parseANSISJson(ansisResponse = sitesJSN, saveFilePath = 'c:/temp/AOs/medium_ADO.txt')

tictoc::tic()
ado <- parseANSISJson(ansisResponse = sitesJSN, saveFilePath = NULL)
tictoc::toc()


properties <- c('3-0-0', '3-1-0')
properties <- c('3-3-0')

qry <- makeQuery(minx=minx, maxx = maxx, miny = miny, maxy = maxy, startYear = 1950)
qry <- makeQuery(minx=minx, maxx = maxx, miny = miny, maxy = maxy, properties)
qry <- makeQuery(provider = c('Tasmania'), sites = c('5246', '5124'))


reqID <- apiSendQuery(qryJSON = qry)
apiQueryStatus_Monitor(reqID)

#apiCancelQuery(reqID)
apiDeleteQuery(reqID='21185663-3d66-4396-a478-155e0af539c5')




apiQueryStatus_All()

#reqID <- '1cbe5407-0fce-411c-8734-c3ee1cd7893d'
qStat <- apiQueryStatus_Single(reqID = reqID,  verbose=T)
qStat

sitesJSN <- apiDownloadQueryData(reqID, outDir = 'c:/temp/ANSISDownloads')
listviewer::jsonedit(sitesJSN)

parseANSISJson(sitesJSN)
ado <- parseANSISJson(ansisResponse = sitesJSN)

 
makeWideTable(ado, propertyType = 'Lab')
makeWideTable(ado, propertyType = 'Horizons')
makeWideTable(ado, propertyType = 'SiteVisit')



getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "ANSISDataObject")
getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "HTML")





tictoc::tic()
reqIDs <- apiSendBigQuery(minx=minx, maxx = maxx, miny = miny, maxy = maxy, slice = 3)
apiQueryStatus_All()
#apiQueryStatus_Single(reqID = 'f7b38576-2014-4c5a-9f87-eb9a73573567', verbose = F)
#apiQueryStatus_Single(reqID = '163dbba5-7f50-4c76-9a83-1efcade05cec', verbose = T)
tictoc::toc()
apiDeleteAllQueries()



# 
# schemaMaps <- read.csv('C:/Projects/ANSIS/shemaMappings/LabSchemaMapping.csv')
# head(schemaMaps)
# labcodesMapping <- read.csv('C:/Projects/ANSIS/shemaMappings/LabANSISSchemaMethodsMappings.csv')

getAnsisPropertyCodes(propertyName = 'Concentration of total organic Carbon')
getAnsisPropertyCodes(propertyName = 'Concentration of organic Carbon')

showAnsisPropertyCodes(propertyName = 'Concentration of organic Carbon')
pdf <- showAnsisPropertyCodes()

reqID <- apiSendQuery(minx=141.95, maxx=152, miny=-27, maxy=-20.9527, propertyName='Concentration of organic Carbon')
apiQueryStatus_Monitor(reqID)
sitesJSN <- apiDownloadQueryData(reqID, outDir = 'c:/temp/ANSISDownloads')
ado <- parseANSISJson(ansisResponse = sitesJSN)
makeWideTable(ansisObject=ado, propertyType = 'Lab', labcodes = '6A1')

showAnsisPropertyCodes(labCode = '6B2')
showAnsisPropertyCodes(soilProperty = 'Nitrogen')
showAnsisPropertyCodes(propertyName = 'Lime (CaCO3) requirement')

unique(ado$CSV$Property)

dsm <- getDSMtable(Name='ANSISDemo2', Description = "This is a description of the purpose of the query", minx=150, maxx=152, miny=-27, maxy=-25, propertyName='Concentration of organic Carbon')

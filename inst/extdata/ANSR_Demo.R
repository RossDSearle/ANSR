#library(httr)
#library(jsonlite)
#library(jsonview)
#library(stringr)
#library(lubridate)


username = 'ross.searle@csiro.au'
password = ''

library(ANSR)


#apiAuthoriseMe(username = 'ross.searle@gmail.com', password = pc, DataStorePath = 'c:/temp/Ansis/Data')
apiAuthoriseMe(username = 'ross.searle@csiro.au', password = pc, DataStorePath = 'c:/temp/Ansis/Data')



####   Return a single site   #######
ao <- getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "ANSISDataObject")
listviewer::jsonedit(ao)

html <- getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "HTML") # need to be finished

jsn <- getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "JSON")
listviewer::jsonedit(ao)

ao <- getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "ANSISDataObject")
makeWideTable(ao, propertyType = 'Lab')
makeWideTable(ao, propertyType = 'Horizons')
makeWideTable(ao, propertyType = 'SiteVisit')



####  Chemical and Physical soil property mappings
showAnsisPropertyCodes()
showAnsisPropertyCodes(soilProperty = 'Nitrogen')
showAnsisPropertyCodes(labCode = '6B2')
showAnsisPropertyCodes(propertyName = 'Lime (CaCO3) requirement')



####  Get a wide format soil property table with location and site info - commonly used in modelling applications

dsm <- getDSMtable(Name='ANSISDemo2', Description = "This is a description of the purpose of the query", minx=150, maxx=152, miny=-27, maxy=-25, propertyName='Concentration of organic Carbon')
head(dsm)



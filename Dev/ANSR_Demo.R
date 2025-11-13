
############################################################################################## #
#####       Author : Ross Searle                                                             ###
#####       Date :  Tue Jun 17 13:59:01 2025                                                 ###
#####       Purpose : The ANSR package enables easy access to the ANSIS data system.         ###
#####                 The package contains functions allowing you to extract data.           ###
#####                 Use the example below to see how it works.                             ###
#####       Comments : We love ANSIS !!!!!!                                                  ###
############################################################################################## #


username = 'ross.searle@csiro.au'
password = ''

library(ANSR)


apiAuthoriseMe(username = 'ross.searle@csiro.au', password = password, DataStorePath = 'c:/temp/Ansis/Data')


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
head(showAnsisPropertyCodes())
showAnsisPropertyCodes(soilProperty = 'Nitrogen')
showAnsisPropertyCodes(labCode = '6B2')
showAnsisPropertyCodes(propertyName = 'Lime (CaCO3) requirement')


ado <- apiGetANSISData(Name='ANSISDemo3xxdgdhdxz', Description = "This is a description of the purpose of the query", minx=150.123, maxx=151, miny=-26, maxy=-25, propertyName='Concentration of organic Carbon')

jsn <- jsonlite::toJSON(ado$jsonList)
cat(jsn , file = 'c:/temp/7kodd.json')

####  Get a wide format soil property table with location and site info - commonly used in modelling applications


dsm <- getDSMtable(ansisObject=ado, propertyCode='6A1')
head(dsm)






############################################################################################## #
#####       Author : Ross Searle                                                             ###
#####       Date :  Tue Jun 17 13:59:01 2025                                                 ###
#####       Purpose : The ANSR package enables easy access to the ANSIS data system.         ###
#####                 The package contains functions allowing you to extract data.           ###
#####                 Use the example below to see how it works.                             ###
#####       Comments : We love ANSIS !!!!!!                                                  ###
############################################################################################## #


#### If you don't have the "ANSR" package already installed on your machine, install it as per below

library(devtools)
devtools::install_github("RossDSearle/ANSR", auth_token = "YourGitHubAuthToken")



username = 'YourANSISUserName'
password = 'YourANSISPassword'



#DataStorePath = '/home/kubeflow/Data'
DataStorePath = 'c:/temp/ansis'

library(ANSR)


apiAuthoriseMe(username = username, password = password, DataStorePath = DataStorePath)


####   Return a single site   #######
ao <- getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "ANSISDataObject")
listviewer::jsonedit(ao)

html <- getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "HTML") # need to be finished

jsn <- getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "JSON")
listviewer::jsonedit(jsn)

ao <- getSingleSite(providerID='CSIRO_CSIS', siteID=1, format = "ANSISDataObject")
makeWideTable(ao, propertyType = 'Lab')
makeWideTable(ao, propertyType = 'Horizons')
makeWideTable(ao, propertyType = 'SiteVisit')



####  Chemical and Physical soil property mappings
showAnsisPropertyCodes()
showAnsisPropertyCodes(soilProperty = 'Chloride')
showAnsisPropertyCodes(propertyName = 'Lime (CaCO3) requirement')



####  Get a wide format soil property table with location and site info - commonly used in modelling applications


dsm <- getDSMtable(Name='ANSISDemo', Description = "This is a description of the purpose of the query", minx=151.4, maxx=152, miny=-25.5, maxy=-25, labCode = '6A1', numCPUs = 15)
head(dsm)
nrow(dsm)

####  Now run the same query again to see the local data cache in action
dsm <- getDSMtable(Name='ANSISDemo', Description = "This is a description of the purpose of the query", minx=151.4, maxx=152, miny=-25.5, maxy=-25, labCode = '4A1', numCPUs = 15)









###### Now lets look at the detail of what is going on behind the above convenience functions



#################  All the bits of the API functions strung together for making and processing a request   ###################

#### These are all strung together in the apiGetANSISData() function along with some other goodness 


# first step - Authorise with ANSIS. This sends back a token which is passed in with all the subsequent calls to the ANSIA API

# The package needs to know where we can save the ANSIS data when it is downloaded - this is given in the "DataStorePath" parameter
apiAuthoriseMe(username = username, password = password, DataStorePath = DataStorePath)

# define a bounding box for the query
minx=151.5;maxx=152;miny=-27.5;maxy=-27

# select the ANSIS Property Name for the data you wish to retrieve
propertyName='Concentration of organic Carbon'

# Send the query to the ANSIS API
reqID <- apiSendQuery(minx=minx, maxx=maxx, miny=miny, maxy=maxy, soilProperty=NULL, propertyName, labCode=NULL, startYear=NULL, endYear=NULL, provider=NULL, sites=NULL)

# Now we wait for the query to do its thing - we have to ping the ANSIS API to see when it has finished retrieving the data

# We can check on a single query status - via its request ID
apiQueryStatus_Single(reqID = reqID, verbose = F)
apiQueryStatus_Single(reqID = reqID, verbose = T)

# Or we can check on all of our query statuses 
apiQueryStatus_All()
apiQueryStatus_All(statusTypes='Processing') # filter on status - 'Processing', 'Completed', 'Queued', 'Error'

# Or we can continually monitor the status of a single query - this is my favourite
apiQueryStatus_Monitor(reqID)

# Once the query has completed it is time to download the data retrieved by the query from the ANSIS server.
# The data is chunked up as files each containing the json for roughly 100 sites

# These are the files that we need to download
status <- apiQueryStatus_Single(reqID = reqID, verbose = T)
status$sdrRequest$files

outDir <-'c:/temp/ANSISData'
if(!dir.exists(outDir)){dir.create(outDir, recursive = T, showWarnings = F)}
# The download function will download all the individual json files produced by the ANSIS query and mash them into one big R list
sitesJSN <- apiDownloadQueryData(reqID, outDir = outDir )

# Now lets have a look at one of the raw json responses
# Get a list of the files downloaded
fls <- list.files(outDir, full.names = T, recursive = F)
fls

# Firstly lets convert the raw json to an R list object
ANSISL <- jsonlite::fromJSON(fls[1])

# now use listviewer::jsonedit() function (it is brilliant) to look at the data
# a little tip look in data > siteVisit > soilProfile > soilLayer for some horizon data
listviewer::jsonedit(ANSISL)

# OK now we have the ANSIS JSON data on our own computer - good luck from here .....





# No, we wouldn't be that mean. json data can be a bit tricky to work with so we have provided a way to massage
# it into some more useful forms. This part can be the slowest part of the ANSIS querying, but never fear
# we will use all of your computers resources to get it done as quickly as possible

ado <- parseANSISJson(ansisResponse = sitesJSN)

# The ANSIS Data Object produced above contains three forms of the data
# 1. A data frame of site locations 
head(ado$locsDF)

# 2. A data frame with one record for each bit of data returned - in both coded and decoded form
head(ado$CSV)

# 3. The raw json data for your own parsing pleasure
str(ado$jsonList, max.level = 1)


# If you so desire after a big ANSIS data session you might want to clean up after yourself
# this deletes the query and the data it produces from the ANSIS servers

# delete a single query
apiDeleteQuery(reqID = reqID)

# or delete all your queries
apiDeleteAllQueries()




##### and finally, some ANSIS metadata functions

# Get the available property groups
apiPropertyDefinitions()

# Lists the ANSIS data providers
apiCatalogueSummary()

# Shows details for specified ANSIS data providers
apiProviderCatalogue(poviderNames = 'CSIRO_CSIS')


##########  So there you have it. ANSIS data to use however you like. Free and to your door.





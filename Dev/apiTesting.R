################################################################# #
#####       Author : Ross Searle                                ###
#####       Date :  Tue Apr 29 08:45:29 2025                    ###
#####       Purpose : ANSIS API V2 Testing                      ###
#####       Comments :                                          ###
################################################################# #


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
  unlink(tf)
}
  


resp <- GET('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/Authorise', add_headers(username=username, password=password))
jsn <- content(resp, 'text', encoding = 'UTF-8')
jl <- fromJSON(jsn)
tkn <- jl$access_token

############# Catalogue Summary  ############ 

### Returns a JSON array of metadata describing the available data catalogues, including ID, file size and the time of the last update. 


resp <- GET('https://apim-ansis-hrm-test-ae.azure-api.net/site-catalogue/v1/catalogue-summary')
resp <- GET('https://apim-ansis-app-prod-ac.azure-api.net/site-catalogue/v1/catalogue-summary')
resp
jsn <- content(resp, 'text', encoding = 'UTF-8')
listviewer::jsonedit(jsn)

head(fromJSON(jsn))



############# Provider Catalogue ############# 

### Returns the catalogue for the provider specified in the {ID} parameter in JSON format. Valid IDs are provided via the “name” field of the Catalogue Summary methods response. 

resp <- GET('https://apim-ansis-hrm-test-ae.azure-api.net/site-catalogue/v1/provider/CSIRO_CSIS')
resp <- GET('https://apim-ansis-app-prod-ac.azure-api.net/site-catalogue/v1/provider/CSIRO_CSIS')
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
listviewer::jsonedit(jsn)

head(fromJSON(jsn))


############# Property Definitions ############# 

### Returns a JSON object/dictionary documenting all valid enumerations used in the properties field used in ANSIS provider catalogues. Each enumeration is documented with its standard ANSIS json code/path, the property name and the ID of child members of that property. 

resp <- GET('https://apim-ansis-hrm-test-ae.azure-api.net/site-catalogue/v1/definitions')
resp <- GET('https://apim-ansis-app-prod-ac.azure-api.net/site-catalogue/v1/definitions')
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
listviewer::jsonedit(jsn)
NP(jsn)
head(fromJSON(jsn))

cat(jsn, file='c:/temp/definitions.json')

defs <- fromJSON(jsn)
defs[grepl("^0-0", names(defs))]
defs[grepl("^3-2", names(defs))]

odf<- data.frame()


for (i in 0:4) {
  print(i)
  n1 <- defs[as.character(i)]
  propType=n1[[1]]$name
  # need to deal with n2 null
  for (j in 1:length(n1[[1]]$members)) {
   print(j) 
    n2 <- defs[n1[[1]]$members[j]]
    prop = n2[[1]]$name
    
    if(!is.null(n2[[1]]$members)){
    for (k in 1:length(n2[[1]]$members)) {
         n3 <- defs[n2[[1]]$members[k]]
       
         df<- data.frame('PropType'=propType, 'Property'=prop, 'Name'=n3[[1]]$name, 'ANSISCode'=n2[[1]]$members[k] )
         odf <- rbind(odf, df)
      }
      
    }
  }
  
}


https://apim-ansis-app-prod-ac.azure-api.net/sdr-public/v1/SingleSite


################################   HERE   ##############################################
resp <- GET('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/Authorise', add_headers(username=username, password=password))
jsn <- content(resp, 'text', encoding = 'UTF-8')
jl <- fromJSON(jsn)
tkn <- jl$access_token


resp <- GET(url='https://apim-ansis-app-prod-ac.azure-api.net/sdr-public/v1/SingleSite?provider=CSIRO_CSIS&site=1',
            add_headers(Authorization = paste0("Bearer ", tkn)))
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
listviewer::jsonedit(jsn)
siteL <- head(fromJSON(jsn, simplifyDataFrame=F))
siteL$data[[1]]$siteVisit[[1]]$soilProfile[[1]]$soilLayer[[1]]$colour[[1]]$result


####  Fetch a Single Site    ########

resp <- GET(url='https://apim-ansis-app-prod-ac.azure-api.net/sdr-public/v2/SingleSite?provider=CSIRO_CSIS&site=1',
            add_headers(Authorization = paste0("Bearer ", tkn)))
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
listviewer::jsonedit(jsn)
siteL <- head(fromJSON(jsn, simplifyDataFrame=F))
siteL$data[[1]]$siteVisit[[1]]$soilProfile[[1]]$soilLayer[[1]]$colour[[1]]$result


tkn <- apiGenerateToken(username, password)


qry <- '{"propertyGroups": ["3-0-0"], "useSDR": true}'
qry <- makeQuery()

minx=151
maxx=152
miny=-26
maxy=-27


qry <- makeQuery(minx, maxx, miny, maxy, startYear=1900, endYear=NULL)


resp <- POST(url='https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/create-query-request',
            body=qry,
            add_headers(Authorization = paste0("Bearer ", tkn)))

resp <- POST(url='https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/create-query-request',
             body=qry,
             add_headers(Authorization = paste0("Bearer ", tkn)))
resp
jsn <- content(resp, 'text', encoding = 'UTF8')

reqID <- str_split(jsn, ': ')[[1]][2]

apiAllQueryStatus(accessToken=tkn)


status <- apiSingleQueryStatus(reqID, verbose = T)
d <- apiDownloadResponse('c3dcdf4d-454e-4d7d-92c5-052bb55708b2')

h <- apiDownloadResponse('20604d96-a3fb-4fea-ad6e-47332c522e91')



resp <- httr::GET(url=paste0('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/get-query-request-status?requestId=', reqID),
            add_headers(Authorization = paste0("Bearer ", tkn)))




outDir <- 'c:/temp/ansis'
if(!dir.exists(outDir)){ dir.create(outDir, recursive = T)}
files <- vector(mode = 'character', length = length(status$sdrRequest$files))
for (i in 1:length(status$sdrRequest$files)) {
  resp <- GET(url=paste0('https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/download-response?fileId=', status$sdrRequest$files[i] ),
              add_headers(Authorization = paste0("Bearer ", tkn)))
  
  resp <- GET(url=paste0('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/download-response?fileId=ae8862c9-32f2-44ed-9cfd-d88a04bc86df'),
              add_headers(Authorization = paste0("Bearer ", tkn)))
  
  resp
  jsn <- content(resp, 'text', encoding = 'UTF-8')
  #down <- fromJSON(jsn, simplifyDataFrame = F)
  files[[i]] <- jsn
  
  fn <- paste0(outDir, '/res_', i, '.json')
  cat(jsn, file = fn)
  
}

 
resp <- GET(url=paste0('https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/download-response?fileId=', status$sdrRequest$files ),
            add_headers(Authorization = paste0("Bearer ", auth$access_token)))
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
down <- fromJSON(jsn, simplifyDataFrame = F)
length(down$data)
down$data[[2]]$geometry




files <- c("C:/Temp/ansis/res_1.json", "C:/Temp/ansis/res_2.json", "C:/Temp/ansis/res_3.json")
jsonl <- lapply(files, function(f) rjson::fromJSON(file = f))
jsonc <- rjson::toJSON(jsonl)
write(jsonc, file = "C:/Temp/res_All.json")

listviewer::jsonedit(jsonc)

reqJson <- '{
    "minYear": 1909,
    "maxYear": 2013,
    "providers": [
        "Tasmania"
    ],
    "propertyGroups": [
        "3-0"
    ],
    "states": null,
    "bounds": [
        [
            [
                147.9569212046403,
                -40.021246219604436
            ],
            [
                147.9569212046403,
                -40.13983481753067
            ],
            [
                148.0719133795693,
                -40.08056633680312
            ],
            [
                147.9569212046403,
                -40.021246219604436
            ]
        ]
    ],
    "sites": [
        {
            "provider": "Tasmania",
            "sites": [
                "5246",
                "5124"
            ]
        }
    ],
    "useSDR": true
}'




makeBoundingBox()


bds <- vector(mode = 'list', length = 4)
bds[[1]] <- c(147.9569212046403, -40.021246219604436)
bds[[2]] <- c(147.9569212046403, -40.13983481753067)
bds[[3]] <- c(148.0719133795693, -40.08056633680312)
bds[[4]] <- c(147.9569212046403, -40.021246219604436)

# bds <- vector(mode = 'list', length = 4)
# bds[[1]] <- '147.9569212046403, -40.021246219604436'
# bds[[2]] <- '147.9569212046403, -40.13983481753067'
# bds[[3]] <- '148.0719133795693, -40.08056633680312'
# bds[[4]] <- '147.9569212046403, -40.021246219604436'


query <- list()
query$bounds[[1]] <- bds

cat(toJSON(query, auto_unbox = T), file = 'c:/temp/query.json')
jsnstr <- toJSON(query, auto_unbox = T)
NP(toJSON(query))

reqJson2 <- '{
    "minYear": 1909,
    "maxYear": 2013,
    "providers": [
        "Tasmania"
    ],
    "propertyGroups": [
        "3-0"
    ],
    "states": null,
    "bounds": [
        [
            [
                147.9569212046403,
                -40.021246219604436
            ],
            [
                147.9569212046403,
                -40.13983481753067
            ],
            [
                148.0719133795693,
                -40.08056633680312
            ],
            [
                147.9569212046403,
                -40.021246219604436
            ]
        ]
    ],
    "sites": [
        {
            "provider": "Tasmania",
            "sites": [
                "5246",
                "5124"
            ]
        }
    ],
    "useSDR": true
}'








minx=151.9
maxx=152
miny=-26.9
maxy=-27

apiCatalogueSummary()
head(apiProviderCatalogue(poviderNames='CSIRO_CSIS' ))

getAttributeTable(user=username, pwd=password, minx, maxx, miny, maxy, startYear=1900, endYear,  properties = c('6A1', '15A1'))



username = 'ross.searle@gmail.com'
password = 'RossTest29'
#password = 'bob'
apiAuthoriseMe(username, password)










































ajsn <- queryQuerySingleSite(providerID = 'CSIRO_CSIS', siteID = '1007', format = 'HTML' )


ado <- parseANSISJson(ansisResponse = ajsn)
generateSiteDescription(ado)




apiCatalogueSummary()

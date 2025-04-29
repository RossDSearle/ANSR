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

NP <- function(jsn){
  
  tf <- tempfile(fileext = '.json')
  cat(jsn, file = tf)
  shell(paste0('C:/LocalProgs/Notepad++/notepad++.exe ', tf), wait=F)
  unlink(tf)
}
  

############# Catalogue Summary  ############ 

### Returns a JSON array of metadata describing the available data catalogues, including ID, file size and the time of the last update. 


resp <- GET('https://apim-ansis-hrm-test-ae.azure-api.net/site-catalogue/v1/catalogue-summary')
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
jsonview::json_tree_view (jsn)

head(fromJSON(jsn))



############# Provider Catalogue ############# 

### Returns the catalogue for the provider specified in the {ID} parameter in JSON format. Valid IDs are provided via the “name” field of the Catalogue Summary methods response. 

resp <- GET('https://apim-ansis-hrm-test-ae.azure-api.net/site-catalogue/v1/provider/CSIRO_CSIS')
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
jsonview::json_tree_view (jsn)
head(fromJSON(jsn))


############# Property Definitions ############# 

### Returns a JSON object/dictionary documenting all valid enumerations used in the properties field used in ANSIS provider catalogues. Each enumeration is documented with its standard ANSIS json code/path, the property name and the ID of child members of that property. 

resp <- GET('https://apim-ansis-hrm-test-ae.azure-api.net/site-catalogue/v1/definitions')
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
jsonview::json_tree_view (jsn)
NP(jsn)
head(fromJSON(jsn))






####  Fetch a Single Site    ########

resp <- GET(url='https://apim-ansis-hrm-test-ae.azure-api.net/sdr-public/v1/SingleSite?provider=CSIRO_CSIS&site=1',
            add_headers(Authorization = paste0("Bearer ", auth$access_token)))
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
jsonview::json_tree_view (jsn)
siteL <- head(fromJSON(jsn, simplifyDataFrame=F))
siteL$data[[1]]$siteVisit[[1]]$soilProfile[[1]]$soilLayer[[1]]$colour[[1]]$result







resp <- POST(url='https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/create-query-request',
            body=qry,
            add_headers(Authorization = paste0("Bearer ", auth$access_token)))

jsnReqList <- fromJSON(reqJson, simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F)
resp
jsn <- content(resp, 'text', encoding = 'UTF8')

reqID <- str_split(jsn, ': ')[[1]][2]











outDir <- 'c:/temp/ansis'
if(!dir.exists(outDir)){ dir.create(outDir, recursive = T)}
files <- vector(mode = 'character', length = length(status$sdrRequest$files))
for (i in 1:length(status$sdrRequest$files)) {
  resp <- GET(url=paste0('https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/download-response?fileId=', status$sdrRequest$files[i] ),
              add_headers(Authorization = paste0("Bearer ", auth$access_token)))
  resp
  jsn <- content(resp, 'text', encoding = 'UTF8')
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




files <- c("C:/Temp/res_1.json", "C:/Temp/res_2.json", "C:/Temp/res_3.json")
jsonl <- lapply(files, function(f) rjson::fromJSON(file = f))
jsonc <- rjson::toJSON(jsonl)
write(jsonc, file = "C:/Temp/res_All.json")



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


minx=151
maxx=152
miny=-26
maxy=-27


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




username = 'ross.searle@gmail.com'
password = 'RossTest29'

tkn <- apiGenerateToken(user = username, pwd= password)


minx=151
maxx=152
miny=-26
maxy=-27

qry <- makeQuery(minx, maxx, miny, maxy, startYear=1900, endYear=NULL)

qryID <- apiCreateQuery(qryJSON=qry, accessToken=tkn)

apiRetrieveData(reqID=qryID, accessToken=tkn)




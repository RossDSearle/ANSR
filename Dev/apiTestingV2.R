resp <- GET('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/Authorise', add_headers(username=username, password=password))
jsn <- content(resp, 'text', encoding = 'UTF-8')
jl <- fromJSON(jsn)
tkn <- jl$access_token
tkn


resp <- GET('https://apim-ansis-app-prod-ac.azure-api.net/site-catalogue/v1/catalogue-summary')
resp
jsn <- content(resp, 'text', encoding = 'UTF-8')
listviewer::jsonedit(jsn)

resp <- GET('https://apim-ansis-app-prod-ac.azure-api.net/site-catalogue/v1/provider/CSIRO_CSIS')
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
listviewer::jsonedit(jsn)

resp <- GET('https://apim-ansis-app-prod-ac.azure-api.net/site-catalogue/v1/definitions')
resp
jsn <- content(resp, 'text', encoding = 'UTF-8')
listviewer::jsonedit(jsn)



resp <- GET(url='https://apim-ansis-app-prod-ac.azure-api.net/sdr-public/v1/SingleSite?provider=CSIRO_CSIS&site=1',
            add_headers(Authorization = paste0("Bearer ", tkn)))
resp
jsn <- content(resp, 'text', encoding = 'UTF-8')
listviewer::jsonedit(jsn)



minx=151.8
maxx=152
miny=-27
maxy=-26.2

qry <- makeQuery(minx, maxx, miny, maxy, startYear=1900, endYear=NULL)


resp <- POST(url='https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/create-query-request',
             body=qry,
             add_headers(Authorization = paste0("Bearer ", tkn)))
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
reqID <- str_split(jsn, ': ')[[1]][2]
reqID


stat <- paste0('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/get-query-request-status?requestId=', reqID)
resp <- httr::GET(url=stat, httr::add_headers(Authorization = paste0("Bearer ", tkn)))
resp
jsn <- content(resp, 'text', encoding = 'UTF-8')
jl <- fromJSON(jsn)
listviewer::jsonedit(jl)
fls <- jl$sdrRequest$files

stat <- paste0('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/get-all-query-requests-statuses') 
resp <- httr::GET(url=stat, httr::add_headers(Authorization = paste0("Bearer ", tkn)))
jsn <- content(resp, 'text', encoding = 'UTF-8')
jl <- fromJSON(jsn)
listviewer::jsonedit(jl)



resp <- GET(url=paste0('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/download-response?fileId=', fls[2] ),
            add_headers(Authorization = paste0("Bearer ", tkn)))
resp
jsn <- content(resp, 'text', encoding = 'UTF-8')
jl <- fromJSON(jsn)
listviewer::jsonedit(jl)


resp <- GET(url=paste0('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/cancel-query-request?requestId=', reqID),
            add_headers(Authorization = paste0("Bearer ", tkn)))
resp

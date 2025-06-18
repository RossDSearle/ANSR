library(httr)

url <- 'https://b2cansisappprodac.b2clogin.com/b2cansisappprodac.onmicrosoft.com/B2C_1A_ROPC_AUTH/oauth2/v2.0/token?grant_type=password'

bdyL <- list()
bdyL$username = 'james.moloney@csiro.au'
bdyL$password = 'TestPassword2'

bdyL$username = 'ross.searle@gmail.com'
bdyL$password = 'Rossiscool01'


bdyL$scope = 'openid 44040d13-769e-4567-8bbe-b2109b42c939'
bdyL$client_id = '44040d13-769e-4567-8bbe-b2109b42c939'
bdyL$response_id = 'token id_token'

#jsnb = jsonlite::toJSON(bdyL)

resp <- httr::POST(url=url, body=bdyL, encode='form')
jsn <- httr::content(resp, 'text', encoding = 'UTF8')
auth = jsonlite::fromJSON(jsn)
auth


urlauth <- 'https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/Authorise'
resp <- httr::GET(url=urlauth, add_headers(username = 'ross.searle@csiro.au', password='Rossiscool01'))

resp <- httr::GET(url=urlauth, add_headers(username = 'james.moloney@csiro.au', password='TestPassword2'))
jsn <- httr::content(resp, 'text', encoding = 'UTF8')
auth = jsonlite::fromJSON(jsn)
auth




bdy <- '{"minYear":null,"maxYear":null,"providers":null,"propertyGroups":null,"states":null,"bounds":null,"sites":[{"provider":"WesternAustralia","sites":["50407"]}],"useSDR":true}'
resp <- POST(url='https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/create-query-request',
             body=qry,
             add_headers(Authorization = paste0("Bearer ", auth$access_token)))
resp
jsn <- content(resp, 'text', encoding = 'UTF8')

reqID <- 'afe68008-c805-497b-8332-01637d5da0e4'

stat <- paste0('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/get-query-request-status?requestId=', reqID)
resp <- GET(url=paste0(stat), add_headers(Authorization = paste0("Bearer ", auth$access_token)))
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
status <- jsonlite:: fromJSON(jsn)
listviewer::jsonedit(status)


stat <- paste0('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/get-all-query-requests-statuses')
resp <- GET(url=paste0(stat), add_headers(Authorization = paste0("Bearer ", auth$access_token)))
resp
jsn <- content(resp, 'text', encoding = 'UTF8')
status <- jsonlite:: fromJSON(jsn)
listviewer::jsonedit(status)


resp <- GET(url=paste0('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/download-response?fileId=', status$sdrRequest$files[1] ),
            add_headers(Authorization = paste0("Bearer ", auth$access_token)))
resp
jsn <- content(resp, 'text', encoding = 'UTF8')



url=paste0( 'https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/cancel-query-request?requestId=', reqID)
resp <- httr::POST(url=url, httr::add_headers(Authorization = paste0("Bearer ", auth$access_token)))
jsn <- httr::content(resp, 'text', encoding = 'UTF8')
jj <- jsonlite::fromJSON(jsn)




providerID='CSIRO_CSIS' 
siteID=1
url <- paste0('https://apim-ansis-app-prod-ac.azure-api.net/ansis-external-api/query-requests/v2/SingleSite?provider=' , providerID, '&site=', siteID )
resp <- httr::GET(url=url, httr::add_headers(Authorization = paste0("Bearer ", auth$access_token)))
jsn <- httr::content(resp, 'text', encoding = 'UTF8')
listviewer::jsonedit(jsn)

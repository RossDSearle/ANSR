


##############  Generate a token    ######################


apiGenerateToken <- function(user, pwd){
  
    url <- 'https://b2cansishrmtestae.b2clogin.com/b2cansishrmtestae.onmicrosoft.com/B2C_1A_ROPC_AUTH/oauth2/v2.0/token?grant_type=password'
    bdyL <- list()
    bdyL$username = user
    bdyL$password = pwd
    bdyL$scope = 'openid 44040d13-769e-4567-8bbe-b2109b42c939'
    bdyL$client_id = '44040d13-769e-4567-8bbe-b2109b42c939'
    bdyL$response_id = 'token id_token'
    
    resp <- POST(url=url, body=bdyL, encode='form')
    jsn <- content(resp, 'text', encoding = 'UTF8')
    auth = fromJSON(jsn)
    
    return(auth$access_token)
}


apiCreateQuery <- function(qryJSON, accessToken){
  
  resp <- POST(url='https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/create-query-request',
               body=qryJSON,
               add_headers(Authorization = paste0("Bearer ", accessToken)))
  jsn <- content(resp, 'text', encoding = 'UTF8')
  reqID <- str_split(jsn, ': ')[[1]][2]
  return(reqID)
}



apiQueryStatus <- function(reqID, accessToken){
  
    inProgress=T  
    
    iter = 0
      
    while (inProgress) {
      iter = iter+1
      cat('Querying ANSIS Server ...', iter)
      
      Sys.sleep(1)
      resp <- GET(url=paste0('https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/get-query-request-status?requestId=', reqID),
                  add_headers(Authorization = paste0("Bearer ", accessToken)))
      jsn <- content(resp, 'text', encoding = 'UTF8')
      jsnList <- fromJSON(jsn, simplifyDataFrame = F)
      
      status <- fromJSON(content(resp, 'text', encoding = 'UTF8'), simplifyDataFrame = F)

      if(status$sdrRequest$status=='Completed'){
        cat('\n\nQuery completed...\n\n')
        inProgress=F  
      }
    }
    
    return(status$sdrRequest$files)
}

apiDownloadResponse <- function(fls, accessToken){
  
  outDir <- paste0(tempdir(), '/tmp_', as.numeric(Sys.time()))
  #outDir <- 'c:/temp/ansis'
  if(!dir.exists(outDir)){ dir.create(outDir, recursive = T)}
  files <- vector(mode = 'character', length = length(fls))
  for (i in 1:length(fls)) {
    resp <- GET(url=paste0('https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2/download-response?fileId=', fls[i] ),
                add_headers(Authorization = paste0("Bearer ", accessToken)))
    resp
    jsn <- content(resp, 'text', encoding = 'UTF8')
    files[[i]] <- jsn
    fn <- paste0(outDir, '/res_', i, '.json')
    cat(jsn, file = fn)
  }
  
 jl <-  mergeResponseFiles(outDir)
 
 return(jl)
}

apiRetrieveData <- function(reqID, accessToken){
  
 fls <- apiQueryStatus(reqID, accessToken)
  
 ad <- apiDownloadResponse(fls, accessToken)
 return(ad)
  
}


getANSISData <- function(usr, bob ){
  
}



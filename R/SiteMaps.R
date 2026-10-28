

getSingleSite2 <- function(providerID, siteID){

  if(!checkIfAuthorised()){return(cat(''))}
  print(providerID)
  print(siteID)

  url <- paste0(Constants@ANSISAPIurlProd, '/sdr-public/v1/SingleSite?provider=' , providerID, '&site=', siteID )
  print(url)
  resp <- httr::GET(url=url, httr::add_headers(Authorization = paste0("Bearer ", authANSIS@Token)))
  jsn <- httr::content(resp, 'text', encoding = 'UTF-8')
  #jf <- tempfile(pattern = 'ANSIS_Site', fileext = '.json')
  jf <- 'c:/temp/site.json'
  cat(jsn, file=jf)
  df <- process_json_fileAll(file_path=jf)
  df <- df %>% arrange(SiteID, endAtTime, fieldDepth, Property , depthUpper, depthLower )
  print(head(df))
  return(df)
}

getSitesTable <- function(providerNames){

  sdf <-  apiProviderCatalogue(providerNames = providerNames, includeProperties = T)
}

# constants <- list()
# constants$ANSISguiURL <- 'https://app-ansis-hrm-portal-test-ae.azurewebsites.net/'
# #constants$ANSISAPIurl <- 'https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2'
# constants$ANSISAPIurlV1 <- 'https://apim-ansis-hrm-test-ae.azure-api.net/site-catalogue/v1'
# constants$ANSISAPIurlV2 <- 'https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2'





setClass("Authorisation", 
         slots = c(
           usr = "character", 
           pwd = "character",
           Authorised = 'logical',
           Token = 'character',
           TokenExpiry = 'POSIXct',
           DataStorePath = 'character'
           
         )
)


setClass("Constants", 
         slots = c(
           ANSISAPIurlDev = "character", 
           ANSISAPIurlProd = "character",
           ANSISguiURL = 'character'      
         )
)

setClass("DataSets", 
         slots = c(
           mps = "data.frame", 
           CodesTable = "data.frame", 
           labcodesMapping = "data.frame"
         )
)





                  

# mps <- getPackageData_mps()             
# CodesTable <- getPackageData_CodesTable()


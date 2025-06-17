.onAttach <- function(lib, pkg) {
  
  
  
  DataSets <<- new("DataSets",
                 mps = getPackageData_mps(),
                 CodesTable = getPackageData_CodesTable(),
                 labcodesMapping = getPackageData_labcodesMapping()

)
  
  Constants <<- new("Constants", 
                    ANSISAPIurlV1 =  'https://apim-ansis-hrm-test-ae.azure-api.net/site-catalogue/v1',
                    ANSISAPIurlV2 =  'https://apim-ansis-hrm-test-ae.azure-api.net/ansis-external-api/query-requests/v2',
                    ANSISguiURL = 'https://app-ansis-hrm-portal-test-ae.azurewebsites.net/'
  )
     
  mps <<- getPackageData_mps()
  CodesTable <<- getPackageData_CodesTable()
  labcodesMapping <<- getPackageData_labcodesMapping()
  
  
  
  msg <- paste0(crayon::bold( crayon::green( paste0("\n\n#########     Welcome to the ANSR Package     ##########\n\n"))),
                crayon::bold( crayon::blue(paste0('This package is designed for interacting with the Australian National Soil Information System (ANSIS).
                                                  \nBefore any of the API functions will work you need to authorise access using the AuthoriseMe() function\nusing your ANSIS account Username and Password.\n\n'), paste0(crayon::magenta('Visit https://ansis.net\n\n')))))
  


  
  packageStartupMessage(msg)
  

}

.onAttach <- function(lib, pkg) {



  DataSets <<- new("DataSets",
                 mps = getPackageData_mps(),
                 CodesTable = getPackageData_CodesTable(),
                 labcodesMapping = getPackageData_labcodesMapping()

)

  Constants <<- new("Constants",
                    ANSISAPIurlDev =  '',
                    ANSISAPIurlProd =  'https://apim-ansis-app-prod-ac.azure-api.net',
                    ANSISguiURL = 'https://portal.ansis.net/'
  )

  mps <<- getPackageData_mps()
  CodesTable <<- getPackageData_CodesTable()
  labcodesMapping <<- getPackageData_labcodesMapping()



  msg <- paste0(crayon::bold( crayon::green( paste0("\n\n#########     Welcome to the ANSR Package     ##########\n\n"))),
                crayon::bold( crayon::blue(paste0('This package is designed for interacting with the Australian National Soil Information System (ANSIS).
                                                  \nBefore any of the API functions will work you need to authorise access using the apiAuthoriseMe() function\nusing your ANSIS account Username and Password. You also need to supply a DataStore path.\n\n'), paste0(crayon::magenta('Visit https://ansis.net\n\n')))))




  packageStartupMessage(msg)


}

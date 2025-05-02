.onAttach <- function(lib, pkg) {
  packageStartupMessage(paste0("\n\nWelcome to the ANSR package\n\n
                        Before any of the API functions will work you need to authorise access using the AuthoriseMe() function\n
                        using your ANSIS account Username and Password\n\n"))
  
  mps <<- getPackageData_mps()
  CodesTable <<- getPackageData_CodesTable()
}

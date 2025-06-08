.onAttach <- function(lib, pkg) {
  
  msg <- paste0(crayon::bold( crayon::green( paste0("\n\nWelcome to the ANSR package\n\n"))),
                crayon::bold( crayon::blue(paste0('Before any of the API functions will work you need to authorise access using the AuthoriseMe() function\nusing your ANSIS account Username and Password\n\n')))
  )

  
  packageStartupMessage(msg)
  
  mps <<- getPackageData_mps()
  CodesTable <<- getPackageData_CodesTable()
}

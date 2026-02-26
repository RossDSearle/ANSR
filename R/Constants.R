

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


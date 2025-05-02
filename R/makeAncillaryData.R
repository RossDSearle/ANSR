 # mps <<- read.csv('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/schemaFieldMapping2.csv')
 # CodesTable <<- read.csv('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ShowANSISSites2/ANSISCodes.csv')
 # ansisExampleObject <- readRDS('C:/Projects/ANSIS/V2/Responses/medium.rds')
 # usethis::use_data(mps, CodesTable, ansisExampleObject, internal = TRUE, overwrite = T)


#  Can't seem to get the sysdata.rda to work so resorting to below - saving the required dfs as text in packageData.R and reading 
#   in from functions in startUpMessage.R

# dput(mps, file='c:/temp/mps.txt')
# dput(CodesTable, file='c:/temp/CodesTable.txt')

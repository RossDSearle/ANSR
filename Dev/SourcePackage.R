
source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Packages/ANSR/R/Constants.R')
source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Packages/ANSR/R/ansisPropertCodes.R')
source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Packages/ANSR/R/parseANSISJson.R')
source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Packages/ANSR/R/API.R')
source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Packages/ANSR/R/apiHelpers.R')
source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Packages/ANSR/R/parsingHelpers.R')
source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Packages/ANSR/R/siteParsing.R')
source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Packages/ANSR/R/packageData.R')



mps <<- getPackageData_mps()
CodesTable <<- getPackageData_CodesTable()
CodesTable <<- getPackageData_CodesTable()


# DataSets <<- new("DataSets",
#                  mps = getPackageData_mps(),
#                  CodesTable = getPackageData_CodesTable(),
#                  labcodesMapping = getPackageData_labcodesMapping()
# )

#source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Packages/ANSR/R/startUpMessage.R')


#apiAuthoriseMe(username = 'ross.searle@gmail.com', password = 'RossTest29')
apiAuthoriseMe(username = 'ross.searle@csiro.au', password = cp, DataStorePath  = 'c:/temp/Ansis/Data' )

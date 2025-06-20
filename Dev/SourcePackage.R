
source('./R/Constants.R')
source('./R/ansisPropertCodes.R')
source('./R/parseANSISJson.R')
source('./R/API.R')
source('./R/apiHelpers.R')
source('./R/parsingHelpers.R')
source('./R/siteParsing.R')
source('./R/packageData.R')
source('./R/QueryCache.R')



mps <<- getPackageData_mps()
CodesTable <<- getPackageData_CodesTable()
CodesTable <<- getPackageData_CodesTable()


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


# DataSets <<- new("DataSets",
#                  mps = getPackageData_mps(),
#                  CodesTable = getPackageData_CodesTable(),
#                  labcodesMapping = getPackageData_labcodesMapping()
# )

#source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Packages/ANSR/R/startUpMessage.R')


#apiAuthoriseMe(username = 'ross.searle@gmail.com', password = 'RossTest29')
#apiAuthoriseMe(username = 'ross.searle@csiro.au', password = cp, DataStorePath  = 'c:/temp/Ansis/Data' )
apiAuthoriseMe(username = 'ross.searle@csiro.au', password = cp, DataStorePath  = '/home/kubeflow/Data' )

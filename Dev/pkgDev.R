
ANSR::apiAuthoriseMe(username = 'ross.searle@gmail.com', password = 'RossTest29')


####   Return a single site   #######
ao <- queryQuerySingleSite(providerID='CSIRO_CSIS', siteID=1, format = "ANSISDataObject")
ao <- queryQuerySingleSite(providerID='CSIRO_CSIS', siteID=1, format = "HTML") # need to be finished


##########################  ANSIS Metadata  ###################################

#### Show provider info
apiCatalogueSummary()

#### Provider Catalogue 
### Returns the catalogue for the provider specified in the {ID} parameter in JSON format. Valid IDs are provided via the “name” field of the Catalogue Summary methods response. 
piDF <- apiProviderCatalogue('CSIRO_CSIS')
min(piDF$StartYear)
max(piDF$EndYear)





#' Get ANSIS Soil Property Codes
#' @param soilProperty The broadest grouping of soil properties. ie at the elemental level eg 'Nitrogen'.
#' @param propertyName A broad laboratory method name.
#' @param labCode A Green Book lab method code.

#' @details ANSIS has its own specific code to identify soil properties. The information in this table is used to map to these codes to allow querying of the ANSIS data repository
#' @author Ross Searle
#' @return data.frame
#' @export


getAnsisPropertyCodes <- function(soilProperty=NULL, propertyName=NULL, labCode=NULL){
  
  if(is.null(soilProperty) & is.null(propertyName) & is.null(labCode)){
    return(NULL)
    
  }else if(!is.null(soilProperty)){
    
    idxs <- which(DataSets@labcodesMapping$Property  == soilProperty)
    ansisProperties <- DataSets@labcodesMapping$ANSISCode[idxs] 
    return(unique(ansisProperties))
    
  }else if(!is.null(propertyName)){
    
    idxs <- which(DataSets@labcodesMapping$PropertyName == propertyName)
    ansisProperties <- DataSets@labcodesMapping$ANSISCode[idxs]    
    return(unique(ansisProperties))
    
  }else if(!is.null(labCode)){
    idxs <- which(DataSets@labcodesMapping$LabCode==labCode)
    ansisProperties = DataSets@labcodesMapping$ANSISCode[idxs]
    return(unique(ansisProperties))
  }else{
    return(NULL)
  }
  
  
  
}


#' Show ANSIS Property Code Mappings
#' @param soilProperty The broadest grouping of soil properties. ie at the elemental level eg 'Nitrogen'.
#' @param propertyName A broad laboratory method name.
#' @param labCode A Green Book lab method code.

#' @details ANSIS has its own specific code to identify soil properties. The information in this table is used to map to these codes to allow querying of the ANSIS data repository
#' @author Ross Searle
#' @return data.frame
#' @export


showAnsisPropertyCodes <- function(soilProperty=NULL, propertyName=NULL, labCode=NULL){
  
  if(is.null(soilProperty) & is.null(propertyName) & is.null(labCode)){
    ansisProperties <- DataSets@labcodesMapping
    
  }else if(!is.null(soilProperty)){
    
    idxs <- which(DataSets@labcodesMapping$Property == soilProperty)
    ansisProperties <- DataSets@labcodesMapping[idxs,]    
    
  }else if(!is.null(propertyName)){
    
    idxs <- which(DataSets@labcodesMapping$PropertyName == propertyName)
    ansisProperties <- DataSets@labcodesMapping[idxs,]    
    
  }else if(!is.null(labCode)){
    idxs <- which(DataSets@labcodesMapping$LabCode==labCode)
    ansisProperties = DataSets@labcodesMapping[idxs,]
  }
  
  
  return(ansisProperties)
  
}







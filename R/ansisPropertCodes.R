
getAnsisPropertyCodes <- function(soilProperty=NULL, propertyName=NULL, labCode=NULL){
  
  if(is.null(soilProperty) & is.null(propertyName) & is.null(labCode)){
    return(NULL)
    
  }else if(!is.null(soilProperty)){
    
    idxs <- which(DataSets@labcodesMapping$SoilProperty  == soilProperty)
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


showAnsisPropertyCodes <- function(soilProperty=NULL, propertyName=NULL, labCode=NULL){
  
  if(is.null(soilProperty) & is.null(propertyName) & is.null(labCode)){
    ansisProperties <- DataSets@labcodesMapping
    
  }else if(!is.null(soilProperty)){
    
    idxs <- which(DataSets@labcodesMapping$SoilProperty == soilProperty)
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







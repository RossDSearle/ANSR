
library(ANSR)
aprops <- apiPropertyDefinitions()


physchemSchemaTable <- read.csv('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Packages/ANSR/inst/extdata/ANSISchem_physProperties.csv')

physchemSchemaTable$ObservableProperty <- stringr::str_trim(physchemSchemaTable$ObservableProperty)

mdf <- merge(aprops, physchemSchemaTable, by.x = 'Name', by.y = 'ObservableProperty', all.x = T)
mdf$lab_mcode <- basename(mdf$Procedure.Identifier)
head(mdf)

unique(mdf$PropType)


write.csv(mdf, 'c:/temp/shemaMappings.csv')


## remove quotes from text
df <- read.csv('c:/temp/shemaMappings.csv')
df[] <- lapply(df, gsub, pattern='"', replacement='')
df
df$lab_mcode <- basename(df$Procedure.Identifier)

colnames(df) <- c("PropertyName","PropType","Property","ANSISCode","Procedure_Identifier","Result_Type","Unit","Collection","Notation","Part_of_soil_fraction","LabCode")
write.csv(df, 'c:/temp/shemaMappings3.csv', row.names = F)

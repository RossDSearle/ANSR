library(ANSR)
library(collapse)


d <- readRDS('C:/Projects/ANSIS/allANSIS.rds')

collapse::get_elem(d$data[1:100], 'ph')

defs <- apiPropertyDefinitions()

write.csv(defs, 'c:/temp/schemaMapping.csv', row.names = F)




s <- seq(1:length(d$data))
idxs <- sample(s, 100000)

sl <- d$data[idxs]

df <- unlist2d(sl)
head(df)

flds <- unique(df$.id.8)

odf <- data.frame()

for (i in 1:length(flds)) {
  rec <- flds[i]
 idxs <- which(df$.id.8 == rec & df$.id.10=='usedProcedure')
 
 if(length(idxs) > 0){
  sdf <- df[idxs, ] 
  meths <- unique(sdf$V1)
  mdf <- data.frame(prop = rec, meths=meths)
  odf <- rbind(odf, mdf)
 }
  
}


### remove numeric (incorrect) vals from meths
idxs <- which(is.na(as.numeric(odf$meths)))
odf2 <- odf[idxs,]

### remove prefixes from lab methods
head(odf2)
odf2$meths <- str_remove(odf2$meths, 'scm:')
odf2$meths <- str_remove(odf2$meths, 'spmile:')

write.csv(odf2, 'C:/Projects/ANSIS/shemaMappings/LabSchemaMethods.csv', row.names = F)



#### get names for manually matching to anis PropertyDefinitions to produce "LabSchemaMapping.csv" file
### Schema Methods and ANSIS propertyDefs mapped manually
props <- sort(unique(odf2$prop))



### Merge the two bits to produce the overall lab mapping file for use in ANSIS API lab queries 
sch <- read.csv('C:/Projects/ANSIS/shemaMappings/LabSchemaMapping.csv')
odf2 <- read.csv('C:/Projects/ANSIS/shemaMappings/LabSchemaMethods.csv')
head(sch)
head(odf2)
mdf <- merge(odf2, sch, by.x = 'prop', by.y = 'SchemaName')
write.csv(mdf, 'C:/Projects/ANSIS/shemaMappings/LabANSISSchemaMethodsMappings.csv')

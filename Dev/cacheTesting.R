dsm <- getDSMtable(Name='ANSISDemo3', Description = "This is a description of the purpose of the query", minx=150, maxx=152, miny=-27, maxy=-25, propertyName='Concentration of organic Carbon')


ShowCachedQueries()
cacheShowFileSizes(T)
cacheDescribe()


dsmrepeat <- getDSMtable(Name='ANSISDemoBig1', Description = "This is a description of the purpose of the query", minx=141.1, maxx=152.1, miny=-25.4, maxy=-25.1, propertyName='Concentration of organic Carbon')
head(dsmrepeat)



emptyCache()

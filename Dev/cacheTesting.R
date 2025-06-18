dsm <- getDSMtable(Name='ANSISDemo3', Description = "This is a description of the purpose of the query", minx=150, maxx=152, miny=-27, maxy=-25, propertyName='Concentration of organic Carbon')


showCacheData()
showCacheFileSizes(T)


dsmrepeat <- getDSMtable(Name='ANSISDemo77', Description = "This is a description of the purpose of the query", minx=151.7, maxx=152, miny=-25.1, maxy=-25, propertyName='Concentration of organic Carbon')
head(dsmrepeat)



emptyCache()

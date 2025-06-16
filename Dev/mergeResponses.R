

list1 <- list(a = 1, b = 2)
list2 <- list(b = 3, c = 4)
# Using a custom function to handle duplicates
merge_lists <- function(list1, list2) {
  combined <- c(list1, list2)
  unique_names <- unique(names(combined))
  return(combined[unique_names])
}

merge_lists(list1, list2)

fls <- c('C:/Temp/ansis/res_1.json','C:/Temp/ansis/res_2.json')

l1 <- jsonlite::fromJSON('C:/Temp/ansis/res_1.json', simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F)
l2 <- jsonlite::fromJSON('C:/Temp/ansis/res_2.json', simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F)

ol <- list()
#ol['$schema'] <- "https://anzsoildata.github.io/def-au-schema-json/schema/domain/2023-07-31/ansis.json"
#ol$data <-list()
ol$included <- list()
ol$included$organizations <- list()
ol$included$persons <- list()
ol$included$projects <- list()
ol$meta <- list()

persons <- list()
orgs <- list()
projs <- list()

for (i in 1:length(fls)) {
  jl <- jsonlite::fromJSON(fls[i], simplifyDataFrame = F, simplifyVector = F, simplifyMatrix = F)
  
  for (j in 1:length(jl$included$persons)) {
    pid <- jl$included$persons[[j]]$id
    if(!pid %in% persons){
      pnl <- vector(mode = 'list', length=1)
      pnl[[1]] <- jl$included$persons[[j]]
      ol$included$persons <- append(ol$included$persons, pnl)
      persons <- append(persons, pid)
    }
  }
  
  for (j in 1:length(jl$included$organizations)) {
    oid <- jl$included$organizations[[j]]$id
    if(!oid %in% orgs){
      onl <- vector(mode = 'list', length=1)
      onl[[1]] <- jl$included$organizations[[j]]
      ol$included$organizations <- append(ol$included$organizations, onl)
      orgs <- append(orgs, oid)
    }
  }
  
  for (j in 1:length(jl$included$projects)) {
    prid <- jl$included$projects[[j]]$id
    if(!prid %in% projs){
      prnl <- vector(mode = 'list', length=1)
      prnl[[1]] <- jl$included$projects[[j]]
      ol$included$projects <- append(ol$included$projects, prnl)
      projs <- append(projs, prid)
    }
  }
  
}

listviewer::jsonedit(ol)

### Make name tag of first three alphabets of species name and variant/subspecies name
### e.g., ANS_mic

makeTag <- function(data, # vector of full species names 
         genus_name # genus name should be got rid of
){
  ### Import species name codes
  spname <- grepl(genus_name, data) %>% data[.]
  codes <- gsub(paste(genus_name, "_", sep=""), "", spname) %>% 
    gsub("subsp._", "", .) %>% 
    gsub("var._", "", .)
  
  spname <- (codes %>% substring(., 1, last = 3) %>% mutate(as_tibble(spname), tag = .))
  colnames(spname)[1] <- "X"
  subsp <- codes %>% 
    strsplit(., "_") %>% 
    lapply(., function(x){
      ifelse(is.na(x[2]), "", x[2])
    }) %>% 
    substring(., 1, last = 3)
  
  spname <- lapply(1:length(subsp), function(i){
    paste(spname[i,"tag"], subsp[i], sep = "_")
  }
  ) %>% unlist %>% 
    gsub("_$", "", .) %>% 
    mutate(spname, tag = .) 
  
  return(spname)
  
}



makeTag_separate <- function(data, # vector of full species names 
                             genus_name, # genus name should be got rid of
                             separate # syntax which separates genus and species names e.g. "_" between "Acaena_agnipila"
){
  ### Import species name codes
  spname <- grepl(genus_name, data) %>% data[.]
  codes <- gsub(paste(genus_name, separate, sep=""), "", spname) %>% 
    gsub(paste("subsp.", separate, sep=""), "", .) %>% 
    gsub(paste("var.", separate, sep=""), "", .)
  
  spname <- (codes %>% substring(., 1, last = 3) %>% mutate(as_tibble(spname), tag = .))
  colnames(spname)[1] <- "X"
  subsp <- codes %>% 
    strsplit(., separate) %>% 
    lapply(., function(x){
      ifelse(is.na(x[2]), "", x[2])
    }) %>% 
    substring(., 1, last = 3)
  
  spname <- lapply(1:length(subsp), function(i){
    paste(spname[i,"tag"], subsp[i], sep = "_")
  }
  ) %>% unlist %>% 
    gsub(paste(separate, "$", sep=""), "", .) %>% 
    mutate(spname, tag = .) 
  
  return(spname)
  
}





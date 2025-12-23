multi_table=function(df){
  
  multi_length=lapply(df,length)
  multi_lvec=NULL
  for (i in 1:length(multi_length)){
    if(is.null(multi_lvec)){
      multi_lvec=multi_length[[i]][1]
    } else {
      multi_lvec=c(multi_lvec,multi_length[[i]][1])
    }
  }
  multi_lmax=max(multi_lvec)
  
  multi=NULL
  for (j in 1:multi_lmax) {
    
    for (i in 1:length(df)) {
      if(is.null(multi)){
        multi=df[[i]][j]
      } else {
        multi=c(multi,df[[i]][j])  
      }
      
    }
    
  }
  
  
  
  return(multi)
}
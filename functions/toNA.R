toNA <-
function(df,vars){
    
    for(i in vars){
      
      if(is(df[[i]],"POSIXct")){
        next
        
      } else {
        
        df[[i]]=replace(df[[i]], df[[i]]=="---", NA)
        
      }
    
    }
    

    return(df)
  }

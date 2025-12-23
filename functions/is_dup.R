is_dup <-
function(df,index){
  
  if (purrr::is_empty(index)){
    return(df)
  } else{
    return(df[-index,])
  }
}

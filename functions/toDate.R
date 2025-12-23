toDate <-
function(df, vars, dorigin){
  for (i in vars) {
    df[[i]]=as.Date(as.numeric(df[[i]]), origin=paste(dorigin))
  }
  return(df)
}

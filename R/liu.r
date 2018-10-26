round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

trans_py = function(df, pyColumn){
  
  old <- c("ju","qu","xu","y","w")
  new <- c("jv","qv","xv","i","u")
  
  old1 <- c("uu","ii","yu")
  new1 <- c("u","i","v")
  
  
  py_trans = stringr::str_replace_all(df[,pyColumn],setNames(new, old))
  py_trans = stringr::str_replace_all(py_trans,setNames(new1, old1))
  return = cbind(df, py_trans)
  
}
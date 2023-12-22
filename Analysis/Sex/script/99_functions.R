# creates necessary rows and columns if does not exist and add the entry
add_entry<-function(df,algoname,dataname,value){
  # as df is a list element unwrap it
  df <- df[[1]]
  if (is.null(df)){
    df <- tibble()
  }
  if (!('algo' %in% colnames(df))){
    df <- df %>% add_column(algo = NA_character_ )
  }
  if (!(algoname %in% df[,'algo'][[1]])){
    df <- df %>% add_row(algo = as.character(algoname) )
  }
  if (!(dataname %in% colnames(df))){
    df <- df %>% add_column("{dataname}" := NA_character_ )
  }
  print(df)
  df[df['algo'] == algoname,dataname] = value
  print(df)
  #wrap df in list element
  return(list(df))
}

R2_Score <- function(y_pred, y_true) {
  R2_Score <- 1 - var(y_true - y_pred) /var(y_true)
  return(R2_Score)
}
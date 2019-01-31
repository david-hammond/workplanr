#' Save project information
#'
#' @param df Project table to save
#' @param folder Location to save Project Tables
#' @return NULL

.db.save = function(df, folder = "./db/"){
  if(!dir.exists(folder)){
    dir.create(folder)
  }
  
  fname <- paste0(folder, (deparse(substitute(df))), ".RData")
  save(df, file = fname)
  return(NULL)
}

#' Retrieve project information
#'
#' @param db Project table to retrieve
#' @param folder Location ofProject Tables
#' @return NULL
.db.get = function(db, folder = "./db/"){
  if(!dir.exists(folder)){
    message("Employee database does not exist, please refer to the manual to create")
    db = NULL
  }else{
    fname <- paste0(folder, db, ".RData")
    load(fname)
    db = get("df")
  }
  return(db)
}
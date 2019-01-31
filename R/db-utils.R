.db.save = function(df, folder = "./db/"){
  if(!dir.exists(folder)){
    dir.create(folder)
  }
  fname <- paste0(folder, (deparse(substitute(df))), ".RData")
  save(df, file = fname)
}

.db.get = function(db, folder = "./db/"){
  if(!dir.exists(folder)){
    message("Employee database does not exist, please refer to the manual to create")
    db = NULL
  }else{
    fname <- paste0(folder, db, ".RData")
    load(fname)
    db = get(db)
  }
  return(db)
}
# file includes all mongoDB related functions

# set mongoDB credentials in environment
username <- Sys.getenv("MONGO_DB_USERNAME")
password <- Sys.getenv("MONGO_DB_PASSWORD")

# function pushes a data frame to mongo and clears the previous related data
#' @param tour character input variable, default is 'pga', options are pga, euro, alt, opp, kft
#' @param df data frame to be pushed to mongoDB
#' @param db database within mongoDB
#' @param collection collection name within mongoDB database
push_ticket_to_mongo <- function(df = data.frame(), db = as.character(), collection = as.character()) {
  
  tryCatch({
    
    conn = mongo(
      collection = collection,
      db = db,
      url = paste0("mongodb+srv://", username, ":", password, "@golfrawdata.wnwpq.mongodb.net/?retryWrites=true&w=majority"),
      verbose = FALSE
    )
    
    conn$insert(df)
    
  }, error = function(e){"Error in push to mongo"})
}

# function pulls data frame from mongoDB
#' @param tour character input variable, default is 'pga', options are pga, euro, alt, opp, kft
#' @param db database within mongoDB
#' @param collection collection name within mongoDB database
pull_from_mongo <- function(tour = "pga", db = as.character(), collection = as.character()) {
  
  tryCatch({
    
    if (is.na(tour)) {
      
      conn = mongo(
        collection = collection,
        db = db,
        url = paste0("mongodb+srv://", username, ":", password, "@golfrawdata.wnwpq.mongodb.net/?retryWrites=true&w=majority"),
        verbose = FALSE
      )
      
      df <- conn$find()
      
    } else {
      
      conn = mongo(
        collection = collection,
        db = db,
        url = paste0("mongodb+srv://", username, ":", password, "@golfrawdata.wnwpq.mongodb.net/?retryWrites=true&w=majority"),
        verbose = FALSE
      )
      query = paste0('{"tour" : "', tour, '"}')
      df <- conn$find(query)
      
    }
    
    return(df)
    
  }, error = function(e){})
  
}

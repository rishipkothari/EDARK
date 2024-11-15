#### DB interface function defintions ####

create_db_connection_toad <- function(){
  user_prompt <- rstudioapi::showPrompt(title="TOAD username", message="Enter username", default="")
  if(is.null(user_prompt)) {
    showNotification("TOAD connection cancelled", type="warning")
    return(NULL)
  }
  tryCatch({
    con <- DBI::dbConnect(RPostgres::Postgres()
                          , host='QCTOADLDB001.som.ucsf.edu'
                          , port='5432'
                          , dbname='toad'
                          , user=user_prompt
                          , password=rstudioapi::askForPassword("TOAD password")
                          # , timezone = Sys.timezone()
    )
    return(con)
  }, error = function(e){
    showNotification("TOAD connection failed", type="error")
    return(NULL)
  })
}

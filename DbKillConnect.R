DbKillConnect <- function() {
    # Summary: remove all active postgres 
    # database connections
    
    # Output: a workspace with no active connections
    # to the PG database
    
    # Example
    # DbPGConnect(pg_uid = "analagossai", 
    #             pg_pwd = "password")
    # DbKillConnect()
    require(purrr)
    require(DBI)
    
    map(dbListConnections(PostgreSQL()), 
        dbDisconnect)
}

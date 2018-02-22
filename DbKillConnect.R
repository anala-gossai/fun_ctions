DbKillConnect <- function() {
    # Summary: remove all active postgres 
    # database connections
    
    # Output: a workspace with no active connections
    # to the PG database
    
    # Example
    # DbPGConnect(pg_uid = "analagossai", 
    #             pg_pwd = "password")
    # DbKillConnect()
    require(RPostgreSQL) # Version ‘0.4.1’
    require(purrr)
    require(DBI)
    
    map(dbListConnections(PostgreSQL()), 
        dbDisconnect)
}

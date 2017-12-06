DbPGConnect <- function(pg_database = "postgres",
                        pg_hostname = "localhost",
                        pg_port = "5432",
                        pg_uid = "analagossai",
                        pg_pwd) {
    # Summary: Connect to postgres databases
    
    # Arg:
    #   pg_database: name of the PG database; defaults to "postgres" 
    #   pg_hostname: name of the database host 
    #                (e.g., "aws-us-east-1-portal.com");
    #                defaults to "localhost"    
    #   pg_port: name of the PG database port (e.g., "11101");
    #            defaults to "5432" 
    #   pg_uid: username for the PG database (e.g., "admin" or "analagossai");
    #           defaults to my username
    #   pg_pwd: password for the PG database (e.g., "password")
    
    # Output: an active connection to the postgres database 
    
    # Example:
    # library(RPostgreSQL)
    # DbPGConnect(pg_uid = "analagossai", 
    #             pg_pwd = "password")
    # dbGetQuery(conn, "SELECT * FROM practice_tests.user_register")
    # dbDisconnect(conn) 
    require(RPostgreSQL) # Version ‘0.4.1’
    
    tryCatch({
        drv <- dbDriver("PostgreSQL")
        print("Connecting to database")
        conn <- dbConnect(drv, 
                          dbname = pg_database,
                          host = pg_hostname, 
                          port = pg_port,
                          user = pg_uid, 
                          password = pg_pwd)
        print("Connected!")
    },
    
    error = function(cond) {
        print("Unable to connect to database.")
    })
    
    # Write the connection to the environment 
    conn <<- conn
}

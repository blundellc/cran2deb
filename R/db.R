
db_start <- function() {
    drv <- dbDriver('SQLite')
    con <- dbConnect(drv, dbname=file.path(cache_root,'cran2deb.db'))
    tables <- dbListTables(con)
    if (!dbExistsTable(con,'sysreq_override')) {
        dbGetQuery(con,paste('CREATE TABLE sysreq_override ('
                  ,' debian_name TEXT NOT NULL'
                  ,',r_pattern TEXT PRIMARY KEY NOT NULL'
                  ,')'))
    }
    if (!dbExistsTable(con,'license_override')) {
        dbGetQuery(con,paste('CREATE TABLE license_override ('
                  ,' name TEXT PRIMARY KEY NOT NULL'
                  ,',accept INT NOT NULL'
                  ,')'))
    }
    if (!dbExistsTable(con,'license_hashes')) {
        dbGetQuery(con,paste('CREATE TABLE license_hashes ('
                  ,' name TEXT NOT NULL'
                  ,',sha1 TEXT PRIMARY KEY NOT NULL'
                  ,')'))
    }
    return(con)
}

db_stop <- function(con) {
    dbDisconnect(con)
}

db_quote <- function(text) {
    return(paste('"',gsub('([^][[:alnum:]*?. ()<>:/=+%-])','\\\\\\1',text),'"',sep=''))
}

db_sysreq_override <- function(sysreq_text) {
    sysreq_text <- tolower(sysreq_text)
    con <- db_start()
    results <- dbGetQuery(con,paste(
                    'SELECT DISTINCT debian_name FROM sysreq_override WHERE'
                            ,db_quote(sysreq_text),'LIKE r_pattern'))
    db_stop(con)
    if (length(results) == 0) {
        return(NA)
    }
    return(results$debian_name)
}

db_add_sysreq_override <- function(pattern,debian_name) {
    pattern <- tolower(pattern)
    debian_name <- tolower(debian_name)
    con <- db_start()
    results <- dbGetQuery(con,paste(
                     'INSERT OR REPLACE INTO sysreq_override'
                    ,'(debian_name, r_pattern) VALUES ('
                    ,' ',db_quote(debian_name)
                    ,',',db_quote(pattern)
                    ,')'))
    db_stop(con)
}

db_sysreq_overrides <- function() {
    con <- db_start()
    overrides <- dbGetQuery(con,paste('SELECT * FROM sysreq_override'))
    db_stop(con)
    return(overrides)
}


db_license_override_name <- function(name) {
    name <- tolower(name)
    con <- db_start()
    results <- dbGetQuery(con,paste(
                    'SELECT DISTINCT accept FROM license_override WHERE'
                            ,db_quote(name),'= name'))
    db_stop(con)
    if (length(results) == 0) {
        return(NA)
    }
    return(as.logical(results$accept))
}

db_add_license_override <- function(name,accept) {
    name <- tolower(name)
    message(paste('adding',name,'accept?',accept))
    if (accept != TRUE && accept != FALSE) {
        stop('accept must be TRUE or FALSE')
    }
    con <- db_start()
    results <- dbGetQuery(con,paste(
                     'INSERT OR REPLACE INTO license_override'
                    ,'(name, accept) VALUES ('
                    ,' ',db_quote(name)
                    ,',',as.integer(accept)
                    ,')'))
    db_stop(con)
}

db_license_override_hash <- function(license_sha1) {
    license_sha1 <- tolower(license_sha1)
    con <- db_start()
    results <- dbGetQuery(con,paste(
                     'SELECT DISTINCT accept FROM license_override'
                    ,'INNER JOIN license_hashes'
                    ,'ON license_hashes.name = license_override.name WHERE'
                    ,db_quote(license_sha1),'= license_hashes.sha1'))
    db_stop(con)
    if (length(results) == 0) {
        return(NA)
    }
    return(as.logical(results$accept))
}

db_license_overrides <- function() {
    con <- db_start()
    overrides <- dbGetQuery(con,paste('SELECT * FROM license_override'))
    hashes    <- dbGetQuery(con,paste('SELECT * FROM license_hashes'))
    db_stop(con)
    # TODO: change accept from 0,1 into FALSE,TRUE
    return(list(overrides=overrides,hashes=hashes))
}

db_add_license_hash <- function(name,license_sha1) {
    name <- tolower(name)
    license_sha1 <- tolower(license_sha1)
    message(paste('adding hash',license_sha1,'for',name))
    con <- db_start()
    dbGetQuery(con,paste(
         'INSERT OR REPLACE INTO license_hashes'
        ,'(name, sha1) VALUES ('
        ,' ',db_quote(name)
        ,',',db_quote(license_sha1)
        ,')'))
    db_stop(con)
}


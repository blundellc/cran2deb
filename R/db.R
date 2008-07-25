
db_start <- function() {
    drv <- dbDriver('SQLite')
    con <- dbConnect(drv, dbname=file.path(root,'data/cran2deb.db'))
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
    if (!dbExistsTable(con,'license_files')) {
        dbGetQuery(con,paste('CREATE TABLE license_files ('
                  ,' name TEXT NOT NULL'
                  ,',file_sha1 TEXT PRIMARY KEY NOT NULL'
                  ,')'))
    }
    return(con)
}

db_stop <- function(con) {
    dbDisconnect(con)
}

db_quote <- function(text) {
    return(paste('"',gsub('([^][[:alnum:]*?. ()<>:/=+-])','\\\\\\1',text),'"',sep=''))
}

db_sysreq_override <- function(sysreq_text) {
    sysreq_text <- tolower(sysreq_text)
    con <- db_start()
    results <- dbGetQuery(con,paste(
                    'SELECT debian_name FROM sysreq_override WHERE'
                            ,db_quote(sysreq_text),'GLOB r_pattern'))
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
                    'SELECT accept FROM license_override WHERE'
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

db_license_override_file <- function(file_sha1) {
    file_sha1 <- tolower(file_sha1)
    con <- db_start()
    results <- dbGetQuery(con,paste(
                     'SELECT name,accept FROM license_override'
                    ,'INNER JOIN license_files'
                    ,'ON license_files.name = license_override.name WHERE'
                    ,db_quote(file_sha1),'= license_files.file_sha1'))
    db_stop(con)
    # TODO: change accept from 0,1 into FALSE,TRUE
    # TODO: NULL -> NA
    return(results)
}

db_license_overrides <- function() {
    con <- db_start()
    overrides <- dbGetQuery(con,paste('SELECT * FROM license_override'))
    files     <- dbGetQuery(con,paste('SELECT * FROM license_files'))
    db_stop(con)
    # TODO: change accept from 0,1 into FALSE,TRUE
    return(list(overrides=overrides,files=files))
}

db_add_license_file <- function(name,file_sha1) {
    name <- tolower(name)
    file_sha1 <- tolower(file_sha1)
    message(paste('adding file',file_sha1,'for',name))
    con <- db_start()
    dbGetQuery(con,paste(
         'INSERT OR REPLACE INTO license_files'
        ,'(name, file_sha1) VALUES ('
        ,' ',db_quote(name)
        ,',',db_quote(file_sha1)
        ,')'))
    db_stop(con)
}


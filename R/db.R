
db.start <- function() {
    drv <- dbDriver('SQLite')
    con <- dbConnect(drv, dbname=file.path(root,'data/cran2deb.db'))
    tables <- dbListTables(con)
    if (!dbExistsTable(con,'sysreq_override')) {
        dbGetQuery(con,paste('CREATE TABLE sysreq_override ('
                  ,' debian_name TEXT UNIQUE NOT NULL'
                  ,',r_pattern TEXT UNIQUE NOT NULL'
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

db.stop <- function(con) {
    dbDisconnect(con)
}

db.quote <- function(text) {
    return(paste('"',gsub('([^][[:alnum:]*?. ()<>:/=-])','\\\\\\1',text),'"',sep=''))
}

db.sysreq.override <- function(sysreq_text) {
    con <- db.start()
    results <- dbGetQuery(con,paste(
                    'SELECT debian_name FROM sysreq_override WHERE'
                            ,db.quote(sysreq_text),'GLOB r_pattern'))
    db.stop(con)
    return(results$debian_name)
}

db.add.sysreq.override <- function(pattern,debian_name) {
    con <- db.start()
    results <- dbGetQuery(con,paste(
                     'INSERT OR REPLACE INTO sysreq_override'
                    ,'(debian_name, r_pattern) VALUES ('
                    ,' ',db.quote(debian_name)
                    ,',',db.quote(pattern)
                    ,')'))
    db.stop(con)
}

db.license.override.name <- function(name) {
    con <- db.start()
    results <- dbGetQuery(con,paste(
                    'SELECT accept FROM license_override WHERE'
                            ,db.quote(name),'= name'))
    db.stop(con)
    if (length(results) == 0) {
        return(FALSE)
    }
    return(as.logical(results$accept))
}

db.add.license.override <- function(name,accept) {
    message(paste('adding',name,'accept?',accept))
    if (accept != TRUE && accept != FALSE) {
        stop('accept must be TRUE or FALSE')
    }
    con <- db.start()
    results <- dbGetQuery(con,paste(
                     'INSERT OR REPLACE INTO license_override'
                    ,'(name, accept) VALUES ('
                    ,' ',db.quote(name)
                    ,',',as.integer(accept)
                    ,')'))
    db.stop(con)
}

db.license.override.file <- function(file_sha1) {
    con <- db.start()
    results <- dbGetQuery(con,paste(
                     'SELECT name,accept FROM license_override'
                    ,'INNER JOIN license_files'
                    ,'ON license_files.name = license_override.name WHERE'
                    ,db.quote(file_sha1),'= license_files.file_sha1'))
    db.stop(con)
    # TODO: change accept from 0,1 into FALSE,TRUE
    # TODO: NULL -> FALSE
    return(results)
}

db.license.overrides <- function() {
    con <- db.start()
    overrides <- dbGetQuery(con,paste('SELECT * FROM license_override'))
    files     <- dbGetQuery(con,paste('SELECT * FROM license_files'))
    db.stop(con)
    # TODO: change accept from 0,1 into FALSE,TRUE
    return(list(overrides=overrides,files=files))
}

db.add.license.file <- function(name,file_sha1) {
    message(paste('adding file',file_sha1,'for',name))
    con <- db.start()
    dbGetQuery(con,paste(
         'INSERT OR REPLACE INTO license_files'
        ,'(name, file_sha1) VALUES ('
        ,' ',db.quote(name)
        ,',',db.quote(file_sha1)
        ,')'))
    db.stop(con)
}


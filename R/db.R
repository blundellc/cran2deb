
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
                  ,' name TEXT UNIQUE NOT NULL'
                  ,',file_sha1 TEXT UNIQUE'
                  ,',accept INT NOT NULL'
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
    return(results$accept)
}

db.add.license.override.name <- function(name,accept) {
    con <- db.start()
    results <- dbGetQuery(con,paste(
                     'INSERT OR REPLACE INTO license_override'
                    ,'(name, accept) VALUES ('
                    ,' ',db.quote(name)
                    ,',',db.quote(accept)
                    ,')'))
    db.stop(con)
}

db.license.override.file <- function(file_sha1) {
    con <- db.start()
    results <- dbGetQuery(con,paste(
                    'SELECT accept FROM license_override WHERE'
                            ,db.quote(file_sha1),'= file_sha1'))
    db.stop(con)
    return(results$accept)
}

db.add.license.override.file <- function(name,file_sha1,accept) {
    con <- db.start()
    results <- dbGetQuery(con,paste(
                     'INSERT OR REPLACE INTO license_override'
                    ,'(name, file_sha1, accept) VALUES ('
                    ,' ',db.quote(name)
                    ,',',db.quote(file_sha1)
                    ,',',db.quote(accept)
                    ,')'))
    db.stop(con)
}


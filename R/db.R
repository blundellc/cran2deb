
db_start <- function() {
    drv <- dbDriver('SQLite')
    con <- dbConnect(drv, dbname=file.path(cache_root,'cran2deb.db'))
    tables <- dbListTables(con)
    if (!dbExistsTable(con,'sysreq_override')) {
        dbGetQuery(con,paste('CREATE TABLE sysreq_override ('
                  ,' depend_alias TEXT NOT NULL'
                  ,',r_pattern TEXT PRIMARY KEY NOT NULL'
                  ,')'))
    }
    if (!dbExistsTable(con,'debian_dependency')) {
        dbGetQuery(con,paste('CREATE TABLE debian_dependency ('
                  ,' id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL'
                  ,',alias TEXT NOT NULL'
                  ,',build INTEGER NOT NULL'
                  ,',debian_pkg TEXT NOT NULL'
                  ,',UNIQUE (alias,build,debian_pkg)'
                  ,')'))
    }
    if (!dbExistsTable(con,'forced_depends')) {
        dbGetQuery(con,paste('CREATE TABLE forced_depends ('
                  ,' r_name TEXT NOT NULL'
                  ,',depend_alias TEXT NOT NULL'
                  ,',PRIMARY KEY (r_name,depend_alias)'
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
    con <- db_start()
    results <- dbGetQuery(con,paste(
                    'SELECT DISTINCT depend_alias FROM sysreq_override WHERE'
                            ,db_quote(tolower(sysreq_text)),'LIKE r_pattern'))
    db_stop(con)
    if (length(results) == 0) {
        return(NA)
    }
    return(results$depend_alias)
}

db_add_sysreq_override <- function(pattern,depend_alias) {
    con <- db_start()
    results <- dbGetQuery(con,paste(
                     'INSERT OR REPLACE INTO sysreq_override'
                    ,'(depend_alias, r_pattern) VALUES ('
                    ,' ',db_quote(tolower(depend_alias))
                    ,',',db_quote(tolower(pattern))
                    ,')'))
    db_stop(con)
}

db_sysreq_overrides <- function() {
    con <- db_start()
    overrides <- dbGetQuery(con,paste('SELECT * FROM sysreq_override'))
    db_stop(con)
    return(overrides)
}

db_get_depends <- function(depend_alias,build=F) {
    con <- db_start()
    results <- dbGetQuery(con,paste(
                    'SELECT DISTINCT debian_pkg FROM debian_dependency WHERE'
                    ,db_quote(tolower(depend_alias)),'= alias'
                    ,'AND',as.integer(build),'= build'))
    db_stop(con)
    return(results$debian_pkg)
}

db_add_depends <- function(depend_alias,debian_pkg,build=F) {
    con <- db_start()
    results <- dbGetQuery(con,paste(
                     'INSERT OR REPLACE INTO debian_dependency'
                    ,'(alias, build, debian_pkg) VALUES ('
                    ,' ',db_quote(tolower(depend_alias))
                    ,',',as.integer(build)
                    ,',',db_quote(tolower(debian_pkg))
                    ,')'))
    db_stop(con)
}

db_depends <- function() {
    con <- db_start()
    depends <- dbGetQuery(con,paste('SELECT * FROM debian_dependency'))
    db_stop(con)
    return(depends)
}

db_get_forced_depends <- function(r_name) {
    con <- db_start()
    forced_depends <- dbGetQuery(con,
                paste('SELECT depend_alias FROM forced_depends WHERE'
                     ,db_quote(r_name),'= r_name'))
    db_stop(con)
    return(forced_depends$depend_alias)
}

db_add_forced_depends <- function(r_name, depend_alias) {
    if (!length(db_get_depends(depend_alias,build=F)) &&
        !length(db_get_depends(depend_alias,build=T))) {
        stop(paste('Debian dependency alias',depend_alias,'is not know,'
                  ,'yet trying to force a dependency on it?'))
    }
    con <- db_start()
    dbGetQuery(con,
            paste('INSERT OR REPLACE INTO forced_depends (r_name, depend_alias)'
                 ,'VALUES (',db_quote(r_name),',',db_quote(depend_alias),')'))
    db_stop(con)
}

db_forced_depends <- function() {
    con <- db_start()
    depends <- dbGetQuery(con,paste('SELECT * FROM forced_depends'))
    db_stop(con)
    return(depends)
}

db_license_override_name <- function(name) {
    con <- db_start()
    results <- dbGetQuery(con,paste(
                    'SELECT DISTINCT accept FROM license_override WHERE'
                            ,db_quote(tolower(name)),'= name'))
    db_stop(con)
    if (length(results) == 0) {
        return(NA)
    }
    return(as.logical(results$accept))
}

db_add_license_override <- function(name,accept) {
    message(paste('adding',name,'accept?',accept))
    if (accept != TRUE && accept != FALSE) {
        stop('accept must be TRUE or FALSE')
    }
    con <- db_start()
    results <- dbGetQuery(con,paste(
                     'INSERT OR REPLACE INTO license_override'
                    ,'(name, accept) VALUES ('
                    ,' ',db_quote(tolower(name))
                    ,',',as.integer(accept)
                    ,')'))
    db_stop(con)
}

db_license_override_hash <- function(license_sha1) {
    con <- db_start()
    results <- dbGetQuery(con,paste(
                     'SELECT DISTINCT accept FROM license_override'
                    ,'INNER JOIN license_hashes'
                    ,'ON license_hashes.name = license_override.name WHERE'
                    ,db_quote(tolower(license_sha1)),'= license_hashes.sha1'))
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
    if (is.na(db_license_override_name(name))) {
        stop(paste('license',name,'is not know, yet trying to add a hash for it?'))
    }
    message(paste('adding hash',license_sha1,'for',name))
    con <- db_start()
    dbGetQuery(con,paste(
         'INSERT OR REPLACE INTO license_hashes'
        ,'(name, sha1) VALUES ('
        ,' ',db_quote(tolower(name))
        ,',',db_quote(tolower(license_sha1))
        ,')'))
    db_stop(con)
}


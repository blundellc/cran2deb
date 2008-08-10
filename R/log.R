log_messages <- list()

log_clear <- function() {
    assign('log_messages',list(),envir=.GlobalEnv)
}

log_add <- function(text) {
    message(text)
    assign('log_messages',c(log_messages, text),envir=.GlobalEnv)
}

log_retrieve <- function() {
    return(log_messages)
}

notice <- function(...) {
    log_add(paste('N:',...))
}

warn <- function(...) {
    log_add(paste('W:',...))
}

error <- function(...) {
    log_add(paste('E:',...))
}

fail <- function(...) {
    txt <- paste('E:',...)
    log_add(txt)
    stop(txt)
}

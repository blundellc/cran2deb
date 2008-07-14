version.new <- function(rver,debian_revision=1, debian_epoch=0) {
    # generate a string representation of the Debian version of an
    # R version of a package
    pkgver = rver

    # ``Writing R extensions'' says that the version consists of at least two
    # non-negative integers, separated by . or -
    if (!length(grep('^([0-9]+[.-])+[0-9]+$',rver))) {
        stop(paste('Not a valid R package version',rver))
    }

    # Debian policy says that an upstream version should start with a digit and
    # may only contain ASCII alphanumerics and '.+-:~'
    if (!length(grep('^[0-9][A-Za-z0-9.+:~-]*$',rver))) {
        stop(paste('R package version',rver
                  ,'does not obviously translate into a valid Debian version.'))
    }

    # if rver contains a : then the Debian version must also have a colon
    if (debian_epoch == 0 && length(grep(':',pkgver)))
        debian_epoch = 1

    # if the epoch is non-zero then include it
    if (debian_epoch != 0)
        pkgver = paste(debian_epoch,':',pkgver,sep='')

    # always add the '-1' Debian release; nothing is lost and rarely will R
    # packages be Debian packages without modification.
    return(paste(pkgver,'-',debian_revision,sep=''))
}

version.epoch <- function(pkgver) {
    # return the Debian epoch of a Debian package version
    if (!length(grep(':',pkgver)))
        return(0)
    return(as.integer(sub('^([0-9]+):.*','\\1',pkgver)))
}
# version.epoch . version.new(x,y) = id
# version.epoch(version.new(x,y)) = 0

version.revision <- function(pkgver) {
    # return the Debian revision of a Debian package version
    return(as.integer(sub('.*-([0-9]+)$','\\1',pkgver)))
}
# version.revision . version.new(x) = id
# version.revision(version.new(x)) = 1

version.upstream <- function(pkgver) {
    # return the upstream version of a Debian package version
    return(sub('-[0-9]+$','',sub('^[0-9]+:','',pkgver)))
}
# version.upstream . version.new = id

version.update <- function(rver, prev_pkgver) {
    # return the next debian package version
    prev_rver <- version.upstream(prev_pkgver)
    if (prev_rver == rver) {
        # increment the Debian revision
        return(version.new(rver
                          ,debian_revision = version.revision(prev_pkgver)+1
                          ,debian_epoch    = version.epoch(prev_pkgver)
                          ))
    }
    # new release
    # TODO: implement Debian ordering over version and then autoincrement
    #       Debian epoch when upstream version does not increment.
    return(version.new(rver
                      ,debian_epoch = version.epoch(prev_pkgver)
                      ))
}

repourl_as_debian <- function(url) {
    # map the url to a repository onto its name in debian package naming
    if (length(grep('cran',url))) {
        return('cran')
    }
    if (length(grep('bioc',url))) {
        return('bioc')
    }
    stop(paste('unknown repository',url))
}

pkgname_as_debian <- function(name,repopref=NULL,version=NULL,binary=T,build=F) {
    # generate the debian package name corresponding to the R package name
    if (name %in% base_pkgs) {
        name = 'R'
    }
    if (name == 'R') {
        # R is special.
        if (binary) {
            if (build) {
                debname='r-base-dev'
            } else {
                debname='r-base-core'
            }
        } else {
            debname='R'
        }
    } else {
        # XXX: data.frame rownames are unique, so always override repopref for
        #      now.
        if (!(name %in% rownames(available))) {
            bundle <- r_bundle_of(name)
            if (is.na(bundle)) {
                stop(paste('package',name,'is not available'))
            }
            name <- bundle
        }
        debname = tolower(name)
        if (binary) {
            repopref <- tolower(repourl_as_debian(available[name,'Repository']))
            debname = paste('r',repopref,debname,sep='-')
        }
    }
    if (!is.null(version) && length(version) > 1) {
        debname = paste(debname,' (',version,')',sep='')
    }
    return(debname)
}


# sudo pbuilder --execute r -e 'rownames(installed.packages())'
# XXX: has to be a better way of doing this
base_pkgs=c('base',   'datasets','grDevices','graphics','grid', 'methods'
           ,'splines','stats',   'stats4',   'tcltk',   'tools','utils')
# found in R source directory:
# 'profile', 'datasets'

repourl.as.debian <- function(url) {
    # map the url to a repository onto its name in debian package naming
    if (length(grep('cran',url))) {
        return('cran')
    }
    if (length(grep('bioc',url))) {
        return('bioc')
    }
    stop(paste('unknown repository',url))
}

pkgname.as.debian <- function(name,repopref=NULL,version=NULL,binary=T) {
    # generate the debian package name corresponding to the R package name
    if (name %in% base_pkgs) {
        name = 'R'
    }
    if (name == 'R') {
        # R is special.
        if (binary) {
            debname='r-base-core'
        } else {
            debname='r-base-dev'
        }
    } else {
        # XXX: data.frame rownames are unique, so always override repopref for
        #      now.
        if (!(name %in% rownames(available))) {
            bundle <- r.bundle.of(name)
            if (is.na(bundle)) {
                stop(paste('package',name,'is not available'))
            }
            name <- bundle
        }
        repopref <- repourl.as.debian(available[name,'Repository'])
        debname = paste('r',tolower(repopref),tolower(name),sep='-')
    }
    if (!is.null(version) && length(version) > 1) {
        debname = paste(debname,' (',version,')',sep='')
    }
    return(debname)
}


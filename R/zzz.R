
changesfile <- function(srcname,version='*') {
    return(file.path(pbuilder_results
                    ,paste(srcname,'_',version,'_'
                          ,host.arch(),'.changes',sep='')))
}

maintainer <- 'cran2deb buildbot <cran2deb@example.org>'
root <- system.file('')
pbuilder_results <- file.path(root,'var/results')
pbuilder_config  <- file.path(root,'etc/pbuilderrc')
dput_config      <- file.path(root,'etc/dput.cf')
dinstall_config  <- file.path(root,'etc/mini-dinstall.conf')
dinstall_archive <- file.path(root,'var/archive')
r_depend_fields  <- c('Depends','Imports') # Suggests, Enhances

# we cache the list of available packages
# should be pulled in already
#load(file.path(root,'var/cache/available.cache.Rd'))


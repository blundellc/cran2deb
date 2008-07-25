.First.lib <- function(libname, pkgname) {
    global <- function(name,value) assign(name,value,envir=.GlobalEnv)
    global("changesfile", function(srcname,version='*') {
        return(file.path(pbuilder_results
                        ,paste(srcname,'_',version,'_'
                              ,host_arch(),'.changes',sep='')))
    })
    global("maintainer", 'cran2deb buildbot <cran2deb@example.org>')
    global("root", system.file(package='cran2deb'))
    global("pbuilder_results", file.path(root,'var/results'))
    global("pbuilder_config", file.path(root,'etc/pbuilderrc'))
    global("dput_config", file.path(root,'etc/dput.cf'))
    global("dinstall_config", file.path(root,'etc/mini-dinstall.conf'))
    global("dinstall_archive", file.path(root,'var/archive'))
    global("r_depend_fields", c('Depends','Imports')) # Suggests, Enhances

    cache <- file.path(root,'data/cache.rda')
    if (file.exists(cache)) {
        load(cache,envir=.GlobalEnv)
    }
}

.First.lib <- function(libname, pkgname) {
    global <- function(name,value) assign(name,value,envir=.GlobalEnv)
    global("maintainer", 'cran2deb autobuild <cran2deb@example.org>')
    global("root", system.file(package='cran2deb'))
    global("cache_root", '/var/cache/cran2deb')
    global("pbuilder_results", '/var/cache/cran2deb/results')
    global("pbuilder_config", file.path(root,'etc/pbuilderrc'))
    global("dput_config", file.path(root,'etc/dput.cf'))
    global("dinstall_config", file.path(root,'etc/mini-dinstall.conf'))
    global("dinstall_archive", file.path(root,'var/archive'))
    global("r_depend_fields", c('Depends','Imports')) # Suggests, Enhances
    # git_revision {
    global("git_revision","0a42c69323bbf485eb18fc4345253bee2efd7aff")
    # git_revision }
    global("changesfile", function(srcname,version='*') {
        return(file.path(pbuilder_results
                        ,paste(srcname,'_',version,'_'
                              ,host_arch(),'.changes',sep='')))
    })

    cache <- file.path(cache_root,'cache.rda')
    if (file.exists(cache)) {
        load(cache,envir=.GlobalEnv)
    }
}

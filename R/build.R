
build <- function(name,extra_deps) {
    dir <- setup()
    pkg <- try((function() {
        # see if it has already been built
        srcname <- pkgname_as_debian(name,binary=F)
        debname <- pkgname_as_debian(name,binary=T)
        version <- version_new(available[name,'Version'])
        if (file.exists(changesfile(srcname, version))) {
            message(paste('N: already built',srcname,'version',version))
            return(NA)
        }
        # XXX: what about building newer versions?
        if (debname %in% debian_pkgs) {
            message(paste('N:',srcname,' exists in Debian (perhaps a different version)'))
            return(NA)
        }

        rm(debname,srcname,version)

        pkg <- prepare_new_debian(prepare_pkg(dir,name),extra_deps)
        # delete the current archive (XXX: assumes mini-dinstall)
        for (subdir in c('mini-dinstall','unstable')) {
            path = file.path(dinstall_archive,subdir)
            if (file.exists(path)) {
                unlink(path,recursive=T)
            }
        }

        # delete notes of upload
        file.remove(Sys.glob(file.path(pbuilder_results,'*.upload')))

        # make mini-dinstall generate the skeleton of the archive
        ret = system(paste('umask 022;mini-dinstall --batch -c',dinstall_config))
        if (ret != 0) {
            stop('failed to create archive')
        }

        # pull in all the R dependencies
        message(paste('N: dependencies:',paste(pkg$depends$r,collapse=', ')))
        for (dep in pkg$depends$r) {
            if (pkgname_as_debian(dep) %in% debian_pkgs) {
                message(paste('N: using Debian package of',dep))
                next
            }
            # otherwise, convert to source package name
            srcdep = pkgname_as_debian(dep,binary=F)

            message(paste('N: uploading',srcdep))
            ret = system(paste('umask 022;dput','-c',shQuote(dput_config),'local'
                        ,changesfile(srcdep)))
            if (ret != 0) {
                stop('upload of dependency failed! maybe you did not build it first?')
            }
        }
        build_debian(pkg)

        # upload the package
        ret = system(paste('umask 022;dput','-c',shQuote(dput_config),'local'
                    ,changesfile(pkg$srcname,pkg$debversion)))
        if (ret != 0) {
            stop('upload failed!')
        }

        return(pkg)
    })())
    cleanup(dir)
    if (inherits(pkg,'try-error')) {
        message(paste('E: failure of',name,'means these packages will fail:'
                     ,paste(r_dependency_closure(name,forward_arcs=F),collapse=', ')))
        stop(call.=F)
    }
    return(pkg)
}

build_debian <- function(pkg) {
    wd <- getwd()
    setwd(pkg$path)
    message(paste('N: building Debian package'
                 ,pkg$debname
                 ,paste('(',pkg$debversion,')',sep='')
                 ,'...'))
    ret = system(paste('pdebuild --configfile',shQuote(pbuilder_config)))
    setwd(wd)
    if (ret != 0) {
        stop('Failed to build package.')
    }
}


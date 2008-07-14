get.dependencies <- function(pkg,extra_deps) {
    if ('SystemRequirements' %in% colnames(pkg$description)) {
        stop(paste('Unsupported SystemRequirements:',pkg$description[1,'SystemRequirements']))
    }

    # determine dependencies
    dependencies <- r.dependencies.of(description=pkg$description)
    depends <- list()
    # these are used for generating the Depends fields
    as.deb <- function(r,binary) {
        return(pkgname.as.debian(paste(dependencies[r,]$name)
                                ,version=dependencies[r,]$version
                                ,repopref=pkg$repo
                                ,binary=binary))
    }
    depends$bin <- lapply(rownames(dependencies), as.deb, binary=T)
    depends$build <- lapply(rownames(dependencies), as.deb, binary=F)
    # add the command line dependencies
    depends$bin = c(extra_deps$deb,depends$bin)
    depends$build = c(extra_deps$deb,depends$build)

    # make sure we depend upon R in some way...
    if (!length(grep('^r-base',depends$build))) {
        depends$build = c(depends$build,pkgname.as.debian('R',version='>= 2.7.0',binary=F))
        depends$bin   = c(depends$bin,  pkgname.as.debian('R',version='>= 2.7.0',binary=T))
    }
    # also include stuff to allow tcltk to build (suggested by Dirk)
    depends$build = c(depends$build,'xvfb','xauth','xfonts-base')

    # remove duplicates
    depends <- lapply(depends,unique)

    # append the Debian dependencies
    depends$build=c(depends$build,'debhelper (>> 4.1.0)','cdbs')
    if (pkg$archdep) {
        depends$bin=c(depends$bin,'${shlibs:Depends}')
    }

    # the names of dependent source packages (to find the .changes file to
    # upload via dput). these can be found recursively.
    depends$r = lapply(r.dependency.closure(dependencies)
                      ,tolower)
    # append command line dependencies
    depends$r = c(extra_deps$r, depends$r)
    return(depends)
}

generate.control <- function(pkg) {
    # construct control file
    control = data.frame()
    control[1,'Source'] = pkg$srcname
    control[1,'Section'] = 'math'
    control[1,'Priority'] = 'optional'
    control[1,'Maintainer'] = maintainer
    control[1,'Build-Depends'] = paste(pkg$depends$build,collapse=', ')
    control[1,'Standards-Version'] = '3.8.0'

    control[2,'Package'] = pkg$debname
    control[2,'Architecture'] = 'all'
    if (pkg$archdep) {
        control[2,'Architecture'] = 'any'
    }
    control[2,'Depends'] = paste(pkg$depends$bin,collapse=', ')

    # bundles provide virtual packages of their contents
    if (pkg$is_bundle) {
        control[2,'Provides'] = paste(
                    lapply(r.bundle.contains(pkg$name)
                          ,function(name) return(pkgname.as.debian(paste(name)
                                                                  ,repopref=pkg$repo
                                                                  ,binary=T)))
                          ,collapse=', ')
    }

    # generate the description
    descr = 'GNU R package "'
    if ('Title' %in% colnames(pkg$description)) {
        descr = paste(descr,pkg$description[1,'Title'],sep='')
    } else {
        descr = paste(descr,pkg$name,sep='')
    }
    if (pkg$is_bundle) {
        long_descr <- pkg$description[1,'BundleDescription']
    } else {
        long_descr <- pkg$description[1,'Description']
    }
    # using \n\n.\n\n is not very nice, but is necessary to make sure
    # the longer description does not begin on the synopsis line --- R's
    # write.dcf does not appear to have a nicer way of doing this.
    descr = paste(descr,'"\n\n', long_descr, sep='')
    if ('URL' %in% colnames(pkg$description)) {
        descr = paste(descr,'\n\nURL: ',pkg$description[1,'URL'],sep='')
    }
    control[2,'Description'] = descr

    # Debian policy says 72 char width; indent minimally
    write.dcf(control,file=pkg$debfile('control.in'),indent=1,width=72)
    write.dcf(control,indent=1,width=72)
}


generate.changelog <- function(pkg) {
    # construct a dummy changelog
    # TODO: ``Writing R extensions'' mentions that a package may also have
    # {NEWS,ChangeLog} files.
    cat(paste(paste(pkg$srcname,' (',pkg$debversion,') unstable; urgency=low',sep='')
             ,'' ,'  * Initial release.',''
             ,paste(' --',maintainer,'',format(Sys.time(),'%a, %d %b %Y %H:%M:%S %z'))
             ,'',sep='\n'),file=pkg$debfile('changelog.in'))
}

generate.rules <- function(pkg) {
    cat(paste('#!/usr/bin/make -f'
             ,paste('debRreposname :=',pkg$repo)
             ,'include /usr/share/R/debian/r-cran.mk'
             ,'',sep='\n')
       ,file=pkg$debfile('rules'))
    Sys.chmod(pkg$debfile('rules'),'0700')
}

generate.copyright <- function(pkg) {
    # generate copyright file; we trust DESCRIPTION
    writeLines(strwrap(
        paste('This Debian package of the GNU R package',pkg$name
             ,'was generated automatically using cran2deb by'
             ,paste(maintainer,'.',sep='')
             ,''
             ,'The original GNU R package is Copyright (C) '
             # TODO: copyright start date, true copyright date
             ,format(Sys.time(),'%Y')
             ,pkg$description[1,'Author']
             ,'and possibly others.'
             ,''
             ,'The original GNU R package is maintained by'
             ,pkg$description[1,'Maintainer'],'and was obtained from:'
             ,''
             ,pkg$repoURL
             ,''
             ,''
             ,'The GNU R package DESCRIPTION offers a'
             ,'Copyright licenses under the terms of the',pkg$license
             ,'license.  On a Debian GNU/Linux system, common'
             ,'licenses are included in the directory'
             ,'/usr/share/common-licenses/.'
             ,''
             ,'The DESCRIPTION file for the original GNU R package '
             ,'can be found in '
             ,file.path('/usr/lib/R/site-library'
                   ,pkg$debname
                   ,'DESCRIPTION'
                   )
             ,sep='\n'), width=72), con=pkg$debfile('copyright.in'))
}

prepare.new.debian <- function(pkg,extra_deps) {
    # generate Debian version and name
    pkg$repo = repourl.as.debian(pkg$repoURL)
    pkg$debversion = version.new(pkg$version)
    if (!length(grep('^[A-Za-z0-9][A-Za-z0-9+.-]+$',pkg$name))) {
        stop(paste('Cannot convert package name into a Debian name',pkg$name))
    }
    pkg$srcname = tolower(pkg$name)
    pkg$debname = pkgname.as.debian(pkg$name,repo=pkg$repo)

    if (!length(grep('\\.tar\\.gz',pkg$archive))) {
        stop('archive is not tarball')
    }

    # re-pack into a Debian-named archive with a Debian-named directory.
    debpath = file.path(dirname(pkg$archive)
                   ,paste(pkg$srcname,'-'
                         ,pkg$version
                         ,sep=''))
    file.rename(pkg$path, debpath)
    pkg$path = debpath
    debarchive = file.path(dirname(pkg$archive)
                          ,paste(pkg$srcname,'_'
                                ,pkg$version,'.orig.tar.gz'
                                ,sep=''))
    wd <- getwd()
    setwd(dirname(pkg$path))
    # remove them pesky +x files
    system(paste('find',shQuote(basename(pkg$path))
                ,'-type f -exec chmod -x {} \\;'))
    # tar it all back up
    system(paste('tar -czf',shQuote(debarchive),shQuote(basename(pkg$path))))
    setwd(wd)
    file.remove(pkg$archive)
    pkg$archive = debarchive

    # make the debian/ directory
    debdir <- file.path(pkg$path,'debian')
    pkg$debfile <- function(x) { file.path(debdir,x) }
    unlink(debdir,recursive=T)
    dir.create(debdir)

    # see if this is an architecture-dependent package.
    # heuristic: if /src/ exists in pkg$path, then this is an
    #            architecture-dependent package.
    # CRAN2DEB.pm is a bit fancier about this but ``Writing R extensions''
    # says: ``The sources and headers for the compiled code are in src, plus
    # optionally file Makevars or Makefile.'' It seems unlikely that
    # architecture independent code would end up here.
    if (pkg$is_bundle) {
        # if it's a bundle, check each of the packages
        pkg$archdep = F
        for (pkgname in r.bundle.contains(pkg$name)) {
            pkg$archdep = file.exists(file.path(pkg$path,pkgname,'src'))
            if (pkg$archdep) {
                break
            }
        }
    } else {
        pkg$archdep = file.exists(file.path(pkg$path,'src'))
    }
    pkg$arch <- 'all'
    if (pkg$archdep) {
        pkg$arch <- host.arch()
    }

    pkg$license <- accept.license(pkg)
    pkg$depends <- get.dependencies(pkg,extra_deps)
    generate.changelog(pkg)
    generate.rules(pkg)
    generate.copyright(pkg)
    generate.control(pkg)

    # TODO: debian/watch from pkg$repoURL

    # convert text to utf8 (who knows what the original character set is --
    # let's hope iconv DTRT).
    for (file in c('control','changelog','copyright')) {
        system(paste('iconv -o ',shQuote(pkg$debfile(file))
                    ,' -t utf8 '
                    ,shQuote(pkg$debfile(paste(file,'in',sep='.')))))
        file.remove(pkg$debfile(paste(file,'in',sep='.')))
    }
    return(pkg)
}

build.debian <- function(pkg) {
    wd <- getwd()
    setwd(pkg$path)
    message(paste('N: building Debian package'
                 ,pkg$debname
                 ,paste('(',pkg$debversion,')',sep='')
                 ,'...'))
    ret = system(paste('pdebuild --configfile',pbuilder_config))
    setwd(wd)
    if (ret != 0) {
        stop('Failed to build package.')
    }
}


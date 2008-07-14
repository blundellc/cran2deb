
setup <- function() {
    # set up the working directory
    tmp <- tempfile('cran2deb')
    dir.create(tmp)
    return (tmp)
}

cleanup <- function(dir) {
    # remove the working directory
    unlink(dir,recursive=T)
    invisible()
}

prepare.pkg <- function(dir, pkgname) {
    # download and extract an R package named pkgname
    # OR the bundle containing pkgname

    # based loosely on library/utils/R/packages2.R::install.packages
    # should do nothing Debian specific

    # first a little trick; change pkgname if pkgname is contained in a bundle
    if (!(pkgname %in% rownames(available))) {
        bundle <- r.bundle.of(pkgname)
        if (is.na(bundle)) {
            stop(paste('package',pkgname,'is unavailable'))
        }
        pkgname <- bundle
    }
    archive <- download.packages(pkgname, dir, available=available, repos='', type="source")[1,2]
    if (length(grep('\\.\\.',archive)) || normalizePath(archive) != archive) {
        stop(paste('funny looking path',archive))
    }
    wd <- getwd()
    setwd(dir)
    if (length(grep('\\.zip$',archive))) {
        cmd = paste('unzip',shQuote(archive))
    } else if (length(grep('\\.tar\\.gz$',archive))) {
        cmd = paste('tar','xzf',shQuote(archive))
    } else {
        stop(paste('Type of archive',archive,'is unknown.'))
    }
    ret = system(cmd)
    setwd(wd)
    if (ret != 0) {
        stop(paste('Extraction of archive',archive,'failed.'))
    }
    pkg <- pairlist()
    pkg$name = pkgname
    pkg$archive = archive
    pkg$path = sub("_\\.(zip|tar\\.gz)", ""
                  ,gsub(.standard_regexps()$valid_package_version, ""
                  ,archive))
    if (!file.info(pkg$path)[,'isdir']) {
        stop(paste(pkg$path,'is not a directory and should be.'))
    }
    pkg$description = read.dcf(file.path(pkg$path,'DESCRIPTION'))
    pkg$repoURL = available[pkgname,'Repository']
    pkg$version = pkg$description[1,'Version']
    pkg$is_bundle = 'Bundle' %in% names(pkg$description[1,])
    # note subtly of short circuit operators (no absorption)
    if ((!pkg$is_bundle && pkg$description[1,'Package'] != pkg$name) ||
        ( pkg$is_bundle && pkg$description[1,'Bundle'] != pkg$name)) {
        stop(paste('package name mismatch'))
    }
    return(pkg)
}



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

prepare_pkg <- function(dir, pkgname) {
    # download and extract an R package named pkgname
    # OR the bundle containing pkgname

    # based loosely on library/utils/R/packages2.R::install.packages
    # should do nothing Debian specific

    # first a little trick; change pkgname if pkgname is contained in a bundle
    if (!(pkgname %in% rownames(available))) {
        bundle <- r_bundle_of(pkgname)
        if (is.na(bundle)) {
            fail('package',pkgname,'is unavailable')
        }
        pkgname <- bundle
    }
    # use this instead of download.packages as it is more resilient to
    # dodgy network connections (hello BT 'OpenWorld', bad ISP)
    fn <- paste(pkgname, '_', available[pkgname,'Version'], '.tar.gz', sep='')
    url <- paste(available[pkgname,'Repository'], fn, sep='/')
    archive <- file.path(dir, fn)
    # don't log the output -- we don't care!
    ret <- system(paste('curl','-o',shQuote(archive),'-m 720 --retry 5',shQuote(url)))
    if (ret != 0) {
        fail('failed to download',url)
    }
    # end of download.packages replacement
#    archive <- download.packages(pkgname, dir, available=available, repos='', type="source")[1,2]
    if (length(grep('\\.\\.',archive)) || normalizePath(archive) != archive) {
        fail('funny looking path',archive)
    }
    wd <- getwd()
    setwd(dir)
    if (length(grep('\\.zip$',archive))) {
        cmd = paste('unzip',shQuote(archive))
    } else if (length(grep('\\.tar\\.gz$',archive))) {
        cmd = paste('tar','xzf',shQuote(archive))
    } else {
        fail('Type of archive',archive,'is unknown.')
    }
    ret = log_system(cmd)
    setwd(wd)
    if (ret != 0) {
        fail('Extraction of archive',archive,'failed.')
    }
    pkg <- pairlist()
    pkg$name = pkgname
    pkg$archive = archive
    pkg$path = sub("_\\.(zip|tar\\.gz)", ""
                  ,gsub(.standard_regexps()$valid_package_version, ""
                  ,archive))
    if (!file.info(pkg$path)[,'isdir']) {
        fail(pkg$path,'is not a directory and should be.')
    }
    pkg$description = read.dcf(file.path(pkg$path,'DESCRIPTION'))
    pkg$repoURL = available[pkgname,'Repository']
    pkg$version = pkg$description[1,'Version']
    pkg$is_bundle = 'Bundle' %in% names(pkg$description[1,])
    # note subtly of short circuit operators (no absorption)
    if ((!pkg$is_bundle && pkg$description[1,'Package'] != pkg$name) ||
        ( pkg$is_bundle && pkg$description[1,'Bundle'] != pkg$name)) {
        fail('package name mismatch')
    }
    return(pkg)
}


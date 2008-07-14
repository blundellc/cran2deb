debian_ok_licenses=c('GPL','LGPL','AGPL','ARTISTIC' #,'UNLIMITED'
                    ,'BSD','MIT','APACHE','X11','MPL')

is_acceptable_license <- function(license) {
    # determine if license is acceptable

    # compress spaces into a single space
    license = gsub('[[:blank:]]+',' ',license)
    # make all characters upper case
    license = toupper(license)
    # don't care about versions of licenses
    license = chomp(sub('\\( ?[<=>!]+ ?[0-9.-]+ ?\\)',''
                    ,sub('-[0-9.-]+','',license)))
    if (license %in% debian_ok_licenses) {
        return(T)
    }
    # uninteresting urls
    license = gsub('HTTP://WWW.GNU.ORG/[A-Z/._-]*','',license)
    license = gsub('HTTP://WWW.X.ORG/[A-Z/._-]*','',license)
    license = gsub('HTTP://WWW.OPENSOURCE.ORG/[A-Z/._-]*','',license)
    # remove all punctuation
    license = gsub('[[:punct:]]+','',license)
    # remove any extra space introduced
    license = chomp(gsub('[[:space:]]+',' ',license))
    # redundant
    license = gsub('THE','',license)
    license = gsub('SEE','',license)
    license = gsub('STANDARD','',license)
    license = gsub('LICEN[SC]E','',license)
    license = gsub('(GNU )?(GPL|GENERAL PUBLIC)','GPL',license)
    license = gsub('(MOZILLA )?(MPL|MOZILLA PUBLIC)','MPL',license)
    # remove any extra space introduced
    license = chomp(gsub('[[:space:]]+',' ',license))
    if (license %in% debian_ok_licenses) {
        message(paste('W: Accepted wild license as',license,'. FIX THE PACKAGE!'))
        return(T)
    }
    # remove everything that looks like a version specification
    license = gsub('(VER?SION|V)? *[0-9.-]+ *(OR *(HIGHER|LATER|NEWER|GREATER|ABOVE))?',''
                   ,license)
    # remove any extra space introduced
    license = chomp(gsub('[[:space:]]+',' ',license))
    if (license %in% debian_ok_licenses) {
        message(paste('W: Accepted wild license as',license,'. FIX THE PACKAGE!'))
        return(T)
    }
    # TODO: put debian_ok_licenses in DB
    # TODO: file {LICENSE,LICENCE} (+ maybe COPYING?)
    message(paste('E: Wild license',license,'did not match'))
    return(F)
}

accept.license <- function(pkg) {
    # check the license
    if (!('License' %in% names(pkg$description[1,]))) {
        stop('package has no License: field in description!')
    }
    accept=NULL
    for (license in strsplit(chomp(pkg$description[1,'License'])
                            ,'[[:space:]]*\\|[[:space:]]*')[[1]]) {
        if (is_acceptable_license(license)) {
            accept=license
            break
        }
    }
    if (is.null(accept)) {
        stop(paste('No acceptable license:',pkg$description[1,'License']))
    } else {
        message(paste('N: Auto-accepted license',accept))
    }
    if (accept == 'Unlimited') {
        # definition of Unlimited from ``Writing R extensions''
        accept=paste('Unlimited (no restrictions on distribution or'
                    ,'use other than those imposed by relevant laws)')
    }
    return(accept)
}

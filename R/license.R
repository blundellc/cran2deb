is_acceptable_license <- function(license) {
    # determine if license is acceptable

    # compress spaces into a single space
    license = gsub('[[:blank:]]+',' ',license)
    # make all characters lower case
    license = tolower(license)
    # don't care about versions of licenses
    license = chomp(sub('\\( ?[<=>!]+ ?[0-9.-]+ ?\\)',''
                    ,sub('-[0-9.-]+','',license)))
    action = db.license.override.name(license)
    if (!is.na(action)) {
        return(action)
    }
    # uninteresting urls
    license = gsub('http://www.gnu.org/[[:alnum:]/._-]*','',license)
    license = gsub('http://www.x.org/[[:alnum:]/._-]*','',license)
    license = gsub('http://www.opensource.org/[[:alnum]/._-]*','',license)
    # remove all punctuation
    license = gsub('[[:punct:]]+','',license)
    # remove any extra space introduced
    license = chomp(gsub('[[:space:]]+',' ',license))
    # redundant
    license = gsub('the','',license)
    license = gsub('see','',license)
    license = gsub('standard','',license)
    license = gsub('licen[sc]e','',license)
    license = gsub('(gnu )?(gpl|general public)','gpl',license)
    license = gsub('(mozilla )?(mpl|mozilla public)','mpl',license)
    # remove any extra space introduced
    license = chomp(gsub('[[:space:]]+',' ',license))
    action = db.license.override.name(license)
    if (!is.na(action)) {
        message(paste('W: Accepting/rejecting wild license as',license,'. FIX THE PACKAGE!'))
        return(action)
    }
    # remove everything that looks like a version specification
    license = gsub('(ver?sion|v)? *[0-9.-]+ *(or *(higher|later|newer|greater|above))?',''
                   ,license)
    # remove any extra space introduced
    license = chomp(gsub('[[:space:]]+',' ',license))
    action = db.license.override.name(license)
    if (!is.na(action)) {
        message(paste('W: Accepting/rejecting wild license as',license,'. FIX THE PACKAGE!'))
        return(action)
    }
    # TODO: file {LICENSE,LICENCE} (+ maybe COPYING?)
    message(paste('E: Wild license',license,'did not match; rejecting'))
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

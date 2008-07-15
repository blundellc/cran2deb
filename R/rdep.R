
r.bundle.of <- function(pkgname) {
    # returns the bundle containing pkgname or NA
    bundles <- names(available[!is.na(available[, 'Bundle']), 'Contains'])
    # use the first bundle
    for (bundle in bundles) {
        if (pkgname %in% r.bundle.contains(bundle)) {
            return(bundle)
        }
    }
    return(NA)
}

r.bundle.contains <- function(bundlename) {
    return(strsplit(available[bundlename,'Contains'],'[[:space:]]+')[[1]])
}

r.requiring <- function(names) {
    for (name in names) {
        if (!(name %in% base_pkgs) && !(name %in% rownames(available))) {
            bundle <- r.bundle.of(name)
            if (is.na(bundle)) {
                stop(paste('package',name,'is not available'))
            }
            names <- c(names,r.bundle.contains(bundle))
        }
    }
    # approximately prune first into a smaller availability
    candidates <- available[sapply(rownames(available)
                                  ,function(name)
                                      length(grep(paste(names,sep='|')
                                                 ,available[name,r_depend_fields])) > 0)
                           ,r_depend_fields
                           ,drop=F]
    if (length(candidates) == 0) {
        return(c())
    }
    # find a logical index into available of every package/bundle
    # whose dependency field contains at least one element of names.
    # (this is not particularly easy to read---sorry---but is much faster than
    # the alternatives i could think of)
    prereq=c()
    dep_matches <- function(dep) chomp(gsub('\\([^\\)]+\\)','',dep)) %in% names
    any_dep_matches <- function(name,field=NA)
                any(sapply(strsplit(chomp(candidates[name,field])
                                   ,'[[:space:]]*,[[:space:]]*')
                          ,dep_matches))

    for (field in r_depend_fields) {
        matches = sapply(rownames(candidates), any_dep_matches, field=field)
        if (length(matches) > 0) {
            prereq = c(prereq,rownames(candidates[matches,]))
        }
    }
    return(unique(prereq))
}

r.dependencies.of <- function(name=NULL,description=NULL) {
    # find the immediate dependencies (children in the dependency graph) of an
    # R package
    if (!is.null(name) && (name == 'R' || name %in% base_pkgs)) {
        return(data.frame())
    }
    if (is.null(description) && is.null(name)) {
        stop('must specify either a description or a name.')
    }
    if (is.null(description)) {
        if (!(name %in% rownames(available))) {
            bundle <- r.bundle.of(name)
            if (is.na(bundle)) {
                stop(paste('package',name,'is not available'))
            }
            name <- bundle
        }
        description <- data.frame()
        # keep only the interesting fields
        for (field in r_depend_fields) {
            if (!(field %in% names(available[name,]))) {
                next
            }
            description[1,field] = available[name,field]
        }
    }
    # extract the dependencies from the description
    deps <- data.frame()
    for (field in r_depend_fields) {
        if (!(field %in% names(description[1,]))) {
            next
        }
        new_deps <- lapply(strsplit(chomp(description[1,field])
                                   ,'[[:space:]]*,[[:space:]]*')[[1]]
                          ,r.parse.dep.field)
        deps <- iterate(lapply(new_deps[!is.na(new_deps)],rbind),deps,rbind)
    }
    return (deps)
}

r.parse.dep.field <- function(dep) {
    if (is.na(dep)) {
        return(NA)
    }
    # remove other comments
    dep = gsub('(\\(\\)|\\([[:space:]]*[^<=>!].*\\))','',dep)
    # squish spaces
    dep = chomp(gsub('[[:space:]]+',' ',dep))
    # parse version
    pat = '^([^ ()]+) ?(\\( ?([<=>!]+ ?[0-9.-]+) ?\\))?$'
    if (!length(grep(pat,dep))) {
        stop(paste('R dependency',dep,'does not appear to be well-formed'))
    }
    version = sub(pat,'\\3',dep)
    dep = sub(pat,'\\1',dep)
    if (!(dep %in% rownames(available))) {
        depb <- r.bundle.of(dep)
        if (!is.na(depb)) {
            dep <- depb
        }
    }
    return(list(name=dep,version=version))
}

r.dependency.closure <- function(fringe, forward_arcs=T) {
    # find the transitive closure of the dependencies/prerequisites of some R
    # packages
    closure <- list()
    if (is.data.frame(fringe)) {
        fringe <- as.list(fringe$name)
    }
    fun = function(x) levels(r.dependencies.of(name=x)$name)
    if (!forward_arcs) {
        fun = r.requiring
    }
    while(length(fringe) > 0) {
        # pop off the top
        top <- fringe[[1]]
        if (length(fringe) > 1) {
            fringe <- fringe[2:length(fringe)]
        } else {
            fringe <- list()
        }
        src <- pkgname.as.debian(top,binary=F)
        if (!length(grep('^r-',src)) || length(grep('^r-base',src))) {
            next
        }
        newdeps <- fun(top)
        closure=c(closure,top)
        fringe=c(fringe,newdeps)
    }
    # build order
    return(rev(unique(closure,fromLast=T)))
}

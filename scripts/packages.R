default_user_lib <- (file.path("~", "R", paste(R.version$platform, "library", sep='-'), paste(R.version$major, strsplit(R.version$minor, '\\.')[[1]][1], sep='.')))

options(repos=c(CRAN="https://cran.rstudio.com"))
bootstrap_dir <- tempdir()
.libPaths(bootstrap_dir)
.libPaths(default_user_lib)
if (!require(optparse, quietly=TRUE)) {
  print(.libPaths())
  install.packages('optparse', lib = bootstrap_dir)
}

args = optparse::OptionParser()
args = optparse::add_option(args, c("-v", "--verbose"), action="store_true",
  default=TRUE, help="Print extra output [default]") 
args = optparse::add_option(args, c("-q", "--quietly"), action="store_false",
  dest="verbose", help="Print little output")  
args = optparse::add_option(args, c("-d", "--directory"), 
  default="R/library", action="store",
  dest="directory", help="directory to use for library") 
args = optparse::add_option(args, c("-l", "--libraries"), 
  default="R-libraries.txt", action="store",
  dest="libraries", help="text file listing one library name on each line.") 
args = optparse::add_option(args, c("-r", "--repositories"), 
  default="https://cran.rstudio.com",
  action="store", dest="repositories", help="repo to install from.") 
args = optparse::add_option(args, c("-u", "--update"), action="store_true",
    default=FALSE, help="Update current packages after install?") 
args = optparse::parse_args(args)

pkgs <- readLines(con=file.path(args[['libraries']]))
pkgs <- pkgs[pkgs != '']

.libPaths(args[['directory']])

for (pkg in pkgs) {
  if (!require(basename(pkg), character.only=TRUE, quietly=TRUE)) {
    o <- try(install.packages(pkgs = basename(pkg), 
      lib = bootstrap_dir, repos = args[['repositories']],
      type='source'))
    if (class(o) == 'try-error')
      if (!require(devtools, quietly=TRUE)) {
        install.packages('devtools', lib = bootstrap_dir)
      }
      try(devtools::install_github(pkg))
  }
  if (!require(basename(pkg), character.only=TRUE, quietly=TRUE))
    warning(paste0("Package ", pkg, " failed to load.")) 
}

if (!dir.exists(args[['directory']])) {
  dir.create(args[['directory']], recursive=TRUE, mode="0775")
}

package_dirs <- list.dirs(bootstrap_dir) %>% normalizePath
for(package in package_dirs) {
  file.copy(package, args[['directory']], overwrite=TRUE, recursive=TRUE)
}
if (args[['update']])
  update.packages(ask=FALSE, checkBuilt=FALSE)




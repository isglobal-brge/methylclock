.onLoad <- function(libname, pkgname) {
  assign("reference.globals", new.env(), envir = parent.env(environment()))
  load.globals()
}

load.globals <- function() {
  load.env <- function(filename, env) {
    if (file.exists(filename)) {
      load(filename, env)
    }
  }

  load.env(system.file("references.rda", package = "methylclock"), reference.globals)
}

# called by ../data-raw/globals.r to save generated global variables to Rdata files
# for loading whenever the package is loaded.
#
# Details : The original meffil.list.cell.type.references and get.cell.type.reference function from meffil v1.0.0
# at githug : https://github.com/perishky/meffil
#
save.globals <- function(dir) {
  require(devtools)
  save.env <- function(filename, env) {
    save(
      list = ls(env),
      file = filename,
      envir = env
    )
    file.copy(filename, inst("methylclock"), overwrite = TRUE)
  }

  save.env(file.path(dir, "references.rda"), reference.globals)
}

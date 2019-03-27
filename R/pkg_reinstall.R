# to reinstall EVERYTHING
pkg_reinstall = function(lib = .libPaths()[1], pkgs = as.data.frame(installed.packages(lib), stringsAsFactors=FALSE)$Package) {
  install.packages(
    lib  = lib,
    pkgs = pkgs,
    type = 'source',
    dependencies=TRUE
  )
}

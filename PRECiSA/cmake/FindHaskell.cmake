include(FindPackageHandleStandardArgs)

find_program(CABAL NAMES cabal $ENV{HOME}/.cabal/bin $ENV{CABAL_HOME}/bin)
find_package_handle_standard_args(Haskell DEFAULT_MSG CABAL)



# This file parses the cabal.project file and builds an attrset with the
# following shape:
#
# {
#   package1 = /path_to/package1;
#   package2 = /path_to/package2;
# }
#
# that is appropriate to be passed to packageSourceOverrides. See the
# default.nix file in the root folder for its usage.

with builtins;

let
  contents = readFile ../cabal.project;

  trimmed = replaceStrings [ "packages:" " " ] [ "" "" ] contents;

  packages = filter (x: isString x && x != "") (split "\n" trimmed);

  makeOverride = name: { "${name}" = ../. + "/${name}"; };

  overrides = foldl' (os: name: os // makeOverride name) {} packages;
in
  overrides

# Package
version       = "1.0.0"
author        = "Joel Lienhard"
description   = "A mobile game"
license       = "License"
srcDir        = "src"
binDir        = "desktop"
namedBin      = {"main": "game"}.toTable

# Dependencies
requires "nim 2.2.2"
requires "fusion"
requires "naylib"
requires "vmath"
requires "arrayutils"

import std/distros
if detectOs(Windows):
 foreignDep "openjdk"
 foreignDep "wget"
elif detectOs(Ubuntu):
 foreignDep "default-jdk"

# Tasks
include "build_android.nims"
include "convert_svgs.nims"
include "export_icon.nims"

task buildFirstBoard, "prebuild the board that will load on first open":
  cd "src"
  exec "nim c -r -d:buildFirstBoard -o:firstboardgen game.nim"
  rmFile "firstboardgen"
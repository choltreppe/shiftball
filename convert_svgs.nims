import std/[strformat, parseutils, os]

proc convertSvgs(relPath = "") =
  for (kind, path) in walkDir("inkscape_svgs"/relPath):
    let relPath = relPath/lastPathPart(path)
    let outPath = "src/resources"/relPath
    case kind
    of pcDir:
      exec &"mkdir -p {outPath}"
      convertSvgs(relPath)
    
    of pcFile:
      exec "inkscape" &
        " inkscape_svgs"/relPath &
        " --export-plain-svg" &
        " --export-type=svg" &
        &" --export-filename={outPath}"

      # delete first 2 lines because they fuck up nanosvg (whyever)
      var file = readFile(outPath)
      var i = 0
      var trash: string
      i += file.parseUntil(trash, "<svg", i)  # there is no skipUntil for string
      writeFile(outPath, file[i..^1])

    else: discard

task convertSvgs, "convert inkscape svgs to plain svgs":
  convertSvgs()
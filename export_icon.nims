import std/[strformat, os]

task exportIcon, "export icon.svg to different size .png":
  for size in [36, 48, 72, 96]:
    exec &"""inkscape --export-type="png" --export-filename="icon/{size}x{size}.png" --export-width={size} icon.svg"""
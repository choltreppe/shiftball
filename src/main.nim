import std/[sequtils]
import ./globals, ./game


when not defined(android):
  import std/os
  setCurrentDir getAppDir()


var startScreen: tuple[
  explanation, startButton: PositionedElement
]

proc loadStartScreen =
  let margin = windowSize.x div 28
  let width = windowSize.x - 2*margin

  startScreen.explanation.texture = loadTextureSvg("resources/explanation.svg", width, 0)
  startScreen.explanation.position = ivec2(margin)

  let h = startScreen.explanation.texture.height + margin
  startScreen.startButton = newButton(
    "START",
    ivec2(
      margin + width,
      h + (windowSize.y-h-buttonHeight) div 2
    ),
    align=right
  )

proc drawStartScreen =
  drawing:
    clearBackground(backgroundColor)
    for elem in startScreen.fields:
      draw elem


proc main =

  const windowTitle = "Shift Ball"
  when defined(android):
    initWindow(0, 0, windowTitle)
    windowSize = ivec2(getScreenWidth(), getScreenHeight())
  else:
    initWindow(windowSize.x, windowSize.y, windowTitle)

  block loadingScreen:
    let logo = loadTextureSvg("resources/chol_logo.svg", windowSize.x div 3, 0)
    drawing:
      clearBackground(Black)
      drawTexture(logo,
        windowSize.x div 3,
        (windowSize.y div 2) - logo.height,
        White)

  try:
    globals.loadAssets()
    game.loadAssets()
    setTargetFPS(60)
    initGame()
    var showStartScreen = level == 1
    if showStartScreen:
      loadStartScreen()
    else:
      waitTime(1)

    while not windowShouldClose():
      if showStartScreen: drawStartScreen()
      else: updateGame(getFrameTime())

      if isGestureDetected(Tap):
        let pos = getMousePosition().toIVec
        if not showStartScreen:
          onClickGame(pos)
        elif pos in startScreen.startButton:
          showStartScreen = false

  finally:
    closeWindow()

main()
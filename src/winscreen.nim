import std/[strutils, strformat]
import arrayutils
import ./globals


type
  Rating = range[0..6]

  BoardStats* = object
    minShots*, minDistance*, minDistanceWithMinShots*: int32

const ratingBarPositions = [float32 0, 0.139, 0.3, 0.5, 0.7, 0.861, 1]

var
  holeTexture: Texture2D
  holeTargetY: float32
  starsMask: PositionedElement
  statFont: Font
  statInfoHeight: int32
  statInfo: RenderTexture2D
  restartButton*, nextButton*: PositionedElement
  
  rating: Rating

proc loadAssets* =
  let width = windowSize.x - 2*margin
  holeTexture = loadTextureSvg("resources/hole.svg", width, 0)

  starsMask.texture = loadTextureSvg("resources/stars.svg", width * 8 div 10, 0)
  starsMask.position = ivec2(
    (windowSize.x - starsMask.texture.width) div 2,
    marginTop
  )

  holeTargetY = float32 starsMask.position.y + starsMask.texture.height + margin

  statFont = newFontSize(buttonFont.baseSize * 3 div 2)
  statInfoHeight = statFont.baseSize + (buttonFont.baseSize * 3 div 2)
  statInfo = loadRenderTexture(windowSize.x - 2*margin, 2*statInfoHeight + margin)

  let buttonYPos = block:
    let top = holeTargetY.int32 + holeTexture.height + 2*statInfoHeight + buttonFont.baseSize + 2*margin
    top + (windowSize.y - top - buttonHeight) div 2
  restartButton = newButton("TRY AGAIN", ivec2(margin, buttonYPos), align=left)
  nextButton = newButton("NEXT BOARD", ivec2(windowSize.x - margin, buttonYPos), align=right)


proc initWinScreen*(
  stats: BoardStats,
  shotCount, totalDistance: int32
): tuple[xp: int32] =

  let shotsDiff = shotCount - stats.minShots
  rating =
    if shotsDiff == 0:
      if totalDistance == stats.minDistanceWithMinShots:
        6
      else: 5
    elif shotCount <= boardAnalyzeDepth and totalDistance <= stats.minDistance:
      5
    else:
      case shotsDiff
      of 1: 4
      of 2: 3
      of 3, 4: 2
      of 5, 6: 1
      else: 0

  result.xp = [int32 0, 1, 2, 3, 5, 8, 10][rating]

  #var pos = ivec2(margin, holeTargetY.int32 + holeTexture.height + margin)
  textureMode(statInfo):
    clearBackground(Color())

    proc draw(
      name: string,
      value, possible: int32,
      yPos: int32,
    ) =
      let mainText = $value & " " & name
      let subText = $possible & " POSSIBLE"
      drawTextWithShadow(statFont, mainText, ivec2(0, yPos), White)
      drawText(buttonFont, subText,
        ivec2(0, yPos + statInfoHeight - buttonFont.baseSize),
        Color(r:147, g:176, b:197, a:255))

    draw("SHOTS", shotCount, stats.minShots, 0)
    if shotCount <= boardAnalyzeDepth:
      draw("BLOCKS TRAVELED", totalDistance, stats.minDistance, statInfoHeight+margin)


proc drawWinScreen*(trans: float32, nextBoardReady: bool) =
  var yPos = mix(-holeTexture.height.float32, holeTargetY, clamp(trans-0.5, 0, 1)).int32
  drawTexture(holeTexture, margin, yPos, White)

  #stars
  if trans > 3:
    drawRectangle(
      posX = starsMask.position.x, posY = starsMask.position.y,
      height = starsMask.texture.height,
      width = int32(starsMask.texture.width.float32 * min((trans-3)/3, ratingBarPositions[rating])),
      color = ballColor
    )
    draw starsMask

  # stats
  if trans > 4:
    drawTextureVFlipped(statInfo.texture,
      vec2(
        mix(windowSize.x.float32, margin.float32, min(1, trans-4)),
        holeTargetY + float32(holeTexture.height + margin * 3 div 2))
    )

  # buttons
  if trans > 7:
    draw restartButton

    if nextBoardReady:
      draw nextButton
    else:
      let pos = nextButton.position
      let size = nextButton.texture.size
      drawRoundedRect(pos, size)
      drawLoading(vec2(pos + size div 2))
  
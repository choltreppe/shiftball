import std/[sugar, options, sequtils, locks]
import fusion/matching
import vmath, raylib
export vmath, raylib


var windowSize* = ivec2(480, 800)

converter toVec*(vector: Vector2): Vec2 =
  cast[Vec2](vector)

converter toVector*(vec: Vec2): Vector2 =
  cast[Vector2](vec)

converter toIVec*(vector: Vector2): IVec2 =
  ivec2(int32 vector.x, int32 vector.y)

converter toVector*(vec: IVec2): Vector2 =
  Vector2(x: float32 vec.x, y: float32 vec.y)

func size*(texture: Texture2D): IVec2 =
  ivec2(texture.width, texture.height)

proc drawTextureVFlipped*(texture: Texture2D, pos: Vec2) =
  drawTexture(texture, Rectangle(width: texture.width.float32, height: -texture.height.float32), pos, White)

proc loadImageSvgImpl(fileName: cstring, width, height: cint): Image
  {.cdecl, importc: "LoadImageSVG", header: "svg_loading/svg_loading.h".}
proc loadImageSvg*(fileName: string, width, height: int32): Image =
  loadImageSvgImpl(fileName.cstring, width, height)

proc loadTextureSvg*(fileName: string, width, height: int32): Texture =
  loadTextureFromImage(loadImageSvg(fileName, width, height))

proc newFontSize*(size: int32): Font =
  #[const codePoints = collect:
    for c in {'A'..'Z', '0'..'9'}: int32(c)]#
  loadFont("resources/font.ttf", size, [])


const
  boardAnalyzeDepth* = 3
  ballColor* = Color(r:255, g:223, b:123, a:255)
  backgroundColor* = Color(r:93, g:119, b:137, a:255)
  shadowColor* = Color(r:49, g:70, b:85, a:255)
  loadingAnimSpeed = 400

var
  margin*: int32
  marginTop*: int32
  padding*: int32
  roundCornerTexture: Texture2D
  buttonFont*: Font
  buttonHeight*: int32
  loadingTexture: Texture2D
  loadingAngle: float32

proc loadAssets* =
  padding = windowSize.x div 32
  roundCornerTexture = loadTextureSvg("resources/round_corner.svg", padding, padding)
  buttonFont = newFontSize(int32 windowSize.x.float * 0.044)
  buttonHeight = buttonFont.baseSize div 3 * 8
  loadingTexture = loadTextureSvg("resources/loading.svg", 0, buttonFont.baseSize)


proc textWidth*(font: Font, text: string): float32 =
  measureText(font, text, float32 font.baseSize, 0).x

proc drawText*(font: Font, text: string, pos: IVec2, color: Color) =
  drawText(font, text, pos, font.baseSize.float32, 0, color)
proc drawText*(font: Font, text: string, pos: Vec2, color: Color, size: float32) =
  drawText(font, text, pos, size, 0, color)

proc renderTextTexture*(font: Font, text: string, color: Color): Texture =
  var renderTexture = loadRenderTexture(int32 textWidth(font, text), font.baseSize)
  textureMode(renderTexture):
    clearBackground(Color())
    drawText(font, text, ivec2(0), color)
  renderTexture.texture

func textShadowOffset(font: Font): IVec2 =
  ivec2(0, font.baseSize div 8)

proc drawTextWithShadow*(font: Font, text: string, pos: IVec2, color: Color) =
  drawText(font, text, pos + textShadowOffset(font), shadowColor)
  drawText(font, text, pos, color)

proc drawTextWithShadow*(font: Font, text: string, pos: Vec2, color: Color, size: float32) =
  drawText(font, text, pos + vec2(0, size/8), shadowColor, size)
  drawText(font, text, pos, color, size)

proc renderTextWithShadowTexture*(font: Font, text: string, color: Color): Texture =
  let size = ivec2(int32 textWidth(font, text), font.baseSize) + textShadowOffset(font)
  var renderTexture = loadRenderTexture(size.x, size.y)
  textureMode(renderTexture):
    clearBackground(Color())
    drawTextWithShadow(font, text, ivec2(0), color)
  renderTexture.texture


proc raylibFileExists*(fileName: cstring): bool
  {.cdecl, importc: "FileExists", header: "raylib.h".}

proc loadFileDataImpl(fileName: cstring, size: ptr cint): pointer
  {.cdecl, importc: "LoadFileData", header: "raylib.h".}
proc loadFileData*[T](fileName: string): T =
  let size = cint sizeof(T)
  cast[ptr T](loadFileDataImpl(fileName.cstring, addr size))[]

proc saveFileDataImpl(fileName: cstring, data: pointer, size: cint)
  {.cdecl, importc: "SaveFileData", header: "raylib.h".}
proc saveFileData*(fileName: string, data: auto) =
  saveFileDataImpl(fileName.cstring, addr data, cint sizeof(data))

proc raylibFileExists*(fileName: string): bool =
  let size = cint 1
  loadFileDataImpl(fileName, addr size) != nil


type PermaVar*[fileName: static string, T] = object
  val: T

proc load*[fileName, T](v: var PermaVar[fileName, T]): bool =
  if raylibFileExists(fileName):
    v.val = loadFileData[T](fileName)
    true
  else: false

proc `<-`*[fileName, T](v: var PermaVar[fileName, T], val: T) =
  v.val = val
  saveFileData(fileName, val)

converter val*[fileName, T](v: PermaVar[fileName, T]): T = v.val

template `.`*(v: PermaVar, field): auto = v.val.field


type Align* = enum left, right

type PositionedElement* = object
  texture*: Texture2D
  position*: IVec2
  isFlipped* = false

proc draw*(element: PositionedElement) =
  if element.isFlipped:
    drawTextureVFlipped(element.texture, vec2(element.position))
  else:
    drawTexture(element.texture, element.position.x, element.position.y, White)

func contains*(element: PositionedElement, pos: IVec2): bool =
  let ePos = element.position
  let eSize = element.texture.size
  (0..1).allIt(pos[it] in ePos[it] .. ePos[it] + eSize[it])


type RollCounter* = object
  fileName: string
  val: uint8
  font: Font
  color: Color
  align: Align
  renderTexture: RenderTexture2D
  prevVal: uint8
  trans: float32

proc newRollCounter*(
  fileName: string,
  fontSize: int32,
  color: Color,
  align: Align,
  startVal: uint8,
  maxVal: string,
): RollCounter =

  if not raylibFileExists(fileName):
    saveFileData(fileName, startVal)
  
  result = RollCounter(
    fileName: fileName,
    val: loadFileData[uint8](fileName),
    font: newFontSize(fontSize),
    color: color,
    align: align,
    trans: 1,
  )
  result.renderTexture = loadRenderTexture(
    textWidth(result.font, maxVal).int32,
    fontSize
  )

func size*(counter: RollCounter): IVec2 =
  counter.renderTexture.texture.size

proc draw*(counter: var RollCounter, pos: IVec2, delta: float32) =

  proc drawNumber(counter: var RollCounter, val: uint8, yPos: int32 = 0) =
    var pos = ivec2(0, yPos)
    let txt = $val
    if counter.align == right:
      pos.x += counter.renderTexture.texture.width - textWidth(counter.font, txt).int32
    drawText(counter.font, txt, pos, counter.color)

  textureMode(counter.renderTexture):
    clearBackground(Color())
    if counter.trans < 1:
      for i, val in [counter.prevVal, counter.val]:
        counter.drawNumber(val, int32(counter.font.baseSize.float32 * (counter.trans - i.float32)))
      counter.trans += delta * 6
    else:
      counter.drawNumber(counter.val)

  var pos = pos
  if counter.align == right:
    pos.x -= counter.renderTexture.texture.width
  drawTextureVFlipped(counter.renderTexture.texture, vec2(pos))

converter val*(counter: RollCounter): uint8 = counter.val

proc `+=`*[T: SomeInteger](counter: var RollCounter, d: T) =
  assert d != 0
  counter.prevVal = counter.val
  counter.val = uint8(counter.val.T + d)
  counter.trans = 0
  saveFileData(counter.fileName, counter.val)

proc `-=`*(counter: var RollCounter, d: int32) =
  counter += -d


proc drawRoundedRect*(pos, size: IVec2, color = Black) =
  drawRectangle(pos.x, pos.y, size.x, size.y, color)
  for i, corner in [ivec2(0, 0), ivec2(1, 0), ivec2(1, 1), ivec2(0, 1)]:
    drawTexture(
      texture = roundCornerTexture,
      position = pos + size*corner,
      rotation = 90.0*i.float32,
      scale = 1.0,
      tint = backgroundColor
    )

proc newButton*(text: string, position: IVec2, align: Align): PositionedElement =
  let padding = ivec2(buttonFont.baseSize, (buttonHeight - buttonFont.baseSize) div 2)
  let size = ivec2(textWidth(buttonFont, text).int32 + 2*padding.x, buttonHeight)
  var renderTexture = loadRenderTexture(size.x, size.y)
  textureMode(renderTexture):
    clearBackground(Color())
    drawRoundedRect(ivec2(0), size)
    drawText(buttonFont, text, padding, White)
  PositionedElement(
    texture: renderTexture.texture,
    isFlipped: true,
    position:
      case align
      of left: position
      of right: position - ivec2(size.x, 0)
  )

proc updateLoadingAnim*(delta: float32) {.inline.} =
  loadingAngle = (loadingAngle + delta * loadingAnimSpeed) mod 360

proc drawLoading*(center: Vec2) =
  let offset = vec2(loadingTexture.size) / 2
  drawTexture(
    texture = loadingTexture,
    position = center - rotate(-loadingAngle.toRadians)*offset,
    rotation = loadingAngle,
    scale = 1,
    tint = White
  )
import std/[options, math, strformat, strutils, parseutils, sequtils, random, times, threadpool, locks, sugar]
import fusion/matching

import ./globals, ./winscreen

const
  animationSpeed = 6
  boardSize = ivec2(5, 8)
  startBallCount = 12u8
  boardGenThreads = 4
  wonTransMax = 8

type
  Direction = enum north="n", east="e", south="s", west="w"

  Slot = object
    side: Direction
    index: int32

  HandSide = enum left="l", right="r"

  TileKind = enum empty, branch, curve, straight, goal
  Tile = object
    case kind: TileKind
    of branch:
      curveDirection: HandSide
    else: discard
    orientation: Direction

  Board = array[boardSize.y, array[boardSize.x, Tile]]

  MotionKind = enum straight, curve, goal
  Motion = object
    entry: Direction
    case kind: MotionKind
    of curve:
      curveExit: Direction
      tileIsMoving: bool
    else: discard

  Ball = tuple
    position: IVec2
    motion: Motion

  Shot = object
    startSlot: Slot
    ball: Ball
    moveTiles: tuple[positions: seq[IVec2], direction: Direction]
    pushBack: bool  # push back tile&ball on half way treveled
    path: seq[seq[IVec2]]  # for detecting infinite loops (devided in segments between bounces) (could become a ring buffer(5))
    distance: int32

  GameState = enum running, trapped, won

  StartBoard = object
    board: Board
    stats: BoardStats

var
  tileSize: int32
  boardOffset: Vec2
  boardInside: RenderTexture2D
  textures: tuple[
    tiles: array[TileKind.curve..TileKind.goal, array[Direction, Texture2D]],
    branchTile: array[HandSide, array[Direction, Texture2D]],
    slotMark: array[Direction, Texture2D],
    ball: Texture2D
  ]
  trappedTexture: Texture2D
  xpBar: tuple[
    texture: Texture2D,
    center: IVec2,
    radius: float32,
  ]
  
  levelFont: Font
  shotCount, totalDistance: int32
  prevScreen: RenderTexture2D
  screenTrans: Option[float32]

  level*: PermaVar["level", int]
  shouldLevelUp, isLevelingUp = false
  xp: PermaVar["xp", int32]
  prevXp: PermaVar["prevxp", int32]
  xpProgress: Slice[float32]
  startBoard: PermaVar["board", StartBoard]  # for restarting the board
  nextStartBoard: StartBoard   # for preloading in background
  nextStartBoardLock: Lock
  nextStartBoardIsReady = false
  board: Board
  shot: Option[Shot]
  state: GameState
  trans: float32   # transition value for current animation

initLock nextStartBoardLock
randomize(times.getTime().toUnix)

proc loadAssets* =

  tileSize = windowSize.x div (boardSize.x + 2)
  let boardScreenSize = tileSize * boardSize
  boardInside = loadRenderTexture(boardScreenSize.x, boardScreenSize.y)
  boardOffset = block:
    let d = vec2(windowSize - boardScreenSize)
    vec2(d.x / 2, (d.y + tileSize.float32 * 1.5) / 2)
  marginTop = boardOffset.y.int32 - tileSize
  margin = windowSize.x div 20

  proc loadImageTile(path: string): Image =
    loadImageSvg(path, tileSize, tileSize)

  template loadAllDirection(textures, image) =
    for texture in textures.mitems:
      texture = loadTextureFromImage(image)
      imageRotateCW(image)

  for kind, texture in textures.tiles.mpairs:
    var image = loadImageTile(&"resources/tiles/{kind}.svg")
    loadAllDirection(texture, image)

  block:
    var image = loadImageTile("resources/tiles/branch.svg")
    loadAllDirection(textures.branchTile[left], image)
    imageFlipHorizontal(image)
    loadAllDirection(textures.branchTile[right], image)

  block:
    var image = loadImageTile("resources/slot_mark.svg")
    loadAllDirection(textures.slotMark, image)

  textures.ball = loadTextureSvg("resources/ball.svg", tileSize, 0)

  levelFont = newFontSize(tileSize div 3 * 2 + padding)

  xpBar.texture = loadTextureSvg("resources/xpbar.svg", 0, levelFont.baseSize)
  xpBar.center = ivec2(vec2(xpBar.texture.size) * vec2(0.15, 0.56))
  xpBar.radius = xpBar.texture.width.float32 * 0.82

  #[restartButton.texture = loadTextureSvg("resources/restart.svg", tileSize div 3, 0)
  restartButton.position = ivec2(padding, windowSize.y - restartButton.texture.height - padding)]#

  prevScreen = loadRenderTexture(windowSize.x, windowSize.y)

  trappedTexture = loadTextureSvg("resources/trapped.svg", windowSize.x, 0)
  winscreen.loadAssets()


func offset(dir: Direction, offset: uint): Direction =
  Direction((dir.uint + offset) mod 4)

func oppositeTo(dir: Direction): Direction {.inline.} = dir.offset(2)

func sideOf(side: HandSide, dir: Direction): Direction =
  dir.offset:
    case side
    of left: 3
    of right: 1

func vec(dir: Direction): IVec2 =
  case dir
  of north: ivec2(0, -1)
  of south: ivec2(0, 1)
  of west: ivec2(-1, 0)
  of east: ivec2(1, 0)

func axis(dir: Direction): tuple[parallel, orthogonal: int] =
  case dir
  of north, south: (1, 0)
  of east , west : (0, 1)


func newTile(kind: TileKind, orientation: Direction): Tile =
  Tile(kind: kind, orientation: orientation)


func position(slot: Slot): IVec2 =
  let (parallel, orthogonal) = slot.side.axis
  result[orthogonal] = slot.index
  result[parallel] =
    if slot.side in {north, west}: -1
    else: boardSize[parallel]


converter toMotion(dir: Direction): Motion =
  Motion(kind: straight, entry: dir)

func exit(motion: Motion): Direction =
  if motion.kind == curve: motion.curveExit
  else: motion.entry


func isOnBoard(pos: IVec2): bool =
  (0..1).allIt(pos[it] in 0 ..< boardSize[it])

func posModBoard(pos: IVec2): IVec2 =
  (pos + boardSize) mod boardSize

func `[]`(board: Board, pos: IVec2): Tile =
  let pos = posModBoard(pos)
  board[pos.y][pos.x]

proc `[]=`(board: var Board, pos: IVec2, tile: Tile) =
  let pos = posModBoard(pos)
  board[pos.y][pos.x] = tile

proc remove(board: var Board, pos: IVec2): Tile =
  result = board[pos]
  board[pos] = newTile(empty, north)

iterator tiles(board: Board): (IVec2, Tile) =
  for y, row in board:
    for x, tile in row:
      yield (ivec2(x, y), tile)


func newShot(slot: Slot): Shot =
  let pos = slot.position
  Shot(
    startSlot: slot,
    ball: (pos, oppositeTo(slot.side)),
    path: @[@[pos]],
  )

func isDone(shot: Shot): bool =
  shot.distance > 0 and not shot.ball.position.isOnBoard

proc updateShot(
  board: var Board,
  shot: var Shot,
  trackPath = true,
): tuple[
  didBounce: bool,
  gotTrapped: bool,
] =

  let shot = addr shot
  let ball = addr shot.ball

  inc shot.distance

  if shot.pushBack:
    ball.motion = oppositeTo(ball.motion.exit)
  else:
    ball.motion = ball.motion.exit
    block:
      var tiles: seq[(IVec2, Tile)]
      for pos in shot.moveTiles.positions:
        tiles &= (pos, board.remove(pos))
      let offset = shot.moveTiles.direction.vec
      for (pos, tile) in tiles:
        board[pos + offset] = tile

  ball.position += ball.motion.exit.vec

  shot.moveTiles.positions = @[]

  let board = addr board

  proc getTileUnderBall: tuple[tile: Tile, isMoving: bool] =
    result.tile = board[ball.position]
    if result.tile.kind == empty and shot.moveTiles.direction == oppositeTo(ball.motion.exit):
      let p = posModBoard(ball.position + ball.motion.exit.vec)
      if p in shot.moveTiles.positions:
        result.tile = board[p]
        result.isMoving = true

  proc updateBall: bool =  # returns true if ball hit something
    result = false

    if ball.position.isOnBoard and
       (let (tile, isMoving) = getTileUnderBall(); tile.kind != empty):

      template asCurveTile(a, b: Direction) =
        let dir = ball.motion.exit
        let fromDir = oppositeTo(dir)
        ball.motion = Motion(
          kind: curve,
          tileIsMoving: isMoving,
          entry: dir,
          curveExit:
            if   fromDir == a: b
            elif fromDir == b: a
            else: return true
        )

      case tile.kind
      of curve:
        asCurveTile(
          tile.orientation,
          left.sideOf(tile.orientation)
        )
      of branch:
        asCurveTile(
          tile.orientation,
          tile.curveDirection.sideOf(tile.orientation)
        )

      of straight:
        if tile.orientation.int mod 2 != ball.motion.entry.int mod 2:
          return true

      of goal:
        let dir = ball.motion.exit
        if tile.orientation == oppositeTo(dir):
          ball.motion = Motion(kind: goal, entry: dir)
        else:
          return true

      of empty: discard

  proc pushTiles =
    var pos = ball.position

    let ballPos = posModBoard(ball.position)
    let dir = ball.motion.exit
    let offset = dir.vec
    let fromDir = oppositeTo(dir)
    
    ball.motion = fromDir
    ball.position += fromDir.vec

    shot.moveTiles.direction = dir
    while board[pos].kind != empty:
      shot.moveTiles.positions &= pos
      pos = posModBoard(pos + offset)
      if pos == ballPos:
        shot.moveTiles.positions = @[]
        break

    shot.pushBack = updateBall()

  result.didBounce = updateBall()
  if result.didBounce: pushTiles()

  if trackPath:
    if result.didBounce:
      shot.path &= @[ball.position]
    else:
      shot.path[^1] &= ball.position
    if (
      len(shot.path) > 4 and
      shot.path[^3] == shot.path[^1] and
      shot.path[^5] == shot.path[^1]
    ):
      result.gotTrapped = true

  if not result.didBounce:
    # switch branch
    let pos = ball.position - vec(if ball.motion.kind == curve: ball.motion.entry else: ball.motion.exit)
    if pos.isOnBoard:
      var tile = board[pos]
      if tile.kind == branch:
        tile.curveDirection = HandSide((tile.curveDirection.int + 1) mod 2)
        board[pos] = tile


func `$`(slots: seq[Slot]): string =
  slots.mapIt(&"{it.side}{it.index}").join(", ")

proc simulateShot(
  board: Board,
  slot: Slot
): tuple[
  board: Board,
  state: GameState,
  distance: int32,
  bounces: int32,
] =
  result.board = board
  var shot = newShot(slot)
  for _ in 0..80: # limit iters to not pause the game when trap detection fails (unlikly but better safe than sorry)
    template resturnWithState(s: GameState) =
      result.state = s
      result.distance = shot.distance - int32(s == running)  # if running, compensate for whats explained below
      return
    let info = updateShot(result.board, shot)
    if info.didBounce:
      inc result.bounces
    if info.gotTrapped:
      resturnWithState trapped
    if shot.ball.motion.kind == goal:
      resturnWithState won
    if shot.isDone:
      discard updateShot(result.board, shot)  # need to apply all shifts allways
      resturnWithState running

type Solution = object
  path: seq[Slot]
  distance, bounces: int32

proc getSolutions(board: Board, checkEmptyRows: static bool): seq[Solution] =
  var solutions: seq[Solution]

  proc getSolutions(board: Board, depth: int, solution: Solution) =
    if depth == 0: return
    for axis, sides in [[north, south], [east, west]]:
      for index in 0 ..< boardSize[axis]:
        for side in sides:
          when checkEmptyRows:
            block filterEmptyRows:
              var pos = ivec2()
              pos[axis] = index
              for i in 0 ..< boardSize[1-axis]:
                pos[1-axis] = i
                if board[pos].kind != empty:
                  break filterEmptyRows
              continue

          let slot = Slot(side: side, index: index)
          let (board, state, distance, bounces) = board.simulateShot(slot)
          var solution = solution
          solution.distance += distance
          solution.bounces += bounces
          solution.path &= slot
          case state
          of trapped: discard
          of running: getSolutions(board, depth-1, solution)
          of won: solutions &= solution

  getSolutions(board, boardAnalyzeDepth, Solution())
  solutions

proc newRandomBoardUnchecked(blockCount: int): Board =
  let goalDir = rand(Direction)
  let goalPos = ivec2(
    rand(1i32 .. boardSize.x-2).int32,
    rand(1i32 .. boardSize.y-2).int32,
  )
  result[goalPos] = newTile(goal, goalDir)

  # block sides
  for axis in 0..1:
    var pos = goalPos
    pos[axis] = rand(goalPos[axis]+1 .. boardSize[axis]-1)
    result[pos] = newTile(rand(branch..TileKind.straight), rand(Direction))
    pos[axis] = rand(0i32 .. goalPos[axis]-1)
    result[pos] = newTile(rand(branch..TileKind.straight), rand(Direction))

  assert blockCount >= 4
  let blockCount = blockCount - 4

  let blockProb = blockCount / static(boardSize.x*boardSize.y)
  block fillBoard:
    var placedBlocks = 0
    for row in result.mitems:
      for tile in row.mitems:
        if tile.kind == empty and rand(0.0..1.0) < blockProb:
          inc placedBlocks
          if placedBlocks > blockCount:
            break fillBoard
          tile = newTile(
            rand(branch..TileKind.curve),
            rand(Direction))
          if tile.kind == branch:
            tile.curveDirection = rand(HandSide)

func analyze(solutions: seq[Solution]): BoardStats =
  result.minShots = min(solutions.mapIt(len(it.path).int32))
  result.minDistance = min(solutions.mapIt(it.distance))
  result.minDistanceWithMinShots = min: collect:
    for solution in solutions:
      if len(solution.path) == result.minShots:
        solution.distance

proc genNextBoard =
  let 
    #blockCount = min(26, 36 - 198 div (level + 6))
    blockCount = min(26, 32 - 106 div (level + 3))
    minBounces = min(8, level div 6 + 2)
    minDistance = min(24, level div 2 + 10)
  when not defined(android):
    echo blockCount, " : ", minBounces, " : ", minDistance

  var result: StartBoard
  while not nextStartBoardIsReady:
    result.board = newRandomBoardUnchecked(blockCount)
    let solutions =
      if blockCount <= 8:
        result.board.getSolutions(checkEmptyRows = true)
      else: result.board.getSolutions(checkEmptyRows = false)

    if len(solutions) > 0:
      result.stats = analyze(solutions)
      if (
        minDistance <= result.stats.minDistance and
        minBounces <= min(solutions.mapIt(it.bounces))
      ):
        when not defined(android):
          echo "perfect:"
          for solution in solutions:
            if len(solution.path) == result.stats.minShots and solution.distance == result.stats.minDistanceWithMinShots:
              echo solution.path
          echo "awsome:"
          for solution in solutions:
            if len(solution.path) == result.stats.minShots and solution.distance > result.stats.minDistanceWithMinShots or
               len(solution.path) == result.stats.minShots + 1:
              echo solution.path
        break
  
  if not nextStartBoardIsReady:
    nextStartBoardIsReady = true
    withLock nextStartBoardLock:
      nextStartBoard = result

proc loadNextBoard =
  sync()
  nextStartBoardIsReady = false
  for _ in 1 .. boardGenThreads:
    spawn genNextBoard()


var contextBoardOffset = vec2(0)  # to manipulate for rendertexture and animation context
proc screenPos(pos: Vec2): Vec2 =
  vec2(pos * float32(tileSize)) + contextBoardOffset

proc updateBoardInside =
  contextBoardOffset = vec2(0)

  var ballIsInGoal = false
  proc draw(tile: Tile, pos: Vec2) =
    let screenPos = screenPos(pos)
    let texture = addr (
      if tile.kind == branch: addr textures.branchTile[tile.curveDirection]
      else: addr textures.tiles[tile.kind]
    )[tile.orientation]
    if (
      tile.kind == goal and
      (Some(@shot) ?= shot) and
      shot.ball.motion.kind == goal and
      trans > 0.5
    ):
      ballIsInGoal = true
      let f = min((trans - 0.75) * 4, 1)  # map to scale -1 .. 1
      let scale = 1f32 + (1 - f*f) * 0.2
      drawTexture(
        texture[],
        screenPos(pos - vec2((scale-1)/2)),
        0, scale, White)
    else:
      drawTexture(texture[], screenPos, White)

  textureMode(boardInside):
    clearBackground(Color())
 
    for pos, tile in board.tiles:
      if tile.kind != empty:
        var pos = pos
        if (
          (Some(@shot) ?= shot) and
          pos in shot.moveTiles.positions
        ):
          let offset = shot.moveTiles.direction.vec
          let trans =
            if shot.pushBack and trans > 0.5: 1 - trans
            else: trans
          draw(tile, vec2(pos) + vec2(offset) * trans)
          if (let nextPos = posModBoard(pos + offset); nextPos != pos + offset):
            draw(tile, vec2(nextPos) - vec2(offset) * (1 - trans))
        else:
          draw(tile, vec2(pos))

    if Some(@shot) ?= shot:
      var pos = vec2(shot.ball.position)

      if shot.ball.motion.kind == curve:
        let entry = vec2(shot.ball.motion.entry.vec)
        let exit = vec2(shot.ball.motion.exit.vec)

        proc curveOffset(trans: float32): Vec2 =
          let angle = trans * PI * 0.5
          0.5f32 * (
            float32(1.0 - cos(angle)) * exit +
            float32(sin(angle) - 1.0) * entry
          )

        if not shot.ball.motion.tileIsMoving:
          pos += curveOffset(trans)
        else:
          if trans < 0.5:
            pos += entry * (trans - 0.5)
          else:
            pos += curveOffset((trans - 0.5) * 2) + entry * (1 - trans)

      elif shot.pushBack and trans > 0.5:
        pos += vec2(oppositeTo(shot.ball.motion.exit).vec) * (trans - 0.5)

      else:
        let trans =
          if shot.ball.motion.kind == goal: min(trans, 0.5)
          else: trans
        pos += vec2(shot.ball.motion.exit.vec) * (trans - 0.5)

      if ballIsInGoal:
        let shrink = clamp(trans-1, 0, 0.5)*2
        drawTexture(
          textures.ball,
          position = screenPos(pos + vec2(shrink/2)),
          rotation = 0,
          scale = 1-shrink,
          tint = colorLerp(ballColor, Black, shrink))
      else:
        drawTexture(textures.ball, screenPos(pos), ballColor)

proc drawBoard(xOffset: float32 = 0) =
  contextBoardOffset = boardOffset
  contextBoardOffset.x += xOffset

  block drawSlotMarks:
    let scale =
      if shot.isNone: trans
      elif shot.get.distance == 0: 1-trans
      else: break drawSlotMarks
    let offset = vec2((1-scale) / 2)
    for side in Direction:
      let (_, orthogonal) = side.axis
      for index in 0 ..< boardSize[orthogonal]:
        let slot = Slot(side: side, index: index)
        drawTexture(
          texture = textures.slotMark[side],
          position = screenPos(vec2(slot.position) + offset),
          rotation = 0,
          scale = scale,
          tint = Black
        )
  if Some(@shot) ?= shot:
    block drawActiveSlot:
      let trans =
        case shot.distance
        of 0: trans - 0.5
        of 1: trans + 0.5
        else: break drawActiveSlot
      let slot = shot.startSlot
      let slotPos = screenPos(vec2(slot.position))
      let ballPos = screenPos(
        vec2(slot.position) +
        vec2(vec(oppositeTo(slot.side))) * trans
      )
      drawRectangle(slotPos.x.int32, slotPos.y.int32, tileSize, tileSize, backgroundColor)
      drawTexture(textures.slotMark[slot.side], ballPos, Black)

  # black board background
  drawRoundedRect(
    pos = ivec2(screenPos(vec2(0))) - ivec2(padding),
    size = boardInside.texture.size + ivec2(2*padding)
  )

  #[block markFullRowsCols:
    for axis in 0..1:
      let ortho = 1-axis
      var size: IVec2
      size[axis] = tileSize + padding
      size[ortho] = tileSize * boardSize[ortho] + padding
      for i in 0 ..< boardSize[axis]:
        var pos: IVec2
        pos[axis] = i
        let isFull = (0i32 ..< boardSize[ortho]).allIt:
          pos[ortho] = it
          board[pos].kind != empty
        if isFull:
          pos[ortho] = 0
          let pos = screenPos(vec2(pos)) - ivec2(padding div 2)
          drawRectangle(pos.x, pos.y, size.x, size.y, Red)]#


  drawTextureVFlipped(boardInside.texture, vec2(contextBoardOffset))

proc drawUi(delta: float32) =

  block drawLevelNumber:
    var pos = ivec2(
      windowSize.x - margin - padding,
      (marginTop - levelFont.baseSize + padding) div 2)

    block drawXp:
      let offset = ivec2(-levelFont.baseSize div 8, levelFont.baseSize div 4)
      let center = pos + offset
      let maskPos = center - xpBar.center
      drawRectangle(maskPos.x, maskPos.y, xpBar.texture.width, xpBar.texture.height, shadowColor)
      let progress = mix(xpProgress.a, xpProgress.b, clamp((trans-6)/2, 0, 1))
      drawCircleSector(
        center = center,
        radius = xpBar.radius,
        startAngle = mix(50f32, -95f32, clamp(progress, 0, 1)),
        endAngle = 55,
        segments = 32,
        color = ballColor)
      drawTexture(xpBar.texture, maskPos, backgroundColor)
      let nextLevelStr = $(level + 1)
      drawText(buttonFont, nextLevelStr,
        maskPos - ivec2(textWidth(buttonFont, nextLevelStr).int32 + padding div 4, padding div 4),
        shadowColor)

    let levelStr = $level
    let levelStrWidth = textWidth(levelFont, levelStr)
    pos.x -= levelStrWidth.int32
    if isLevelingUp and (let f = 1 - abs((trans-wonTransMax)*2); f > 0):
      let grow = f*0.2
      let size = vec2(levelStrWidth, levelFont.baseSize.float32)
      drawTextWithShadow(levelFont, levelStr,
        vec2(pos) - size*(grow/2),
        colorLerp(ballColor, White, f),
        size.y * (1+grow))
    else:
      drawTextWithShadow(levelFont, levelStr, pos, ballColor)
    pos.x -= textWidth(buttonFont, "LVL").int32 - (levelFont.baseSize div 3)
    pos.y += levelFont.baseSize - (buttonFont.baseSize * 4 div 3)
    drawText(buttonFont, "LVL", pos, White)
  
  #[if state != won and shotCount > 0:
    draw restartButton]#


proc drawGame(delta: float32) =
  var xOffset = 0f32

  if Some(@trans) ?= screenTrans:
    xOffset = windowSize.x.float32 * (1f32 - trans)
    drawTextureVFlipped(prevScreen.texture, vec2(windowSize.x.float32 * -trans, 0))
  
  case state
  of running:
    drawBoard(xOffset)
  of trapped:
    drawTextureVFlipped(prevScreen.texture, vec2(0))
    let
      h = trappedTexture.height.float32
      yPos = mix(-h, (windowSize.y.float32 - h) / 2, min(1, trans*2))
    drawTexture(trappedTexture, vec2(0, yPos), White)
  of won:
    drawWinScreen(trans, nextStartBoardIsReady)


proc restart =
  isLevelingUp = false
  board = startBoard.board
  shot = none(Shot)
  state = running
  shotCount = 0
  totalDistance = 0

proc captureScreen =
  textureMode(prevScreen):
    clearBackground(Color())
    drawGame(0)

proc startScreenTrans =
  captureScreen()
  screenTrans = some(0f32)

proc startNewBoard =
  if nextStartBoardIsReady:
    isLevelingUp = false
    startScreenTrans()
    startBoard <- nextStartBoard
    loadNextBoard()
    restart()
    prevXp <- 0

proc onClickGame*(mousePos: IVec2) =

  if state == won and trans >= wonTransMax:
    if mousePos in winscreen.restartButton:
      startScreenTrans()
      restart()
    elif mousePos in winscreen.nextButton:
      startNewBoard()

  elif state == trapped:
    restart()

  elif state == running and shot.isNone:
    var fpos = (vec2(mousePos) - boardOffset) / float32(tileSize)
    for i in 0..1:
      if fpos[i] < 0: fpos[i] -= 1
    let pos = ivec2(fpos)
    for i in 0..1:
      let ortho = 1 - i
      let index = pos[ortho]
      if index in 0 ..< boardSize[ortho]:
        let side = Direction:
          if pos[i] == -1: 3*(1-i)
          elif pos[i] == boardSize[i]: i+1
          else: continue
        shot = some(newShot(Slot(side: side, index: index)))
        shotCount += 1
        trans = 0.5

proc endGame(kind: range[trapped..won]) =
  state = kind
  shot = none(Shot)
  trans = 0

proc getXpProgress: float32 =
  xp.val.float32 / min(26, 6 + level*2).float32

proc updateGame*(delta: float32) =
  updateLoadingAnim(delta)

  let prevTrans = trans
  trans += delta * animationSpeed * (
    if (Some(@shot) ?= shot) and shot.ball.motion.kind == curve:
      4/PI
    else: 1
  )

  case state
  of running:
    if Some(@shotVal) ?= shot:
      if shotVal.ball.motion.kind == goal:
        if trans > 4:
          startScreenTrans()
          endGame(won)
          totalDistance += shotVal.distance

          let newXp = initWinScreen(startBoard.stats, shotCount, totalDistance).xp
          if newXp > prevXp:
            xp <- xp + newXp - prevXp
            prevXp <- newXp
            xpProgress.b = getXpProgress()
            if xpProgress.b >= 1:
              shouldLevelUp = true
              isLevelingUp = true

      elif trans > 1:
        trans = 0
        if updateShot(board, shot.get).gotTrapped:
          captureScreen()
          endGame(trapped)
        elif shotVal.isDone:
          shot = none(Shot)
          totalDistance += shotVal.distance

    elif trans > 1: trans = 1
    
    updateBoardInside()

  of trapped:
    trans = min(trans, 1)
  
  of won:
    if trans > wonTransMax:
      if shouldLevelUp:
        shouldLevelUp = false
        level <- level + 1
        xpProgress = 0f32..0f32
        xp <- 0
      else:
        xpProgress.a = xpProgress.b
    trans = min(10, trans)
    if isGestureDetected(SwipeLeft):
      startNewBoard()

  if Some(@trans) ?= screenTrans:
    screenTrans.get += delta * animationSpeed
    if trans > 1:
      screenTrans = none(float32)

  drawing:
    clearBackground(backgroundColor)
    drawUi(delta)
    drawGame(delta)

when defined(buildFirstBoard):
  var result: StartBoard
  result.board[1][2] = newTile(branch, east)
  result.board[1][4] = newTile(curve, south)
  result.board[2][0] = newTile(curve, north)
  result.board[4][1] = newTile(straight, east)
  result.board[5][1] = newTile(goal, north)
  result.stats = analyze(result.board.getSolutions(checkEmptyRows = true))
  saveFileData("../resources/first_board", result)

proc initGame* =
  if not xp.load(): xp <- 0
  if not prevXp.load(): prevXp <- 0

  let firstOpen = not level.load()
  if firstOpen:
    level <- 1

  xpProgress.b = getXpProgress()
  xpProgress.a = xpProgress.b
  
  if firstOpen or not startBoard.load():
    startBoard <- loadFileData[StartBoard]("resources/first_board")
  loadNextBoard()
  restart()
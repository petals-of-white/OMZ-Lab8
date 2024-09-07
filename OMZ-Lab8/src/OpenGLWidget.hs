{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module OpenGLWidget (
  openGLWidget
) where

import           Control.Lens
import           Coordinates
import           Data.Default
import           Data.Typeable                (cast)
import           Data.Vector.Storable         (Vector)
import           Foreign.Ptr
import           Foreign.Storable

import qualified Data.Vector.Storable         as V

import           Data.Word                    (Word8)
import           Foreign.Marshal              (withArrayLen)
import           Graphics.Rendering.OpenGL.GL as GL hiding (color)
import           Linear
import           Linear.OpenGL                (m44GLmatrix)
import           Monomer
import qualified Monomer.Lens                 as L
import           Monomer.Widgets.Single

import           Log                          (writeLog)
import           Shaders
import           Types
data OpenGLWidgetMsg =
  OpenGLWidgetInit Program Program VertexArrayObject BufferObject TextureObject TextureObject
  | OpenGLWidgetError String
  deriving (Show, Eq)

data OpenGLWidgetState = OpenGLWidgetState {
  _ogsProgramOne    :: Program,
  _ogsProgramFusion :: Program,
  _ogsVao           :: VertexArrayObject,
  _ogsVbo           :: BufferObject,
  _ogsTexture1      :: TextureObject,
  _ogsTexture2      :: TextureObject
} deriving (Show, Eq)


openGLWidget :: SimpleImage Word8 -> SimpleImage Word8 -> DisplayMode -> M44 Float -> WidgetNode s e

openGLWidget img1 img2 displayMode transMatrix = defaultWidgetNode "openGLWidget" widget where
  widget = makeOpenGLWidget img1 img2 transMatrix displayMode glState
  glState = Nothing

makeOpenGLWidget :: SimpleImage Word8 -> SimpleImage Word8 -> M44 Float -> DisplayMode ->  Maybe OpenGLWidgetState -> Widget s e

makeOpenGLWidget
  img1@SimpleImage{imgPixels=imgPix1, imgRows=rows1, imgColumns=cols1}
  img2@SimpleImage{imgPixels=imgPix2, imgRows=rows2, imgColumns=cols2}
  transMat
  displayMode
  state = widget where
  widget = createSingle state def {
              singleInit = initialize,
              singleMerge = merge,
              singleDispose = dispose,
              singleHandleMessage = handleMessage,
              singleGetSizeReq = getSizeReq,
              singleRender = render
  }

  initialize _wenv node = resultReqs node reqs where
    widgetId = node ^. L.info . L.widgetId
    path = node ^. L.info . L.path

    floatSize = sizeOf (undefined :: Float)
    initOpenGL = do
      -- This needs to run in render thread
      debugOutput $= Enabled
      debugMessageCallback $= Just (\dmsg@(DebugMessage _ dtype _ _ msg) ->
          case dtype of
            DebugTypeError              -> writeLog (show dmsg)
            DebugTypeUndefinedBehavior  -> writeLog (show dmsg)
            DebugTypeDeprecatedBehavior -> writeLog (show dmsg)
            _                           -> writeLog (show msg)

        )

      glVersion >>= writeLog

      vbo <- genObjectName
      vao <- genObjectName

      bindVertexArrayObject $= Just vao

      bindBuffer ArrayBuffer $= Just vbo


      bufferData ArrayBuffer $= (fromIntegral (floatSize * 4 * 6), nullPtr, StaticDraw)

      --First texture
      tex1 <- genObjectName
      activeTexture $= TextureUnit 0
      textureBinding Texture2D $= Just tex1

      rowAlignment Unpack $= 1
      textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
      textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
      textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

      withArrayLen imgPix1 (\ _ ptr ->
        let pixData = PixelData Red UnsignedByte ptr in
        texImage2D Texture2D NoProxy 0 R8 (TextureSize2D (fromIntegral cols1) (fromIntegral rows1)) 0 pixData
        )

      -- textureBinding Texture2D $= Nothing


      -- Second texture
      tex2 <- genObjectName
      activeTexture $= TextureUnit 1
      textureBinding Texture2D $= Just tex2

      rowAlignment Unpack $= 1
      textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
      textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
      textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

      withArrayLen imgPix2 (\ _ ptr ->
        let pixData = PixelData Red UnsignedByte ptr in
        texImage2D Texture2D NoProxy 0 R8 (TextureSize2D (fromIntegral cols2) (fromIntegral rows2)) 0 pixData
        )


      -- textureBinding Texture2D $= Nothing


      prog1 <- createShaderProgram vertShaderPath fragShader1Path
      currentProgram $= Just prog1
      setTransMat prog1 transMat
      setTexture1 prog1 0

      progFusion <- createShaderProgram vertShaderPath fragShaderFusionPath
      currentProgram $= Just progFusion
      setTransMat progFusion transMat
      setTexture1 progFusion 0
      setTexture2 progFusion 1

      currentProgram $= Nothing
      textureBinding Texture2D $= Nothing


      return $ OpenGLWidgetInit prog1 progFusion vao vbo tex1 tex2

    reqs = [RunInRenderThread widgetId path initOpenGL]

  merge _wenv node _oldNode oldState = resultNode newNode where
    newNode = node
           & L.widget .~ makeOpenGLWidget img1 img2 transMat displayMode oldState

  dispose _wenv node = resultReqs node reqs where

    widgetId = node ^. L.info . L.widgetId
    path = node ^. L.info . L.path
    disposeOpenGL = do
      case state of
        Just (OpenGLWidgetState program1 programFusion vao vbo tex1 tex2)  -> do
          writeLog "Disposing..."
          deleteObjectName tex1
          deleteObjectName tex2
          deleteObjectName vao
          deleteObjectName vbo
          deleteObjectNames [program1, programFusion]
          writeLog "disposed"
        Nothing -> pure ()

    reqs = [RunInRenderThread widgetId path disposeOpenGL]

  handleMessage _wenv node _target msg = case cast msg of
    Just (OpenGLWidgetInit prog1 progFusion vao vbo tex1 tex2) -> Just result where
      newState = Just (OpenGLWidgetState prog1 progFusion vao vbo tex1 tex2)
      newNode = node
        & L.widget .~ makeOpenGLWidget img1 img2 transMat displayMode newState
      result = resultReqs newNode [RenderOnce]
    _ -> Nothing

  getSizeReq _wenv _node = (sizeReqW_, sizeReqH_) where
    sizeReqW_ = fixedSize (fromIntegral (max cols1 cols2))
    sizeReqH_ = fixedSize (fromIntegral (max rows1 rows2))

  render wenv node renderer_ =
    case state of
      Just actualState -> do
        createRawTask renderer_ (do

          writeLog $  "WinSize:" ++ show winSize ++  ". Active VP::" ++ show activeVp
                      ++ ". Node content area: " ++ show nodeVp ++ ". Dpr: " ++ show dpr

          writeLog $ "Texture coords: " ++ show textureCoords

          doInScissor winSize dpr offset activeVp $
            let vertices = V.fromList $
                            concat [ map realToFrac [x, y, u, v] | (Monomer.Point x y, Monomer.Point u v) <- textureCoords]
            in do
            writeLog $ "vertices: " ++ show vertices

            drawVertices transMat actualState displayMode vertices
          )
      Nothing -> writeLog "Nothing to Render."

    where
      dpr = wenv ^. L.dpr
      winSize = wenv ^. L.windowSize
      activeVp = wenv ^. L.viewport
      style = currentStyle wenv node
      nodeVp = getContentArea node style

      offset = wenv ^. L.offset

doInScissor :: Monomer.Size -> Double -> Point -> Monomer.Rect -> IO () -> IO ()
doInScissor winSize dpr offset vp action = do

  -- OpenGL's Y axis increases from bottom to top
  scissor $= Just (Position (round $ rx+ox) (round $ winH - ry - oy - rh), GL.Size (round rw) (round rh))
  action
  scissor $= Nothing
  where
    winH = winSize ^. L.h * dpr
    Monomer.Point ox oy = mulPoint dpr offset
    Rect rx ry rw rh = mulRect dpr vp



drawVertices :: M44 Float -> OpenGLWidgetState -> DisplayMode -> Vector Float -> IO ()
drawVertices transmat state displayMode vertices = do

  bindVertexArrayObject $= Just vao

  bindBuffer ArrayBuffer $= Just vbo


  V.unsafeWith vertices $ \vertsPtr ->
    bufferSubData ArrayBuffer WriteToBuffer 0 (fromIntegral (V.length vertices * floatSize)) vertsPtr

  -- GL coords
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 2 Float (fromIntegral (floatSize * 4)) nullPtr)
  vertexAttribArray (AttribLocation 0) $= Enabled

  -- UV
  vertexAttribPointer (AttribLocation 1) $=
    (ToFloat, VertexArrayDescriptor 2 Float (fromIntegral (floatSize * 4)) (nullPtr `plusPtr` (floatSize * 2)))

  vertexAttribArray (AttribLocation 1) $= Enabled

  -- Configure shader program
  currentProgram $= Just program1
  transformMatLoc <- get $ uniformLocation program1 "u_transform"
  uniform transformMatLoc $= transmat ^. m44GLmatrix


  case displayMode of
    DisplayFirst -> do
  -- draw first images
      activeTexture $= TextureUnit 0
      textureBinding Texture2D $= Just tex1
      setTexture1 program1 0
      currentProgram $= Just program1
      drawArrays Triangles 0 6

    DisplaySecond -> do
      
      activeTexture $= TextureUnit 1
      textureBinding Texture2D $= Just tex2

      setTexture1 program1 1
      currentProgram $= Just program1
      drawArrays Triangles 0 6

    DisplayFusionFirstSecond -> do
      currentProgram $= Just programFusion

      setTexture1 programFusion 0

      setTexture2 programFusion 1

      drawArrays Triangles 0 6
  
    DisplayFusionSecondFirst -> do
      currentProgram $= Just programFusion

      setTexture1 programFusion 1
      setTexture2 programFusion 0

      drawArrays Triangles 0 6

  where
    floatSize = sizeOf (undefined :: Float)
    OpenGLWidgetState program1 programFusion vao vbo tex1 tex2 = state

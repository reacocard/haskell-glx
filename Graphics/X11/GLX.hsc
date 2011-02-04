module Graphics.X11.GLX
    ( GLXPixmap
    , GLXDrawable
    , GLXFBConfigID
    , GLXContextID
    , GLXWindow
    , GLXPbuffer

    , GLXEventType
    , glxDamaged
    , glxSaved

    , GLXDrawType
    , glxWindow
    , glxPBuffer

    , GLXClientStringName
    , glxClientVendor
    , glxClientVersion
    , glxClientExtensions

    , GLXRenderType
    , glxRgbaType
    , glxColorIndexType

    , GLXAttribute
    , glxUseGL
    , glxBufferSize    
    , glxLevel         
    , glxRGBA          
    , glxDoubleBuffer  
    , glxStereo        
    , glxAuxBuffers    
    , glxRedSize       
    , glxGreenSize     
    , glxBlueSize      
    , glxAlphaSize     
    , glxDepthSize     
    , glxStencilSize   
    , glxAccumRedSize  
    , glxAccumGreenSize
    , glxAccumBlueSize 
    , glxAccumAlphaSize
    , glxVisualId
    , glxConfigCaveat
    , glxRenderType

    , glxNone
    , glxSlowConfig
    
    , glxRgbaBit

    , XVisualInfo
    , GLXContext(..)
    , GLXFBConfig

    -- glx 1.0
    , glXChooseVisual
    , glXCreateContext
    , glXDestroyContext
    , glXMakeCurrent
    , glXCopyContext
    , glXSwapBuffers
    , glXCreateGLXPixmap
    , glXDestroyGLXPixmap
    , glXQueryExtension
    , glXQueryVersion
    , glXIsDirect
    , glXGetConfig
    , glXGetCurrentContext
    , glXGetCurrentDrawable
    , glXWaitGL
    , glXWaitX
    , glXUseXFont

    -- glx 1.1
    , glXQueryExtensionsString
    , glXQueryServerString
    , glXGetClientString

    -- glx 1.2
    , glXGetCurrentDisplay
    
    -- glx 1.3
    , glXChooseFBConfig
    , glXGetFBConfigAttrib
    , glXGetFBConfigs
    , glXGetVisualFromFBConfig
    , glXCreateWindow
    , glXDestroyWindow
    , glXCreatePixmap
    , glXDestroyPixmap
    , glXCreatePbuffer
    , glXDestroyPbuffer
    , glXQueryDrawable
    , glXCreateNewContext
    , glXGetCurrentReadDrawable
    , glXQueryContext 
    , glXSelectEvent
    , glXGetSelectedEvent

    -- texture from pixmap
--    , glXBindTexImageEXT
--    , glXReleaseTexImageEXT

    -- misc
    , xviVisualId
    , withAttrList
    ) where


-- NAMING CONVENTIONS: 
-- types begin with GLX
-- instances begin with glx
-- functions begin with glX
-- the remainder of each name is the same as the C function, 
-- but with camelcase instead of underscores. 
-- e.g. GLX_BUFFER_SIZE becomes GLXBufferSize
--
-- functions and data that do not have a direct C correspondent do not begin with glx.


-- TODO
-- - make sure everything is using Maybe instead of null pointers
-- - find a way to reduce duplication of code
-- - pretty much EVERYTHING related to FBConfigs is broken, no idea why

#include "GL/glx.h"

import Data.Maybe
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign

import Graphics.Rendering.OpenGL.Raw
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Types

type GLXPixmap      = XID
type GLXDrawable    = XID
type GLXFBConfigID  = XID
type GLXContextID   = XID
type GLXWindow      = XID
type GLXPbuffer     = XID


type GLXEventType = CInt

glxDamaged :: GLXEventType
glxDamaged = 0x8020

glxSaved :: GLXEventType
glxSaved = 0x8021

type GLXDrawType = CInt

glxWindow :: GLXDrawType
glxWindow = 0x8022

glxPBuffer :: GLXDrawType
glxPBuffer = 0x8023

type GLXClientStringName = CInt
glxClientVendor :: GLXClientStringName
glxClientVendor = (#const GLX_VENDOR)
glxClientVersion :: GLXClientStringName
glxClientVersion = (#const GLX_VERSION)
glxClientExtensions :: GLXClientStringName
glxClientExtensions = (#const GLX_EXTENSIONS)

type GLXRenderType = CInt
glxRgbaType :: GLXRenderType
glxRgbaType = (#const GLX_RGBA_TYPE)
glxColorIndexType :: GLXRenderType
glxColorIndexType = (#const GLX_COLOR_INDEX_TYPE)

type GLXAttribute = Int32
-- attributes
glxUseGL            :: GLXAttribute
glxUseGL            = (#const GLX_USE_GL)
glxBufferSize       :: GLXAttribute
glxBufferSize       = (#const GLX_BUFFER_SIZE)
glxLevel            :: GLXAttribute
glxLevel            = (#const GLX_LEVEL)
glxRGBA             :: GLXAttribute
glxRGBA             = (#const GLX_RGBA)
glxDoubleBuffer     :: GLXAttribute
glxDoubleBuffer     = (#const GLX_DOUBLEBUFFER)
glxStereo           :: GLXAttribute
glxStereo           = (#const GLX_STEREO)
glxAuxBuffers       :: GLXAttribute
glxAuxBuffers       = (#const GLX_AUX_BUFFERS)
glxRedSize          :: GLXAttribute
glxRedSize          = (#const GLX_RED_SIZE)
glxGreenSize        :: GLXAttribute
glxGreenSize        = (#const GLX_GREEN_SIZE)
glxBlueSize         :: GLXAttribute
glxBlueSize         = (#const GLX_BLUE_SIZE)
glxAlphaSize        :: GLXAttribute
glxAlphaSize        = (#const GLX_ALPHA_SIZE)
glxDepthSize        :: GLXAttribute
glxDepthSize        = (#const GLX_DEPTH_SIZE)
glxStencilSize      :: GLXAttribute
glxStencilSize      = (#const GLX_STENCIL_SIZE)
glxAccumRedSize     :: GLXAttribute
glxAccumRedSize     = (#const GLX_ACCUM_RED_SIZE)
glxAccumGreenSize   :: GLXAttribute
glxAccumGreenSize   = (#const GLX_ACCUM_GREEN_SIZE)
glxAccumBlueSize    :: GLXAttribute
glxAccumBlueSize    = (#const GLX_ACCUM_BLUE_SIZE)
glxAccumAlphaSize   :: GLXAttribute
glxAccumAlphaSize   = (#const GLX_ACCUM_ALPHA_SIZE)
glxVisualId         :: GLXAttribute
glxVisualId         = (#const GLX_VISUAL_ID)
glxConfigCaveat     :: GLXAttribute
glxConfigCaveat     = (#const GLX_CONFIG_CAVEAT)
glxRenderType       :: GLXAttribute
glxRenderType       = (#const GLX_RENDER_TYPE)

-- glxConfigCaveat values
glxNone             :: GLXAttribute
glxNone             = (#const GLX_NONE)
glxSlowConfig       :: GLXAttribute
glxSlowConfig       = (#const GLX_SLOW_CONFIG)

-- glxRenderType values
glxRgbaBit          :: GLXAttribute
glxRgbaBit          = (#const GLX_RGBA_BIT)



newtype XVisualInfo = XVisualInfo (Ptr XVisualInfo)
newtype GLXContext = GLXContext (Ptr GLXContext)
newtype GLXFBConfig = GLXFBConfig (Ptr GLXFBConfig)

glXChooseVisual :: Display -> ScreenNumber -> [GLXAttribute] -> IO (Maybe XVisualInfo)
glXChooseVisual dpy screen attrs = do
    xvi@(XVisualInfo xviPtr) <- withAttrList attrs $ cglXChooseVisual dpy screen
    if xviPtr == nullPtr
        then return Nothing
        else return (Just xvi)
foreign import ccall unsafe "glXChooseVisual"
    cglXChooseVisual :: Display -> ScreenNumber -> Ptr Int32 -> IO XVisualInfo

glXCreateContext :: Display -> XVisualInfo -> Maybe GLXContext -> Bool -> IO (Maybe GLXContext)
glXCreateContext dpy xvi share direct = do
    ctx@(GLXContext ctxPtr) <- cglXCreateContext dpy xvi (fromMaybe (GLXContext nullPtr) share) direct
    if ctxPtr == nullPtr
        then return Nothing
        else return (Just ctx)
foreign import ccall unsafe "glXCreateContext"
    cglXCreateContext :: Display -> XVisualInfo -> GLXContext -> Bool -> IO GLXContext

foreign import ccall unsafe "glXDestroyContext"
    glXDestroyContext :: Display -> GLXContext -> IO ()

foreign import ccall unsafe "glXMakeCurrent"
    glXMakeCurrent :: Display -> GLXDrawable -> GLXContext -> IO Bool

foreign import ccall unsafe "glXCopyContext"
    glXCopyContext :: Display -> GLXContext -> GLXContext -> CULong -> IO ()

foreign import ccall unsafe "glXSwapBuffers"
    glXSwapBuffers :: Display -> GLXDrawable -> IO ()

foreign import ccall unsafe "glXCreateGLXPixmap"
    glXCreateGLXPixmap :: Display -> XVisualInfo -> Pixmap -> IO GLXPixmap

foreign import ccall unsafe "glXDestroyGLXPixmap"
    glXDestroyGLXPixmap :: Display -> GLXPixmap -> IO ()

glXQueryExtension :: Display -> IO (Maybe (CInt, CInt))
glXQueryExtension dpy = wrapPtr2 (cglXQueryExtension dpy) go
    where go False _ _        = Nothing
          go True major minor = Just (fromIntegral major, fromIntegral minor)
foreign import ccall unsafe "glXQueryExtension"
    cglXQueryExtension :: Display -> Ptr Int -> Ptr Int -> IO Bool

glXQueryVersion :: Display -> IO (Maybe (CInt, CInt))
glXQueryVersion dpy = wrapPtr2 (cglXQueryVersion dpy) go
    where go False _ _        = Nothing
          go True major minor = Just (fromIntegral major, fromIntegral minor)
foreign import ccall unsafe "glXQueryVersion"
    cglXQueryVersion :: Display -> Ptr Int -> Ptr Int -> IO Bool

foreign import ccall unsafe "glXIsDirect"
    glXIsDirect :: Display -> GLXContext -> IO Bool

foreign import ccall unsafe "glXGetConfig"
    glXGetConfig :: Display -> XVisualInfo -> CInt -> Ptr CInt -> IO CInt

glXGetCurrentContext :: IO (Maybe GLXContext)
glXGetCurrentContext = do
    ctx@(GLXContext ctxPtr) <- cglXGetCurrentContext
    if ctxPtr == nullPtr
        then return Nothing
        else return (Just ctx)
foreign import ccall unsafe "glXGetCurrentContext"
    cglXGetCurrentContext :: IO GLXContext

foreign import ccall unsafe "glXGetCurrentDrawable"
    glXGetCurrentDrawable :: IO GLXDrawable

foreign import ccall unsafe "glXWaitGL"
    glXWaitGL :: IO ()

foreign import ccall unsafe "glXWaitX"
    glXWaitX :: IO ()

foreign import ccall unsafe "glXUseXFont"
    glXUseXFont :: Font -> CInt -> CInt -> CInt -> IO ()


-- GLX 1.1 and later
glXQueryExtensionsString :: Display -> ScreenNumber -> IO [String]
glXQueryExtensionsString dpy screen = do
    exts   <- cglXQueryExtensionsString dpy screen
    stexts <- peekCString exts
    return $ words stexts
foreign import ccall unsafe "glXQueryExtensionsString"
    cglXQueryExtensionsString :: Display -> ScreenNumber -> IO (Ptr CChar)

glXQueryServerString :: Display -> ScreenNumber -> IO String
glXQueryServerString dpy screen = do
    css    <- cglXQueryServerString dpy screen
    if css == nullPtr
        then return ""
        else peekCString css
foreign import ccall unsafe "glXQueryServerString"
    cglXQueryServerString :: Display -> ScreenNumber -> IO (Ptr CChar)

glXGetClientString :: Display -> GLXClientStringName -> IO String
glXGetClientString dpy screen = do
    css    <- cglXGetClientString dpy screen
    peekCString css
foreign import ccall unsafe "glXGetClientString"
    cglXGetClientString :: Display -> GLXClientStringName -> IO (Ptr CChar)


-- GLX 1.2 and later
foreign import ccall unsafe "glXGetCurrentDisplay"
    glXGetCurrentDisplay :: IO Display


-- GLX 1.3 and later

glXChooseFBConfig :: Display -> ScreenNumber -> [GLXAttribute] -> IO [GLXFBConfig]
glXChooseFBConfig dpy screen attrs =
    alloca $ \n -> 
        withAttrList attrs $ \attrlist -> do
            fbcPtr <- cglXChooseFBConfig dpy screen attrlist n
            n <- peek n
            return $ map (GLXFBConfig) $ ptrToList fbcPtr (fromIntegral n)
    where 
        ptrToList :: Ptr a -> Int -> [Ptr a]
        ptrToList ptr 0 = []
        ptrToList ptr n = [ptr] ++ (ptrToList (plusPtr ptr (#size GLXFBConfig)) (n-1))
foreign import ccall unsafe "glXChooseFBConfig"
    cglXChooseFBConfig :: Display -> ScreenNumber -> Ptr Int32 -> Ptr CInt -> IO (Ptr GLXFBConfig)

glXGetFBConfigAttrib :: Display -> GLXFBConfig -> GLXAttribute -> IO (Maybe CInt)
glXGetFBConfigAttrib dpy fbc attr =
    alloca $ \n -> do
        status <- cglXGetFBConfigAttrib dpy fbc attr n
        if status == 0
            then do val <- peek n; return (Just val)
            else return Nothing
foreign import ccall unsafe "glXGetFBConfigAttrib"
    cglXGetFBConfigAttrib :: Display -> GLXFBConfig -> Int32 -> Ptr CInt -> IO CInt 

glXGetFBConfigs :: Display -> ScreenNumber -> IO [GLXFBConfig]
glXGetFBConfigs dpy screen =
    alloca $ \n -> do
        fbc@(GLXFBConfig fbcPtr) <- cglXGetFBConfigs dpy screen n
        n <- peek n
        return $ map (GLXFBConfig) $ ptrToList fbcPtr (fromIntegral n)
    where 
        ptrToList :: Ptr a -> Int -> [Ptr a]
        ptrToList ptr 0 = []
        ptrToList ptr n = [ptr] ++ (ptrToList (plusPtr ptr (#size GLXFBConfig)) (n-1))
foreign import ccall unsafe "glXGetFBConfigs"
    cglXGetFBConfigs :: Display -> ScreenNumber -> Ptr CInt -> IO GLXFBConfig

glXGetVisualFromFBConfig :: Display -> GLXFBConfig -> IO (Maybe XVisualInfo)
glXGetVisualFromFBConfig dpy fbc = do
    xvi@(XVisualInfo xviPtr) <- cglXGetVisualFromFBConfig dpy fbc
    if xviPtr == nullPtr
        then return Nothing
        else return (Just xvi)
foreign import ccall unsafe "glXGetVisualFromFBConfig"
    cglXGetVisualFromFBConfig :: Display -> GLXFBConfig -> IO XVisualInfo

glXCreateWindow :: Display -> GLXFBConfig -> Window -> [GLXAttribute] -> IO GLXWindow
glXCreateWindow dpy fbconfig win attrs =
    withAttrList attrs $ cglXCreateWindow dpy fbconfig win
foreign import ccall unsafe "glXCreateWindow"
    cglXCreateWindow :: Display -> GLXFBConfig -> Window -> Ptr Int32 -> IO GLXWindow

foreign import ccall unsafe "glXDestroyWindow"
    glXDestroyWindow :: Display -> GLXWindow -> IO ()

glXCreatePixmap :: Display -> GLXFBConfig -> Pixmap -> [GLXAttribute] -> IO GLXPixmap
glXCreatePixmap dpy fbconfig pixmap attrs =
    withAttrList attrs $ cglXCreatePixmap dpy fbconfig pixmap
foreign import ccall unsafe "glXCreatePixmap"
    cglXCreatePixmap :: Display -> GLXFBConfig -> Pixmap -> Ptr Int32 -> IO GLXPixmap

foreign import ccall unsafe "glXDestroyPixmap"
    glXDestroyPixmap :: Display -> GLXPixmap -> IO ()

glXCreatePbuffer :: Display -> GLXFBConfig -> [GLXAttribute] -> IO GLXPbuffer
glXCreatePbuffer dpy fbconfig attrs =
    withAttrList attrs $ cglXCreatePbuffer dpy fbconfig
foreign import ccall unsafe "glXCreatePbuffer"
    cglXCreatePbuffer :: Display -> GLXFBConfig -> Ptr Int32 -> IO GLXPbuffer

foreign import ccall unsafe "glXDestroyPbuffer"
    glXDestroyPbuffer :: Display -> GLXPbuffer -> IO ()

foreign import ccall unsafe "glXQueryDrawable"
    glXQueryDrawable :: Display -> GLXDrawable -> CInt -> Ptr CUInt -> IO ()

glXCreateNewContext :: Display -> GLXFBConfig -> GLXRenderType -> Maybe GLXContext -> Bool -> IO (Maybe GLXContext)
glXCreateNewContext dpy fbc render share direct = do
    ctx@(GLXContext ctxPtr) <- cglXCreateNewContext dpy fbc render (fromMaybe (GLXContext nullPtr) share) direct
    if ctxPtr == nullPtr
        then return Nothing
        else return (Just ctx)
foreign import ccall unsafe "glXCreateNewContext"
    cglXCreateNewContext :: Display -> GLXFBConfig -> GLXRenderType -> GLXContext -> Bool -> IO GLXContext

foreign import ccall unsafe "glXMakeContextCurrent"
    glXMakeContextCurrent :: Display -> GLXDrawable -> GLXDrawable -> GLXContext -> IO Bool

foreign import ccall unsafe "glXGetCurrentReadDrawable"
    glXGetCurrentReadDrawable :: IO GLXDrawable

foreign import ccall unsafe "glXQueryContext"
    glXQueryContext :: Display -> GLXContext -> CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "glXSelectEvent"
    glXSelectEvent :: Display -> GLXDrawable -> CULong -> IO ()

foreign import ccall unsafe "glXGetSelectedEvent"
    glXGetSelectedEvent :: Display -> GLXDrawable -> CULong -> IO ()



-- helper functions

xviVisualId :: XVisualInfo -> IO VisualID
xviVisualId (XVisualInfo xvi) = ((#peek XVisualInfo, visualid) xvi)

xviDepth :: XVisualInfo -> IO CInt
xviDepth (XVisualInfo xvi) = ((#peek XVisualInfo, depth) xvi)

withAttrList :: [GLXAttribute] -> (Ptr Int32 -> IO a) -> IO a
withAttrList attrs f = withArray0 (#const None) attrs $ \attrlist -> f attrlist

-- Borrowed from the Xdamage bindings
wrapPtr2 :: (Storable a, Storable b) => (Ptr a -> Ptr b -> IO c) -> (c -> a -> b -> d) -> IO d
wrapPtr2 cfun f =
  withPool $ \pool -> do aptr <- pooledMalloc pool
                         bptr <- pooledMalloc pool
                         ret <- cfun aptr bptr
                         a <- peek aptr
                         b <- peek bptr
                         return (f ret a b)

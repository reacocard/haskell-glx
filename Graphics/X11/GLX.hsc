module Graphics.X11.GLX
    ( GLXPixmap
    , GLXDrawable
    , GLXFBConfigID
    , GLXContextID
    , GLXWindow
    , GLXPbuffer

    , GLXEventType
    , glXDamaged
    , glXSaved

    , GLXDrawType
    , glXWindow
    , glXPBuffer

    , ClientStringName
    , glxClientVendor
    , glxClientVersion
    , glxClientExtensions

    , RenderType
    , glxRgbaType
    , glxColorIndexType

    , Attribute
    , attrBufferSize    
    , attrLevel         
    , attrRGBA          
    , attrDoubleBuffer  
    , attrStereo        
    , attrAuxBuffers    
    , attrRedSize       
    , attrGreenSize     
    , attrBlueSize      
    , attrAlphaSize     
    , attrDepthSize     
    , attrStencilSize   
    , attrAccumRedSize  
    , attrAccumGreenSize
    , attrAccumBlueSize 
    , attrAccumAlphaSize

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
    , visual
    , depth
    , visualId
    , withAttrList
    ) where

-- TODO
-- - make sure everything is using Maybe instead of null pointers
-- - find a way to reduce duplication of code

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

glXDamaged :: GLXEventType
glXDamaged = 0x8020

glXSaved :: GLXEventType
glXSaved = 0x8021

type GLXDrawType = CInt

glXWindow :: GLXDrawType
glXWindow = 0x8022

glXPBuffer :: GLXDrawType
glXPBuffer = 0x8023

type ClientStringName = CInt
glxClientVendor :: ClientStringName
glxClientVendor = (#const GLX_VENDOR)
glxClientVersion :: ClientStringName
glxClientVersion = (#const GLX_VERSION)
glxClientExtensions :: ClientStringName
glxClientExtensions = (#const GLX_EXTENSIONS)

type RenderType = CInt
glxRgbaType :: RenderType
glxRgbaType = (#const GLX_RGBA_TYPE)
glxColorIndexType :: RenderType
glxColorIndexType = (#const GLX_COLOR_INDEX_TYPE)

type Attribute = Int32
attrBufferSize      :: Attribute
attrBufferSize      = (#const GLX_BUFFER_SIZE)      :: Int32
attrLevel           :: Attribute
attrLevel           = (#const GLX_LEVEL)            :: Int32
attrRGBA            :: Attribute
attrRGBA            = (#const GLX_RGBA)             :: Int32
attrDoubleBuffer    :: Attribute
attrDoubleBuffer    = (#const GLX_DOUBLEBUFFER)     :: Int32
attrStereo          :: Attribute
attrStereo          = (#const GLX_STEREO)           :: Int32
attrAuxBuffers      :: Attribute
attrAuxBuffers      = (#const GLX_AUX_BUFFERS)      :: Int32
attrRedSize         :: Attribute
attrRedSize         = (#const GLX_RED_SIZE)         :: Int32
attrGreenSize       :: Attribute
attrGreenSize       = (#const GLX_GREEN_SIZE)       :: Int32
attrBlueSize        :: Attribute
attrBlueSize        = (#const GLX_BLUE_SIZE)        :: Int32
attrAlphaSize       :: Attribute
attrAlphaSize       = (#const GLX_ALPHA_SIZE)       :: Int32
attrDepthSize       :: Attribute
attrDepthSize       = (#const GLX_DEPTH_SIZE)       :: Int32
attrStencilSize     :: Attribute
attrStencilSize     = (#const GLX_STENCIL_SIZE)     :: Int32
attrAccumRedSize    :: Attribute
attrAccumRedSize    = (#const GLX_ACCUM_RED_SIZE)   :: Int32
attrAccumGreenSize  :: Attribute
attrAccumGreenSize  = (#const GLX_ACCUM_GREEN_SIZE) :: Int32
attrAccumBlueSize   :: Attribute
attrAccumBlueSize   = (#const GLX_ACCUM_BLUE_SIZE)  :: Int32
attrAccumAlphaSize  :: Attribute
attrAccumAlphaSize  = (#const GLX_ACCUM_ALPHA_SIZE) :: Int32

newtype XVisualInfo = XVisualInfo (Ptr XVisualInfo)
newtype GLXContext = GLXContext (Ptr GLXContext)
newtype GLXFBConfig = GLXFBConfig (Ptr GLXFBConfig)

glXChooseVisual :: Display -> ScreenNumber -> [Attribute] -> IO (Maybe XVisualInfo)
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

glXGetClientString :: Display -> ClientStringName -> IO String
glXGetClientString dpy screen = do
    css    <- cglXGetClientString dpy screen
    peekCString css
foreign import ccall unsafe "glXGetClientString"
    cglXGetClientString :: Display -> ClientStringName -> IO (Ptr CChar)


-- GLX 1.2 and later
foreign import ccall unsafe "glXGetCurrentDisplay"
    glXGetCurrentDisplay :: IO Display


-- GLX 1.3 and later
glXChooseFBConfig :: Display -> ScreenNumber -> [Attribute] -> IO (Maybe GLXFBConfig)
glXChooseFBConfig dpy screen attrs = do
    fbc@(GLXFBConfig fbcPtr) <- alloca $ \n -> 
                                withAttrList attrs $ \attrlist -> 
                                cglXChooseFBConfig dpy screen attrlist n
    if fbcPtr == nullPtr
        then return Nothing
        else return (Just fbc)
foreign import ccall unsafe "glXChooseFBConfig"
    cglXChooseFBConfig :: Display -> ScreenNumber -> Ptr Int32 -> Ptr CInt -> IO GLXFBConfig

glXGetFBConfigAttrib :: Display -> GLXFBConfig -> Attribute -> IO (Maybe CInt)
glXGetFBConfigAttrib dpy fbc attr =
    alloca $ \n -> do
        status <- cglXGetFBConfigAttrib dpy fbc attr n
        if status == 0
            then do val <- peek n; return (Just val)
            else return Nothing
foreign import ccall unsafe "glXGetFBConfigAttrib"
    cglXGetFBConfigAttrib :: Display -> GLXFBConfig -> Int32 -> Ptr CInt -> IO CInt 

foreign import ccall unsafe "glXGetFBConfigs"
    glXGetFBConfigs :: Display -> ScreenNumber -> Ptr CInt -> IO GLXFBConfig

glXGetVisualFromFBConfig :: Display -> GLXFBConfig -> IO (Maybe XVisualInfo)
glXGetVisualFromFBConfig dpy fbc = do
    xvi@(XVisualInfo xviPtr) <- cglXGetVisualFromFBConfig dpy fbc
    if xviPtr == nullPtr
        then return Nothing
        else return (Just xvi)
foreign import ccall unsafe "glXGetVisualFromFBConfig"
    cglXGetVisualFromFBConfig :: Display -> GLXFBConfig -> IO XVisualInfo

glXCreateWindow :: Display -> GLXFBConfig -> Window -> [Attribute] -> IO GLXWindow
glXCreateWindow dpy fbconfig win attrs =
    withAttrList attrs $ cglXCreateWindow dpy fbconfig win
foreign import ccall unsafe "glXCreateWindow"
    cglXCreateWindow :: Display -> GLXFBConfig -> Window -> Ptr Int32 -> IO GLXWindow

foreign import ccall unsafe "glXDestroyWindow"
    glXDestroyWindow :: Display -> GLXWindow -> IO ()

glXCreatePixmap :: Display -> GLXFBConfig -> Pixmap -> [Attribute] -> IO GLXPixmap
glXCreatePixmap dpy fbconfig pixmap attrs =
    withAttrList attrs $ cglXCreatePixmap dpy fbconfig pixmap
foreign import ccall unsafe "glXCreatePixmap"
    cglXCreatePixmap :: Display -> GLXFBConfig -> Pixmap -> Ptr Int32 -> IO GLXPixmap

foreign import ccall unsafe "glXDestroyPixmap"
    glXDestroyPixmap :: Display -> GLXPixmap -> IO ()

glXCreatePbuffer :: Display -> GLXFBConfig -> [Attribute] -> IO GLXPbuffer
glXCreatePbuffer dpy fbconfig attrs =
    withAttrList attrs $ cglXCreatePbuffer dpy fbconfig
foreign import ccall unsafe "glXCreatePbuffer"
    cglXCreatePbuffer :: Display -> GLXFBConfig -> Ptr Int32 -> IO GLXPbuffer

foreign import ccall unsafe "glXDestroyPbuffer"
    glXDestroyPbuffer :: Display -> GLXPbuffer -> IO ()

foreign import ccall unsafe "glXQueryDrawable"
    glXQueryDrawable :: Display -> GLXDrawable -> CInt -> Ptr CUInt -> IO ()

glXCreateNewContext :: Display -> GLXFBConfig -> RenderType -> Maybe GLXContext -> Bool -> IO (Maybe GLXContext)
glXCreateNewContext dpy fbc render share direct = do
    ctx@(GLXContext ctxPtr) <- cglXCreateNewContext dpy fbc render (fromMaybe (GLXContext nullPtr) share) direct
    if ctxPtr == nullPtr
        then return Nothing
        else return (Just ctx)
foreign import ccall unsafe "glXCreateNewContext"
    cglXCreateNewContext :: Display -> GLXFBConfig -> RenderType -> GLXContext -> Bool -> IO GLXContext

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

-- TODO: name and organize these properly
visual :: XVisualInfo -> Visual
visual (XVisualInfo xvi) = Visual ((#ptr XVisualInfo, visual) xvi)

depth :: XVisualInfo -> IO Int
depth (XVisualInfo xvi) = (#peek XVisualInfo, depth) xvi

visualId :: XVisualInfo -> IO VisualID
visualId (XVisualInfo xvi) = ((#peek XVisualInfo, visualid) xvi)

withAttrList :: [Attribute] -> (Ptr Int32 -> IO a) -> IO a
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

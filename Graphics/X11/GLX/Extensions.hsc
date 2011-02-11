module Graphics.X11.GLX.Extensions (
      GLXBufferExt
    , glxFrontLeftExt
    , glxFrontRightExt
    , glxBackLeftExt   
    , glxBackRightExt  
    , glxFrontExt      
    , glxBackExt       
    , glxAux0Ext       
    , glxAux1Ext       
    , glxAux2Ext       
    , glxAux3Ext       
    , glxAux4Ext       
    , glxAux5Ext       
    , glxAux6Ext       
    , glxAux7Ext       
    , glxAux8Ext       
    , glxAux9Ext       
    , glXGetProcAddress
    , glXBindTexImageEXT
    , glXReleaseTexImageEXT
    , glxBindToTextureRgbaExt
    ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign
import Foreign.Ptr
import Graphics.X11.Xlib
import Graphics.X11.GLX

#include "GL/glx.h"
#include "GL/glxext.h"




-- texture from pixmap
glxBindToTextureRgbaExt     :: GLXAttribute
glxBindToTextureRgbaExt     = (#const GLX_BIND_TO_TEXTURE_RGBA_EXT)

-- texture buffer ids for BindTexImageEXT
type GLXBufferExt = CInt
glxFrontLeftExt     :: GLXBufferExt
glxFrontLeftExt     = (#const GLX_FRONT_LEFT_EXT)
glxFrontRightExt    :: GLXBufferExt
glxFrontRightExt    = (#const GLX_FRONT_RIGHT_EXT)
glxBackLeftExt      :: GLXBufferExt
glxBackLeftExt      = (#const GLX_BACK_LEFT_EXT)
glxBackRightExt     :: GLXBufferExt
glxBackRightExt     = (#const GLX_BACK_RIGHT_EXT)
glxFrontExt         :: GLXBufferExt
glxFrontExt         = (#const GLX_FRONT_EXT)
glxBackExt          :: GLXBufferExt
glxBackExt          = (#const GLX_BACK_EXT)
glxAux0Ext          :: GLXBufferExt
glxAux0Ext          = (#const GLX_AUX0_EXT)
glxAux1Ext          :: GLXBufferExt
glxAux1Ext          = (#const GLX_AUX1_EXT)
glxAux2Ext          :: GLXBufferExt
glxAux2Ext          = (#const GLX_AUX2_EXT)
glxAux3Ext          :: GLXBufferExt
glxAux3Ext          = (#const GLX_AUX3_EXT)
glxAux4Ext          :: GLXBufferExt
glxAux4Ext          = (#const GLX_AUX4_EXT)
glxAux5Ext          :: GLXBufferExt
glxAux5Ext          = (#const GLX_AUX5_EXT)
glxAux6Ext          :: GLXBufferExt
glxAux6Ext          = (#const GLX_AUX6_EXT)
glxAux7Ext          :: GLXBufferExt
glxAux7Ext          = (#const GLX_AUX7_EXT)
glxAux8Ext          :: GLXBufferExt
glxAux8Ext          = (#const GLX_AUX8_EXT)
glxAux9Ext          :: GLXBufferExt
glxAux9Ext          = (#const GLX_AUX9_EXT)


--glXBindTexImageEXT :: Display -> GLXDrawable -> CInt -> [GLXAttribute] -> IO ()
--glXBindTexImageEXT dpy drawable buffer attrs = 
--    withAttrList attrs $ cglXBindTexImageEXT dpy drawable buffer 
--foreign import ccall unsafe "glXBindTexImageEXT"
--    cglXBindTexImageEXT :: Display -> GLXDrawable -> CInt -> Ptr Int32 -> IO ()
--
--foreign import ccall unsafe "glXReleaseTexImageEXT"
--    glXReleaseTexImageEXT :: Display -> GLXDrawable -> CInt -> IO ()


-- TODO: this shoudl check for null ptr and use Maybe
glXGetProcAddress :: String -> IO (FunPtr a)
glXGetProcAddress name = withCString name cglXGetProcAddress
foreign import ccall unsafe "glXGetProcAddress"
    cglXGetProcAddress :: CString -> IO (FunPtr a)


-- TODO 
--  - add sanity checking so we dont blow up if the extension doesnt exist
--  - determine whether rebinding every time like this is too slow, and if so, optimize somehow

type BindTexImageEXT = Display -> GLXDrawable -> GLXBufferExt -> Ptr Int32 -> IO ()
foreign import ccall "dynamic" 
   mkBindTexImageEXT :: (FunPtr BindTexImageEXT) -> (BindTexImageEXT)
glXBindTexImageEXT :: Display -> GLXDrawable -> GLXBufferExt -> [GLXAttribute] -> IO ()
glXBindTexImageEXT dpy drawable buffer attrs = do
    btiFPtr <- glXGetProcAddress "glXBindTexImageEXT"
    let f = mkBindTexImageEXT btiFPtr
    withAttrList attrs $ f dpy drawable buffer

type ReleaseTexImageEXT = Display -> GLXDrawable -> GLXBufferExt -> IO ()
foreign import ccall "dynamic" 
   mkReleaseTexImageEXT :: (FunPtr ReleaseTexImageEXT) -> (ReleaseTexImageEXT)
glXReleaseTexImageEXT :: Display -> GLXDrawable -> GLXBufferExt -> IO ()
glXReleaseTexImageEXT dpy drawable buffer = do
    btiFPtr <- glXGetProcAddress "glXReleaseTexImageEXT"
    let f = mkReleaseTexImageEXT btiFPtr
    f dpy drawable buffer



-- This is an incomplete binding, patches welcome for binding the rest
-- of the specification, which is available at
-- http://cgit.freedesktop.org/xorg/proto/fixesproto/plain/fixesproto.txt

module Graphics.X11.Xfixes (
    xfixesQueryExtension,
    xfixesQueryVersion,
    Region,
    WindowRegion,
    windowRegionBounding,
    windowRegionClip,
    xfixesCreateRegion,
    xfixesCreateRegionFromBitmap,
    xfixesCreateRegionFromWindow,
    -- xfixesCreateRegionFromGC,
    xfixesDestroyRegion,
    xfixesSetRegion,
    xfixesCopyRegion,
    xfixesUnionRegion,
    xfixesIntersectRegion,
    xfixesSubtractRegion,
    xfixesInvertRegion,
    xfixesTranslateRegion,
    xfixesRegionExtents,
    xfixesFetchRegion,
    xfixesSetWindowShapeRegion,
    -- xfixesSetGCClipRegion,
    xfixesExpandRegion
    ) where

import Foreign
import Foreign.C.Types
import Graphics.X11.Xlib hiding (Region) -- xfixes has its own Region based on XID

#include <X11/Xlib.h>

#include <X11/extensions/Xfixes.h>

-- Shape enum constants
shapeSet, shapeUnion, shapeIntersect, shapeSubtract, shapeInvert :: CInt
shapeSet = 0
shapeUnion = 1
shapeIntersect = 2
shapeSubtract = 3
shapeInvert = 4

shapeBounding, shapeClip, shapeInput :: CInt
shapeBounding = 0
shapeClip = 1
shapeInput = 2

shapeNotify, shapeNumberEvents :: CInt
-- shapeNotifyMask = -- Whatever, I hope I don't need this
shapeNotify = 0
shapeNumberEvents = shapeNotify + 1


xfixesQueryExtension :: Display -> IO (Maybe (CInt, CInt))
xfixesQueryExtension dpy = wrapPtr2 (cXfixesQueryExtension dpy) go
    where go False _ _                = Nothing
          go True eventbase errorbase = Just (fromIntegral eventbase, fromIntegral errorbase)

xfixesQueryVersion :: Display -> IO (Maybe (CInt, CInt))
xfixesQueryVersion dpy = wrapPtr2 (cXfixesQueryVersion dpy) go
    where go False _ _        = Nothing
          go True major minor = Just (fromIntegral major, fromIntegral minor)

foreign import ccall "XFixesQueryExtension"
    cXfixesQueryExtension :: Display -> Ptr CInt -> Ptr CInt -> IO Bool

foreign import ccall "XFixesQueryVersion"
    cXfixesQueryVersion :: Display -> Ptr CInt -> Ptr CInt -> IO Bool


-- Borrowed from the Xdamage bindings
wrapPtr2 :: (Storable a, Storable b) => (Ptr a -> Ptr b -> IO c) -> (c -> a -> b -> d) -> IO d
wrapPtr2 cfun f =
  withPool $ \pool -> do aptr <- pooledMalloc pool
                         bptr <- pooledMalloc pool
                         ret <- cfun aptr bptr
                         a <- peek aptr
                         b <- peek bptr
                         return (f ret a b)


type WindowRegion = CInt

type Region = XID

windowRegionBounding :: WindowRegion
windowRegionBounding = 0
windowRegionClip :: WindowRegion
windowRegionClip = 1


foreign import ccall "XFixesCreateRegion"
    xfixesCreateRegion :: Display -> Ptr Rectangle -> CInt -> IO Region

foreign import ccall "XFixesCreateRegionFromBitmap"
    xfixesCreateRegionFromBitmap :: Display -> Pixmap -> IO Region

foreign import ccall "XFixesCreateRegionFromWindow"
    xfixesCreateRegionFromWindow :: Display -> Window -> WindowRegion -> IO Region

-- Disabled due to build error
-- GC cannot be marshalled in a foreign call
-- because its data constructor is not in scope
-- foreign import ccall "XFixesCreateRegionFromGC"
--     xfixesCreateRegionFromGC :: Display -> GC -> IO Region

-- Disabled due to lack of binding for Picture
--foreign import ccall "XFixesCreateRegionFromPicture"
--    xfixesCreateRegionFromPicture :: Display -> Picture -> IO Region

foreign import ccall "XFixesDestroyRegion"
    xfixesDestroyRegion :: Display -> Region -> IO ()

foreign import ccall "XFixesSetRegion"
    xfixesSetRegion :: Display -> Region -> Ptr Rectangle -> CInt -> IO ()

foreign import ccall "XFixesCopyRegion"
    xfixesCopyRegion :: Display -> Region -> Region -> IO ()

foreign import ccall "XFixesUnionRegion"
    xfixesUnionRegion :: Display -> Region -> Region -> Region -> IO ()

foreign import ccall "XFixesIntersectRegion"
    xfixesIntersectRegion :: Display -> Region -> Region -> Region -> IO ()

foreign import ccall "XFixesSubtractRegion"
    xfixesSubtractRegion :: Display -> Region -> Region -> Region -> IO ()

foreign import ccall "XFixesInvertRegion"
    xfixesInvertRegion :: Display -> Region -> Ptr Rectangle -> Region -> IO ()

foreign import ccall "XFixesTranslateRegion"
    xfixesTranslateRegion :: Display -> Region -> CInt -> CInt -> IO ()

foreign import ccall "XFixesRegionExtents"
    xfixesRegionExtents :: Display -> Region -> Region -> IO ()

foreign import ccall "XFixesFetchRegion"
    xfixesFetchRegion :: Display -> Region -> CInt -> IO ()

-- Disabled due to build error
-- GC cannot be marshalled in a foreign call
-- because its data constructor is not in scope
-- foreign import ccall "XFixesSetGCClipRegion"
--     xfixesSetGCClipRegion :: Display -> GC -> CInt -> CInt -> Region -> IO ()

-- Enabling, since shape is in fact just an CInt enumerated value
foreign import ccall "XFixesSetWindowShapeRegion"
   xfixesSetWindowShapeRegion :: Display -> Window -> CInt -> CInt -> CInt -> Region -> IO ()

-- Disabled due to lack of binding for Picture
--foreign import ccall "XFixesSetPictureClipRegion"
--    xfixesSetPictureClipRegion :: Display -> Picture -> CInt -> CInt -> Region -> IO ()

-- Only available in XFixes version 3 or higher
foreign import ccall "XFixesExpandRegion"
    xfixesExpandRegion :: Display -> Region -> Region -> CInt -> CInt -> CInt -> CInt -> IO ()


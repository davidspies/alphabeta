module ThunkTree.Descend where

import System.IO.Unsafe (unsafePerformIO)
import ThunkTree.Inner (ThunkTreeInner, descendStep)

descend :: ThunkTreeInner a -> ThunkTreeInner a
descend x = unsafePerformIO $ maybe x descend <$> descendStep x

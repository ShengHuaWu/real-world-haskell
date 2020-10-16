{-  The bytestring library provides a fast, cheap alternative to the `String` type. 
    Code written with `ByteString` can often match or exceed the performance and memory footprint of C, 
    while maintaining Haskell's expressivity and conciseness. 
    
    The `Data.ByteString` module defines a strict type named `ByteString`. This represents a string of binary or text data in a single array.
    The `Data.ByteString.Lazy` module provides a lazy type, also named `ByteString`. This represents a string of data as a list of chunks, arrays of up to 64KB in size.
    For streaming a large quantity (hundreds of megabytes to terabytes) of data, the lazy `ByteString` type is usually best. 
    Its chunk size is tuned to be friendly to a modern CPU's L1 cache, and a garbage collector can quickly discard chunks of streamed data that are no longer being used.
    The strict `ByteString` type performs best for applications that are less concerned with memory footprint, or that need to access data randomly. 
    -}

{-  When we want to refer to the lazy `ByteString` module's `take` function, we must write `L.take`, 
    since we imported the module under the name `L`.
-}
import qualified Data.ByteString.Lazy as L
module FileOperations where

import Prelude

import Control.MonadZero
import Data.Maybe
import Data.Path
import Data.Array (concatMap, (:), filter, head)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- Exercise q1
onlyFiles :: Path -> Array Path
onlyFiles = filter (\x -> not (isDirectory x)) <<< allFiles'

-- largest and smallest files in the filesystem
-- Exercise q2


-- search for a file by name
-- Exercise q3
whereIsHelper :: String -> Maybe Path
whereIsHelper str = head $ do 
    p <- allFiles' root
    child <- ls p
    guard $ filename child == str
    pure p


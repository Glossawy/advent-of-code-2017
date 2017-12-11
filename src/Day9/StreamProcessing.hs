module Day9.StreamProcessing where

data StreamState = StreamState { groupDepth :: Int
                                ,garbageFound :: Int
                                ,groupScore :: Int
                                ,isGarbage :: Bool
                                ,isSkip :: Bool } deriving (Show)

initialState :: StreamState
initialState = StreamState 0 0 0 False False

processStream :: StreamState -> String -> StreamState
processStream state [] = state
processStream state@(StreamState gd gf gs garbo skip) (c:cs) = processStream newState cs
  where newState
          | c == '!' || isSkip state = StreamState gd gf gs garbo (not skip)
          | isGarbage state = if c == '>' then StreamState gd gf gs (not garbo) skip else StreamState gd (gf+1) gs garbo skip
          | c == '{'        = StreamState (gd+1) gf gs garbo skip
          | c == '}'        = if gd > 0 then StreamState (gd-1) gf (gs+gd) garbo skip else state
          | c == '<'        = StreamState gd gf gs True skip
          | otherwise       = state

getStreamStats :: String -> StreamState
getStreamStats = processStream initialState

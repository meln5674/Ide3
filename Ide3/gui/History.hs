module History 
    ( History
    , empty
    , singleton
    , null
    , insertBack
    , insertForward
    , replace
    , deleteBack
    , deleteForward
    , deleteAll
    , abandonPast
    , abandonFuture
    , present
    , back
    , forward
    ) where

import Prelude hiding (null)

-- | ADT representing a "history" of values, with a past, present, and future
data History a = History [a] [a]

-- | Create an empty history
empty :: History a
empty = History [] []

-- | Test if a history is empty
null :: History a -> Bool
null (History [] []) = True
null _ = False

-- | Create a history where the present is the beginning and end of time
singleton :: a -> History a
singleton current = History [current] []

-- | Insert a value as the present, pushing the old present into the past
insertBack :: a -> History a -> History a
insertBack new (History past future) = History (new:past) future

-- | Replace the present with a new value, returns Nothing if there is no present
replace :: a -> History a -> Maybe (History a)
replace present' (History (present:past) future) = Just $ History (present':past) future
replace _ _ = Nothing

-- | Insert a value as the present, pushing the old present into the future
insertForward :: a -> History a -> History a
insertForward new (History (current:past) future) = History (new:past) (current:future)
insertForward new (History [] future) = History [new] future

-- | Delete the present, pulling the previous item of the past into the present
-- Returns nothing if there is no present
deleteBack :: History a -> Maybe (History a)
deleteBack (History (present:past) future) = Just $ History past future
deleteBack _ = Nothing

-- | Delete the present, pulling the next item of the future into the present
-- Returns nothing if there is no present
deleteForward :: History a -> Maybe (History a)
deleteForward (History (present:past) (present':future)) = Just $ History (present':past) future
deleteForward (History (present:past) []) = Just $ History past []
deleteForward _ = Nothing

-- | Discard all values in the past, making the present the beginning of time
abandonPast :: History a -> History a
abandonPast (History (current:past) future) = History [current] future
abandonPast x = x

-- | Discard all values in the future, making the present the end of time
abandonFuture :: History a -> History a
abandonFuture (History past _) = History past []

-- | Get the present value if there is one
present :: History a -> Maybe a
present (History (current:past) future) = Just current
present _ = Nothing

-- | Shift back one step into the past, making the present the next item in the future
-- Returns nothing if the present is the beginning of time
back :: History a -> Maybe (History a)
back (History (current:past) future) = Just $ History past (current:future)
back _ = Nothing

-- | Shift forward one step into the future, making the present the previous item in the past
-- Returns nothign if the present in the end of time
forward :: History a -> Maybe (History a)
forward (History past (next:future)) = Just $ History (next:past) future
forward _ = Nothing

-- | Delete all instances of a value from history, if the present is removed, go back in time
deleteAll :: Eq a => a -> History a -> History a
deleteAll x (History past future) = History (filter (/=x) past) (filter (/=x) future)

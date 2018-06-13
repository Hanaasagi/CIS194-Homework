{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}
module LogAnalysis where

import Log

-- exercise 1
parseMessage :: String -> LogMessage
parseMessage str =
  let wordList = words str in
    case wordList of
      ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
      ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
      ("E":level:ts:msg) -> LogMessage (Error (read level)) (read ts) (unwords msg)
      _ -> Unknown (unwords wordList)


parse :: String -> [LogMessage]
parse = (map parseMessage) . lines


-- exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert lmsg Leaf = Node Leaf lmsg Leaf
insert lmsg1@(LogMessage _ ts1 _) (Node left lmsg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left lmsg2 (insert lmsg1 right)
  | otherwise = Node (insert lmsg1 left) lmsg2 right
insert _ tree = tree


-- exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


-- exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lmsg right) = inOrder left ++ [lmsg] ++ inOrder right


-- exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMessage . inOrder . build . filter isRelevant
  where isRelevant(LogMessage (Error n) _ _) = if n > 50 then True else False
        isRelevant _ = False
        extractMessage (LogMessage _ _ msg) = msg
        extractMessage _ = error "should never go here"

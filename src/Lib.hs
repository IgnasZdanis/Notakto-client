module Lib
    ( get, startFirst, encode, parseDict, parseInt, move, Move(..), postData
    ) where

import Network.HTTP
import Network.URI
import Data.Maybe
import Data.BEncode.Parser
import Data.BEncode
import Text.Read

data Move = Move (Int, Int) String String
  deriving Show

instance Eq Move where
 (==) (Move (c1, c2) _ _) (Move (c3, c4) _ _) = (c1 == c3 && c2 == c4)

myId = "Ignas"
initialBoard = ['0','1','2','3','4','5','6','7','8']
bencode = "application/bencode"
-----------------------
getData :: String -> IO String
getData gameName2 =
  let
    url = "http://tictactoe.haskell.lt/game/" ++ gameName2 ++ "/player/1"
    uri = fromJust $ parseURI (url)
    req = Request {
      rqURI = uri,
      rqMethod = GET,
      rqHeaders = [
        mkHeader HdrAccept bencode,
        mkHeader HdrContentLength "0"
      ],
      rqBody = ""
    }
  in simpleHTTP (req) >>= getResponseBody

postData :: String -> String -> IO String
postData board gameName2 = simpleHTTP (postRequestWithBody
   url
   bencode
   board
   ) >>= getResponseBody
  where
    url = "http://tictactoe.haskell.lt/game/" ++ gameName2 ++ "/player/1"

startFirst :: String -> IO ()
startFirst gameName2 = do
  let noMoves = getMoves "de"
  let moves = move noMoves initialBoard
  let coded = encode moves ""
  postResponse <- postData coded gameName2
  playGame gameName2

playGame :: String -> IO ()
playGame gameName2 = do
  getResponse <- getData gameName2
  let moves = getMoves getResponse
  let newBoard = updateBoard initialBoard moves
  let winner = checkWinner initialBoard moves ""
  if (isNothing winner)
    then do
      let newMoves = move moves newBoard
      let coded = encode newMoves ""
      let winner2 = checkWinner initialBoard newMoves ""
      postResponse <- postData coded gameName2
      if (isNothing winner2)
        then playGame gameName2
        else putStrLn "You lost"
    else putStrLn "You won"
  where
  
    
------------------------------
move :: [Move] -> String -> [Move]
move moves board = [Move (c1, c2) myId "x"] ++ moves
  where
    pos = lookForPosition board 0 0
    c1 = mod pos 3
    c2 = quot pos 3

lookForPosition :: String -> Int -> Int -> Int
lookForPosition board 9 lastOk = lastOk
lookForPosition board pos lastOk =
  if board !! pos == 'x'
    then lookForPosition board (pos+1) lastOk
    else if t1 && t2 && t3 && t4 && t5 && t6 && t7 && t8 && t9 && t10 && t11
      then pos
      else lookForPosition board (pos+1) pos
  where
    t1 = if mod pos 3 == 0
      then if board !! (pos+1) == 'x' && board !! (pos+2) == 'x'
        then False
        else True
      else True
    t2 = if mod pos 3 == 1
      then if board !! (pos-1) == 'x' && board !! (pos+1) == 'x'
        then False
        else True
      else True
    t3 = if mod pos 3 == 2
      then if board !! (pos-1) == 'x' && board !! (pos-2) == 'x'
        then False
        else True
      else True
    t4 = if quot pos 3 == 0
      then if board !! (pos+3) == 'x' && board !! (pos+6) == 'x'
        then False
        else True
      else True
    t5 = if quot pos 3 == 1
      then if board !! (pos-3) == 'x' && board !! (pos+3) == 'x'
        then False
        else True
      else True
    t6 = if quot pos 3 == 2
      then if board !! (pos-3) == 'x' && board !! (pos-6) == 'x'
        then False
        else True
      else True
    t7 = if pos == 0
      then if board !! 4 == 'x' && board !! 8 == 'x'
        then False
        else True
      else True
    t8 = if pos == 4
      then if (board !! 0 == 'x' && board !! 8 == 'x') || (board !! 2 == 'x' && board !! 6 == 'x')
        then False
        else True
      else True
    t9 = if pos == 8
      then if board !! 0 == 'x' && board !! 4 == 'x'
        then False
        else True
      else True
    t10 = if pos == 2
      then if board !! 4 == 'x' && board !! 6 == 'x'
        then False
        else True
      else True
    t11 = if pos == 6
      then if board !! 2 == 'x' && board !! 4 == 'x'
        then False
        else True
      else True

updateBoard :: String -> [Move] -> String
updateBoard board [] = board
updateBoard board moves = updateBoard newBoard (init moves)
  where
    (Move (c1, c2) id v) = last moves
    position = c2*3+c1;
    newBoard = take position board ++ "x" ++ drop (position+1) board

getMoves :: String -> [Move]
getMoves msg = moves
  where
    noSpaces = filter(/=' ') msg
    dictParse = parseDict noSpaces
    moves = fst $ fromJust dictParse

checkWinner :: String -> [Move] -> String -> Maybe String
checkWinner _ [] _ = Nothing
checkWinner board moves prevId =
  if threeEq p00 p01 p02 || 
    threeEq p10 p11 p12 || 
    threeEq p20 p21 p22 || 
    threeEq p00 p10 p20 || 
    threeEq p01 p11 p21 || 
    threeEq p02 p12 p22 || 
    threeEq p00 p11 p22 || 
    threeEq p02 p11 p20
    then Just prevId
    else checkWinner newBoard (init moves) id
  where
    (Move (c1, c2) id v) = last moves
    position = c1*3+c2;
    newBoard = take position board ++ "x" ++ drop (position+1) board
    [p00, p01, p02, p10, p11, p12, p20, p21, p22] = newBoard

threeEq :: Char -> Char -> Char -> Bool
threeEq a b c =
  if a == b && a == c
    then True
    else False
--------------------------------------
encode :: [Move] -> String -> String
encode [] str =
  if str == ""
    then "de"
    else str
encode moves str = 
  if str == ""
    then encode 
      (init moves) 
      ("d" ++ (encodeCoords c1 c2) ++ (encodeId id) ++ "1:v1:xe")
    else encode 
      (init moves) 
      ("d" ++ (encodeCoords c1 c2) ++ (encodeId id) ++ "4:prev" ++ str ++ "1:v1:xe")
  where
    (Move (c1, c2) id v) = last moves

encodeCoords :: Int -> Int -> String
encodeCoords c1 c2 = "1:cli" ++ (show c1) ++ "ei" ++ (show c2) ++ "ee"

encodeId :: String -> String
encodeId id = "2:id" ++ (show (length id)) ++ ":" ++ id
  
-----------------------------------------------
parseCoords :: String -> Maybe ((Int, Int), String)
parseCoords ('1':':':'c':'l':rest) =
  if isNothing parse1 || isNothing parse2 || firstChar /= 'e'
    then Nothing
    else Just ((coord1, coord2), tail rest2)
  where
    parse1 = parseInt rest
    (coord1, rest1) = fromJust parse1
    parse2 = parseInt rest1
    (coord2, rest2) = fromJust parse2
    firstChar = head rest2
parseCoords _ = Nothing
parseString :: String -> Maybe (String, String)
parseString msg = 
  if isNothing strLength
    then Nothing
    else Just (str, rest1)
  where
    iAsStr = takeWhile(/= ':') msg
    rest = drop (length iAsStr + 1) msg
    strLength = readMaybe iAsStr
    str = take (fromJust strLength) rest
    rest1 = drop (fromJust strLength) rest

parseId :: String -> Maybe (String, String)
parseId ('2':':':'i':'d':rest) = 
  if isNothing parse
    then Nothing
    else Just (id, rest1)
  where
    parse = parseString rest
    (id, rest1) = fromJust parse

parseInt :: String -> Maybe(Int, String)
parseInt ('i':rest) = 
  if length iAsStr == 1 && (iAsStr == "0" || iAsStr == "1" || iAsStr == "2")
    then Just (read iAsStr, rest1)
    else Nothing
  where
    iAsStr = takeWhile (/= 'e') rest
    strLength = length iAsStr + 1
    rest1 = drop strLength rest
parseInt _ = Nothing

parseX :: String -> Maybe (String, String)
parseX ('1':':':'v':rest) = 
  if isNothing parse || (v /= "x" && v /= "X")
    then Nothing
    else Just (v, rest1)
  where
    parse = parseString rest
    (v, rest1) = fromJust parse
parseX _ = Nothing

parsePrev :: String -> Maybe ([Move], String)
parsePrev ('4':':':'p':'r':'e':'v':rest) = parseDict rest
parsePrev a = Just([], a)

parseDict :: String -> Maybe ([Move], String)
parseDict ('d':'e':rest) = Just([], rest)
parseDict ('d':rest) =
  if isNothing coordsParse ||
    isNothing idParse ||
    isNothing prevParse ||
    isNothing xParse ||
    head rest4 /= 'e'
    then Nothing
    else Just (mov:prevMov, finalRest)
  where
    coordsParse = parseCoords rest
    ((coord1, coord2), rest1) = fromJust coordsParse
    idParse = parseId rest1
    (id, rest2) = fromJust idParse
    prevParse = parsePrev rest2
    (prevMov, rest3) = fromJust prevParse
    xParse = parseX rest3
    (v, rest4) = fromJust xParse
    finalRest = drop 1 rest4
    mov = Move (coord1, coord2) id v
parseDict _ = Nothing
    

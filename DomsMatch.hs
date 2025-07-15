{- 
   DomsMatch: code to play a dominoes match between two players.
   
   The top level function is domsMatch - it takes five arguments:
       games - the number of games to play
       target - the target score to reach
       player1, player2 - two DomsPlayer functions, representing the two players
       seed - an integer to seed the random number generator
   The function returns a pair showing how many games were won by each player.

   The functions of type DomsPlayer must take four arguments:
       The current Hand
       The current Board
       The Player (which will be one of P1 or P2)
       The current Scores
   The function returns a tuple containing the Domino to play and End to play it on.

   Stub with types provided by Emma Norling (October 2023).

   You should add your functions and any additional types that you require to your own copy of
   this file. Before you submit, make sure you update this header documentation to remove these
   instructions and replace them with authorship details and a brief summary of the file contents.

   Similarly, remember you will be assessed not *just* on correctness, but also code style,
   including (but not limited to) sensible naming, good functional decomposition, good layout,
   and good comments.
 -}

module DomsMatch where
    import System.Random
    import Data.List
    import Data.Ord (comparing)
    import Data.Maybe


    -- types used in this module
    type Domino = (Int, Int) -- a single domino
    {- Board data type: either an empty board (InitState) or the current state as represented by
        * the left-most domino (such that in the tuple (x,y), x represents the left-most pips)
        * the right-most domino (such that in the tuple (x,y), y represents the right-most pips)
        * the history of moves in the round so far
     -}
    data Board = InitState | State Domino Domino History deriving (Eq, Show)
    {- History should contain the *full* list of dominos played so far, from leftmost to
       rightmost, together with which player played that move and when they played it
     -}
    type History = [(Domino, Player, MoveNum)]
    type HistoryWithEnd = [(Domino, Player, MoveNum, Maybe End)]
    data Player = P1 | P2 deriving (Eq, Show)
    data End = L | R deriving (Eq, Show)
    type Move = (Domino, End) -- a move that can be played
    type Scores = (Int, Int) -- P1’s score, P2’s score
    type MoveNum = Int
    type Hand = [Domino]
    {- DomsPlayer is a function that given a Hand, Board, Player and Scores will decide
       which domino to play where. The Player information can be used to "remember" which
       moves in the History of the Board were played by self and which by opponent
     -}
    type DomsPlayer = Hand -> Board -> Player -> Scores -> (Domino, End)

    {- domSet: a full set of dominoes, unshuffled -}
    domSet = [ (l,r) | l <- [0..6], r <- [0..l] ]

    {- shuffleDoms: returns a shuffled set of dominoes, given a number generator
       It works by generating a random list of numbers, zipping this list together
       with the ordered set of dominos, sorting the resulting pairs based on the random
       numbers that were generated, then outputting the dominos from the resulting list.
     -}
    shuffleDoms :: StdGen -> [Domino]
    shuffleDoms gen = [ d | (r,d) <- sort (zip (randoms gen :: [Int]) domSet)]

    {- domsMatch: play a match of n games between two players,
        given a seed for the random number generator
       input: number of games to play, number of dominos in hand at start of each game,
              target score for each game, functions to determine the next move for each
              of the players, seed for random number generator
       output: a pair of integers, indicating the number of games won by each player
     -}
    domsMatch :: Int -> Int -> Int -> DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
    domsMatch games handSize target p1 p2 seed
        = domsGames games p1 p2 (mkStdGen seed) (0, 0)
          where
          domsGames 0 _  _  _   wins               = wins
          domsGames n p1 p2 gen (p1_wins, p2_wins)
            = domsGames (n-1) p1 p2 gen2 updatedScore
              where
              updatedScore
                | playGame handSize target p1 p2 (if odd n then P1 else P2) gen1 == P1 = (p1_wins+1,p2_wins)
                | otherwise                                            = (p1_wins, p2_wins+1)
              (gen1, gen2) = split gen
              {- Note: the line above is how you split a single generator to get two generators.
                 Each generator will produce a different set of pseudo-random numbers, but a given
                 seed will always produce the same sets of random numbers.
               -}

    {- playGame: play a single game (where winner is determined by a player reaching
          target exactly) between two players
       input: functions to determine the next move for each of the players, player to have
              first go, random number generator 
       output: the winning player
     -}
    playGame :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> Player
    playGame handSize target p1 p2 firstPlayer gen
        = playGame' p1 p2 firstPlayer gen (0, 0)
          where
          playGame' p1 p2 firstPlayer gen (s1, s2)
            | s1 == target = P1
            | s2 == target = P2
            | otherwise
                = let
                      newScores = playDomsRound handSize target p1 p2 firstPlayer currentG (s1, s2)
                      (currentG, nextG) = split gen
                  in
                  playGame' p1 p2 (if firstPlayer == P1 then P2 else P1) nextG newScores

    {- playDomsRound: given the starting hand size, two dominos players, the player to go first,
        the score at the start of the round, and the random number generator, returns the score at
        the end of the round.
        To complete a round, turns are played until either one player reaches the target or both
        players are blocked.
     -}
    playDomsRound :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> (Int, Int) -> (Int, Int)
    playDomsRound handSize target p1 p2 first gen scores
        = playDomsRound' p1 p2 first (hand1, hand2, InitState, scores)
          where
          -- shuffle the dominoes and generate the initial hands
          shuffled = shuffleDoms gen
          hand1 = take handSize shuffled
          hand2 = take handSize (drop handSize shuffled)
          {- playDomsRound' recursively alternates between each player, keeping track of the game state
             (each player's hand, the board, the scores) until both players are blocked -}
          playDomsRound' p1 p2 turn gameState@(hand1, hand2, board, (score1,score2))
            | (score1 == target) || (score2 == target) || (p1_blocked && p2_blocked) = (score1,score2)
            | turn == P1 && p1_blocked = playDomsRound' p1 p2 P2 gameState
            | turn == P2 && p2_blocked = playDomsRound' p1 p2 P1 gameState
            | turn == P1               = playDomsRound' p1 p2 P2 newGameState
            | otherwise                = playDomsRound' p1 p2 P1 newGameState
              where
              p1_blocked = blocked hand1 board
              p2_blocked = blocked hand2 board
              (domino, end)          -- get next move from appropriate player
                  | turn == P1 = p1 hand1 board turn (score1, score2)
                  | turn == P2 = p2 hand2 board turn (score1, score2)
                                     -- attempt to play this move
              maybeBoard             -- try to play domino at end as returned by the player
                  | turn == P1 && not (elem domino hand1) = Nothing -- can't play a domino you don't have!
                  | turn == P2 && not (elem domino hand2) = Nothing
                  | otherwise = playDom turn domino board end
              newGameState           -- if successful update board state (exit with error otherwise)
                 | maybeBoard == Nothing = error ("Player " ++ show turn ++ " attempted to play an invalid move.")
                 | otherwise             = (newHand1, newHand2, newBoard,
                                              (limitScore score1 newScore1, limitScore score2 newScore2))
              (newHand1, newHand2)   -- remove the domino that was just played
                 | turn == P1 = (hand1\\[domino], hand2)
                 | turn == P2 = (hand1, hand2\\[domino])
              score = scoreBoard newBoard (newHand1 == [] || newHand2 == [])
              (newScore1, newScore2) -- work out updated scores
                 | turn == P1 = (score1+score,score2)
                 | otherwise  = (score1,score2+score)
              limitScore old new     -- make sure new score doesn't exceed target
                 | new > target = old
                 | otherwise    = new
              Just newBoard = maybeBoard -- extract the new board from the Maybe type
              
    {- domInHand: check if a particular domino is contained within a hand -}
    domInHand :: Domino -> Hand -> Bool
    domInHand (l,r) hand = [ 1 | (dl, dr) <- hand, (dl == l && dr == r) || (dr == l && dl == r) ] /= []

    {- scoreBoard: given the board state will calulate the current score 
       input: Board state and bool for if its the last domino dropped
       output: the score
      -}
    scoreBoard :: Board -> Bool -> Int
    scoreBoard InitState _ = 0
    scoreBoard (State lDomino rDomino history) lastDomino
      | lastDomino = 1 + playerScore -- adds one if its the last domino in that players hand
      | otherwise = playerScore
      where
        playerScore = calcFivesAndThrees (calcPipTotal lDomino rDomino history) -- using scoring for FIVES-AND-THREES

    {- calcPipTotal: calculates the pip value baised on if its the first domino dropped 
       input: left and right dominos and the history
       output: calulated pip score 
      -}
    calcPipTotal :: Domino -> Domino -> History -> Int
    calcPipTotal lDomino rDomino history
      | length history == 1 = uncurry (+) lDomino
      | otherwise = calcPipValue lDomino L + calcPipValue rDomino R

    {- calcPipValue: takes one domino and send back the value needed to be added baised on
          if the dominod is a double or note
       input: domino and the end
       output: pip count depending on if its a double and which end its going-}
    calcPipValue :: Domino -> End -> Int
    calcPipValue (left, right) end
      | left == right = left + right -- if the domino is a double 
      | end == L = left
      | otherwise = right

    {- calcFivesAndThrees: finds if the value is a multiple of 5 or 3 or both
       input: pipcount
       output: score-}
    calcFivesAndThrees :: Int -> Int
    calcFivesAndThrees value
      | value `mod` 5 == 0 && value `mod` 3 == 0 = value `div` 5 + value `div` 3
      | value `mod` 5 == 0 = value `div` 5
      | value `mod` 3 == 0 = value `div` 3
      | otherwise = 0

    {- blocked: checks if a player has a valid move to make 
       input: hand and board state
       output: true if they can make a play or false if not-}
    blocked :: Hand -> Board -> Bool
    blocked _ InitState = False
    blocked [] _ = True
    blocked (domino:restHand) board
      | canPlay domino L board || canPlay domino R board = False --checks if it can be played on either side of the line
      | otherwise = blocked restHand board -- moving through the hand

    {- canPlay: checks if a domino can be played at a certain end of a board
       input: domino wanting to be played
       output: True if domino can be played and False if not-}
    canPlay :: Domino -> End -> Board -> Bool
    canPlay _ _ InitState = True
    canPlay (side1, side2) end (State lDomino rDomino _)
      | end == L && (side1 == fst lDomino || side2 == fst lDomino) = True --checks against left of the left domino
      | end == R && (side1 == snd rDomino || side2 == snd rDomino) = True --checks against right of the right domino
      | otherwise = False

    {- playDom: plays a domino by taking a player, domino, board and the end
          checks if its possible for the domino to be played and if it can it will return the new board with the added 
          domnino to it or it will return nothing
       input: the player, domino to be played, current board state and the end 
       output: trys to place the domino on the board at that end if it cant then returns Nothing
      -}
    playDom :: Player -> Domino -> Board -> End -> Maybe Board
    playDom player domino board end
      | canPlay domino end board = Just (addDominoToEnd player domino board end)
      | otherwise = Nothing

    {- addDominoToEnd: adds the domino to the correct position in history and adds it to the board too 
       input: player, domino, board and the end
       output: the board with the domino placed to the correct end of the board
       -}
    addDominoToEnd :: Player -> Domino -> Board -> End -> Board
    addDominoToEnd player domino InitState _ = State domino domino [(domino, player, 1)]
    addDominoToEnd player domino (State lDomino rDomino history) end
      -- adds to the left or right with the correct rotation 
      | end == L =
          State (rotateDomino domino lDomino end) rDomino ((rotateDomino domino lDomino end, player, calcCurrentMove history):history)
      | otherwise =
          State lDomino (rotateDomino domino rDomino end) (history ++ [(rotateDomino domino rDomino end, player, calcCurrentMove history)])

    {- rotateDomino: takes in 2 dominos and checks if the first domino is rotated correctly againsed the second domino 
          depending on which end it was on 
       input: domino to be reversed, domino to reverse it against and to which end
       output: domino
       -}
    rotateDomino :: Domino -> Domino -> End -> Domino
    rotateDomino (x1, x2) (y1, y2) end
      | end == L && x2 /= y1 = (x2, x1)
      | end == R && x1 /= y2 = (x2, x1)
      | otherwise = (x1, x2)

    {- calcCurrentMove: figures out the current move to be played 
       input: History 
       output: next move to be played
      -}
    calcCurrentMove :: History -> Int
    calcCurrentMove [] = 1
    calcCurrentMove history = findLargestNum [x | (_, _, x) <- history] + 1

    {- findLargestNum: finds the largest number in a list (used to find the last moved played in history
       input: any type that is appart of the Ord class
       output: largest 
      -}
    findLargestNum :: (Ord a) => [a] -> a
    findLargestNum [] = error "Cannot find largest element"
    findLargestNum [x] = x
    findLargestNum (x:xs) = max x (findLargestNum xs)

    {- simplePlayer: returns the first valid move that is possible with their hand-}
    simplePlayer :: DomsPlayer
    simplePlayer hand board _ _ =
      head (dominosCanPlay hand board)

    smartPlayer :: DomsPlayer
    smartPlayer hand InitState _ _
      -- Strategy Moves if we have first drop
      | (5,4) `elem` hand = ((5,4), L) -- Plays (5,4) domino if we have it in hand
      | (4,5) `elem` hand = ((4,5), L)
      | otherwise = highestScoreMove --Else Plays highest scoring move
      where
        possibleMovesWithScores = map (addScores InitState) (dominosCanPlay hand InitState)
        highestScoreMove = getHighestPointMove possibleMovesWithScores hand

    smartPlayer hand (State doml domr history) player (p1Score, p2Score)
      | isJust winningMove = makeValidMove (fromJust winningMove) hand -- Plays winning move if possible
      | not (null blockOpponmentMoves) = makeValidMove (head blockOpponmentMoves) hand -- Plays blocking move if possible
      | otherwise = highestPointMove --Else Plays highest scoring move
      where
        -- Gavering data about the game
        neededPoints = 61 - (if player == P1 then p1Score else p2Score) -- calculate points needed to win
        opp = if player == P1 then P2 else P1 -- find the opponment 
        oppsKnockedPips = listPipsOpponentDoesntHave history opp -- get pips that opponment doesn't have
        couldPlayRotated = rotateDominoList (dominosCanPlay hand (State doml domr history)) doml domr -- get possible plays and rotating them accordenly 
        couldPlayWithScores = map (addScores (State doml domr history)) couldPlayRotated -- appending the score that each play would give the player

        -- Strategy Moves
        winningMove = findWinningMove couldPlayWithScores neededPoints -- see if we have a move to win the game
        blockOpponmentMoves = [ x | x <- couldPlayRotated, doesMoveBlockOpp x (State doml domr history) oppsKnockedPips] -- see if we can block the opponment
        highestPointMove = getHighestPointMove couldPlayWithScores hand -- get the highest scoring move


    {- dominosCanPlay: takes the hand and a Board and returns all the dominos that the player can play
       input: hand and the board
       output: all the dominos that the player can play
    -}
    dominosCanPlay :: Hand -> Board -> [Move]
    dominosCanPlay hand board = [(domino, end) | domino <- hand, end <- [L, R], canPlay domino end board || canPlay domino end board]

    {- rotateDominoList: takes a list of dominos valid dominos with the end they will be played 
          left and right domino then rotates the dominos accrodingly 
      input: list of moves and the left and right dominos
      output: list of moves but with the dominos in correct orientation
      -}
    rotateDominoList :: [Move] -> Domino -> Domino -> [Move]
    rotateDominoList [] _ _ = []
    rotateDominoList ((domino, end):dominos) ldomino rdomino
      | end == L = (rotateDomino domino ldomino end, end):rotateDominoList dominos ldomino rdomino
      | otherwise = (rotateDomino domino rdomino end,end):rotateDominoList dominos ldomino rdomino

    {- addScores: takes a board and a move then calulates the points that move will get and returns the move with the points 
       input: board and a move
       output: the move with the points that the move will get
      -}
    addScores :: Board -> Move -> (Move, Int)
    addScores InitState (domino, end) = ((domino, end), calcFivesAndThrees (uncurry (+) domino))
    addScores (State doml domr _) (domino, end) = ((domino, end), score)
      where
        score = calcFivesAndThrees (calcPipTotal (if end == L then domino else doml) (if end == R then domino else domr) [])

    {- unRotate: takes a domino and flips it 
       input: a move 
       output: domino flipped 
       -}
    unRotate :: Move -> Move
    unRotate ((leftPip, rightPip), end) = ((rightPip, leftPip), end)

    {- makeValidMove: checks if the domino is in the hand if not then unrotates it 
       intput: move and hand
       output: a move thats in the hand 
      -}
    makeValidMove :: Move -> Hand -> Move
    makeValidMove move hand = if fst move `elem` hand then move else unRotate move

    {- getHighestPointMove: finds the move with the highest points and sends it back with the correct rotaion 
       input: list of moves with their scores and a hand
       outputs: the highest validated move
    -}
    getHighestPointMove :: [(Move, Int)] -> Hand -> Move
    getHighestPointMove moves hand = validMove
      where
        validMove = makeValidMove (fst $ foldl maxPoints (((0,0),L), -1) moves) hand

    {- maxPoints: 2 moves with a scores and compars for the highest score
       inputs: 2 moves with a scores
       outputs: move with highest score
      -}
    maxPoints :: (Move,Int) -> (Move,Int) -> (Move,Int)
    maxPoints (dom1, score1) (dom2, score2) = if score1 > score2 then (dom1, score1) else (dom2, score2)

    {- findWinningMove: checks if the list of moves contains a winning move 
       input: a list of moves with their scores and the needed points to win
       output: wither a winning move or nothing
    -}
    findWinningMove :: [(Move,Int)] -> Int -> Maybe Move
    findWinningMove [] _ = Nothing
    findWinningMove (((domino, end), domScore):rest) neededPoints
      | neededPoints == domScore = Just (domino, end)
      | otherwise = findWinningMove rest neededPoints

    {- doesMoveBlockOpp: takes a valid move and a pips that the opponent cant play and checks if the move will block the opponement
       input: move, board, pips opponent doesn't have
       output: true if the move will block the opponent and false if not
      -}
    doesMoveBlockOpp :: Move -> Board -> [Int] -> Bool
    doesMoveBlockOpp _ InitState _ = False
    doesMoveBlockOpp (domino, end) (State lDomino rDomino _) knockPips
      | end == L = fst domino `elem` knockPips && snd rDomino `elem` knockPips -- Checks if both sides block the opponent 
      | otherwise = fst lDomino `elem` knockPips && snd domino `elem` knockPips


    {- listPipsOpponentDoesntHave: takes the history and opponment then returns the list of pips they where blocked at
       input: History, opponment
       output: List of pips that opponment can't play
      -}
    listPipsOpponentDoesntHave :: History -> Player -> [Int]
    listPipsOpponentDoesntHave history opponent = nub knockedPips -- remove duplicates
      where
        sortedHistory = sortHistoryWithEnd (toHistoryWithEnd history (Just L)) -- converts history to HistoryWithEnd then sorts it
        (_, startingPlayer, _, _) = head sortedHistory -- Gets the starting player
        (_, endingPlayer, _, _) = last sortedHistory
        isOppCurrentlyBlocked = endingPlayer /= opponent 
        -- gets the current blocked pip if opp is currently blocked
        currentBlockedPips = if isOppCurrentlyBlocked then getEndPips (toHistoryWithEnd history (Just L)) else [] 
        knockedPips = currentBlockedPips ++ oppsKnockedPipsList startingPlayer opponent sortedHistory [] -- works out the pips the opponment knocked at

    {- oppsKnockedPipsList: with the sorted history startingPlayer and the opponment it will re make the history and creating 
          the list of pips that the opponment blocked on
       input: startingPlayer, opponment, a sorted HistoryWithEnd and an [] that is type HistoryWithEnd
       output: a list of pips that the opponment doesn't have in their hand
     -}
    oppsKnockedPipsList :: Player -> Player -> HistoryWithEnd -> HistoryWithEnd -> [Int]
    oppsKnockedPipsList _ _ [] output = []
    oppsKnockedPipsList P1 opp (turn:history) [] = oppsKnockedPipsList P2 opp history [turn] -- adds the first move and swaps the player
    oppsKnockedPipsList P2 opp (turn:history) [] = oppsKnockedPipsList P1 opp history [turn]
    oppsKnockedPipsList player opp (turn:history) output
      --if the player /= playersTurn and player is the opponment then add the end pips to the output
      | player /= playersTurn && player == opp = getEndPips output ++ oppsKnockedPipsList player opp history (turn:output) 
      -- recalls oppsKnockedPipsList adding the history to the left or the right
      | end == Just L = oppsKnockedPipsList nextPlayer opp history (turn:output)
      | otherwise = oppsKnockedPipsList nextPlayer opp history (output ++ [turn])
      where
        (_, playersTurn, _, end) = turn -- gets the players turn it should be
        nextPlayer -- gets the next players to send into the next call
          | player /= playersTurn && player /= opp = player
          | player == P1 = P2
          | otherwise = P1

    {- toHistoryWithEnd: converts History to a HistoryWithEnd type the end is either Just L, Just R, or Nothing (for the first move)
       input: a History and Just L because we start on the Left
       output: HistoryWithEnd
    -}
    toHistoryWithEnd :: History -> Maybe End -> HistoryWithEnd
    toHistoryWithEnd [] _ = []
    toHistoryWithEnd ((domino, player, 1):history) _ =
                        (domino, player, 1, Nothing):toHistoryWithEnd history (Just R)
    toHistoryWithEnd ((domino, player, moveNum):history) (Just L) =
                        (domino, player, moveNum, Just L):toHistoryWithEnd history (Just L)
    toHistoryWithEnd ((domino, player, moveNum):history) (Just R) =
                        (domino, player, moveNum, Just R):toHistoryWithEnd history (Just R)

    {- toHistory: converts HistoryWithEnd back to History type
       input: HistoryWithEnd
       output: History
    -}
    toHistory :: HistoryWithEnd -> History
    toHistory [] = []
    toHistory ((domino, player, moveNum, _):restHistory) = (domino, player, moveNum):toHistory restHistory

    {- getEndPips: gets HistoryWithEnds and returns the 2 exposing pips on the end 
       input: HistoryWithEnd
       output: both the exposed pips
    -}
    getEndPips :: HistoryWithEnd -> [Int]
    getEndPips historyWE = [leftPip,rightPip]
      where
        ((leftPip,_), _, _, _) = head historyWE
        ((_,rightPip), _, _, _) = last historyWE

    {- sortHistoryWithEnd: sorts a HistoryWithEnd list baised on the moveNum using merge sort
       input: HistoryWithEnd
       output: sorted HistoryWithEnd
    -}
    sortHistoryWithEnd :: HistoryWithEnd -> HistoryWithEnd
    sortHistoryWithEnd [] = []
    sortHistoryWithEnd [x] = [x]
    sortHistoryWithEnd list
        = merge (sortHistoryWithEnd xs) (sortHistoryWithEnd ys) -- merges the split list when the lists become [x]
            where (xs,ys) = splitList list

    {- splitList: takes the list and splits it down the middle
       input: whole list
       output: split list
    -}
    splitList :: HistoryWithEnd -> (HistoryWithEnd, HistoryWithEnd)
    splitList xs = splitAt mid xs
      where mid = length xs `div` 2

    {- merge: takes 2 of the HistoryWithEnd split lists and merges them back togther 
            sorts them baised on MoveNum (x,y) smallest to largest
       input: 2 HistoryWithEnd
       output: sorted list
    -}
    merge :: HistoryWithEnd -> HistoryWithEnd -> HistoryWithEnd
    merge [] ys = ys
    merge xs [] = xs
    merge ((a,b,x,c):xs) ((d,e,y,f):ys)
      | x <= y = (a,b,x,c):merge xs ((d,e,y,f):ys)
      | otherwise = (d,e,y,f):merge ((a,b,x,c):xs) ys



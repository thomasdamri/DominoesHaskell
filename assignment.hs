{- 
   Assignment: code to create two domino players
   
   The two top level player functions are attackPlayer and defensivePlayer
   They both take a DomsPlayer for the argument

   Code by Thomas Damri
 -}
module Assignment where
    import DomsMatch
    import Data.Char
    import Data.List
    import System.Random
    import Debug.Trace
    import Data.Maybe
    import Data.Ord (comparing)
    
    --Gets the first item in a triple tuple
    fstTriple :: (a,b,c) -> a
    fstTriple (a,_,_) = a
    
    --Gets the second item in a triple tuple
    middleTriple :: (a,b,c) -> b
    middleTriple (_,b,_) = b
    
    --Function to take a domino board and return all the played dominoes using the boards history
    history :: DominoBoard -> [Domino]
    history InitBoard = domSet --If InitBoard, return the whole set
    --Recurse over the history adding the domino to a list
    history (Board leftDomino rightDomino (move1:[])) = [fstTriple(move1)] 
    history (Board leftDomino rightDomino (move1:rest)) = fstTriple(move1) : history (Board leftDomino rightDomino (rest))
    
    --Function which calculates all possible dominoes opponent can have. This function does't take into account where player has knocked, another function will use those both to calculate the answer
    opponentPossibles :: DominoBoard -> Hand -> [Domino]
    --Removes the players hand and history from the domset. Will also flip the dominos round to remove them if the dots are in the wrong order, but its the same domino
    opponentPossibles InitBoard hand = domSet \\ hand
    opponentPossibles board hand = (((domSet\\domHistory)\\swapPlaces domHistory)\\hand)\\(swapPlaces hand)
                                 where
                                 domHistory = history board
    
    --Function which takes a list of dominoes and swaps around first and second int
    swapPlaces :: [Domino] -> [Domino]
    swapPlaces [] = []
    swapPlaces ((x1,x2):rest) = (x2,x1) : (swapPlaces rest)
    
    --Function to calculate whether a player is one move away from winning
    oneMoveAway :: Player -> Scores -> Bool
    oneMoveAway P1 (score,_) = if (score >= 57) then True else False
    oneMoveAway P2 (_,score) = if (score >= 57) then True else False
   
    --Function to return the correct score of a player
    getPlayerScore :: Player -> Scores -> Int
    getPlayerScore P1 (score,_) = score
    getPlayerScore P2 (_,score) = score
    
    --Function to return the other player
    getOtherPlayer :: Player -> Player
    getOtherPlayer P1 = P2
    getOtherPlayer P2 = P1
    
    --Returns the number dominos left of a certain number dots. Can be used to count certain number in your own hand. Can also be used to count possible occurences in opponents hand
    countNumberLeft :: Int -> [Domino] -> Int
    countNumberLeft x leftDominos = length ([d | d@(x1,x2) <- leftDominos, x1==x || x2==x]) 
    
    --Takes a board and a player and will return a list of dominos which causes the player to knock
    checkWhereKnocked :: DominoBoard -> Player -> [Domino]
    checkWhereKnocked InitBoard player = []
    checkWhereKnocked board player = checkWhereKnocked' board player []
                                     where
                                     checkWhereKnocked' (Board leftDomino rightDomino (move1:[])) player knockedList = knockedList
                                     checkWhereKnocked' (Board leftDomino rightDomino (move1:move2:rest)) player knockedList
                                         | playerKnocked && correctPlayer = checkWhereKnocked' newBoard player newList
                                         | otherwise = checkWhereKnocked' newBoard player knockedList
                                                       where
                                                       --Gets the player from the history and sees if they are the same, meaning the other player knocked
                                                       dominoKnock = fstTriple(move1)
                                                       newDomino = (snd(dominoKnock),snd(dominoKnock))
                                                       playerKnocked = middleTriple(move1)==middleTriple(move2) 
                                                       correctPlayer = middleTriple(move1)==player --If the player is the same, the other player knocked
                                                       newBoard = Board leftDomino rightDomino (move2:rest)  
                                                       newList = newDomino:knockedList
                                                       
    

    --Function to deduct all dominos which caused player to knock from the possible domino list. Takes a list of dominos which caused a knock, then the list from opponentPossibles and returns the new list                                                                    
    opponentPossiblesMinusKnocks :: [Domino] -> [Domino] -> [Domino]                                                                
    opponentPossiblesMinusKnocks [] opponentPossibles = opponentPossibles
    opponentPossiblesMinusKnocks (x:rest) opponentPossibles = opponentPossiblesMinusKnocks rest (removeDotsFromList x opponentPossibles)

                                                                    
    
    --Takes a domino which caused the player to knock and a list of dominos. Then returns the list with all occurences of the dots which caused a knock removed                                                                
    removeDotsFromList :: Domino -> [Domino] -> [Domino]    
    removeDotsFromList knockDomino dominoList = [d | d <- dominoList, (containsTheseDots knockDomino d)]
    
    --Takes a domino which caused a knock and another domino which is possibly in opponents hand. Then if those dots are on the domino return false
    containsTheseDots :: Domino -> Domino -> Bool
    containsTheseDots (x1,x2) (y1,y2)
        | y1 == x1 || y1 == x2 = False
        | y2 == x1 || y2 == x2 = False
        | otherwise = True
                                                      
    
    --Takes a hand, a board and a target score. Returns list of possible plays
    targetScore :: Hand -> DominoBoard -> Int -> [(Domino,End)]
    targetScore hand board target = [(d,y) | d <- hand, (d2,y) <- scoreN board target, equalDomino d d2]
    
    --Checks to see if two dominos are equal to eachother
    equalDomino :: Domino -> Domino -> Bool
    equalDomino (x1,x2) (y1,y2)
        | x1 == y1 && x2 == y2 = True --Need to check both combinations, as some dots could be flipped
        | x1 == y2 && x2 == y1 = True
        | otherwise = False
    
    --Creates a tuple of score for a left play and score for a right play
    getScoreFromPlay :: Domino -> DominoBoard -> (Int,Int)
    getScoreFromPlay domino board = (getScoreFromPlayEnd domino board L, getScoreFromPlayEnd domino board R)
    
    
    --Takes a domino, board and an end and returns the score which would be generated                                              
    getScoreFromPlayEnd :: Domino -> DominoBoard -> End -> Int
    getScoreFromPlayEnd domino board end
        | canPlay domino end board == False = 0
        | otherwise = scoreBoard newBoard
                      where
                      Just newBoard = playDom P1 domino board end
                      
    --Used to check if a list contains a certain domino. Can be used to check if an opponent could have a certain domino. Use it after generating list of opponent possibilities                  
    includesDomino :: [Domino] -> Domino -> Bool
    includesDomino [] domino = False
    includesDomino (d:rest) domino
        | equalDomino d domino = True
        | otherwise = includesDomino rest domino
        


    --Takes the possible dominos in opponents hand, then goes through 0-9, seeing count of dots and order the outcome, Use opponentPossibleMinusKnock to get the possibleDoms list                                           
    countDotsInList :: [Domino] -> [(Int,Int)]                                         
    countDotsInList possibleDoms = sortBy (comparing (\(x,y) -> abs y)) [(n,countNumberLeft n possibleDoms) | n <- [0..6]]
    
                              
    --Loop through the ordered likely dots list(got from using countDotsInList). If the count is between 0 and 2, it will see if a domino can be placed and if so how                          
    loopOrderedDotsToBlock :: Hand -> [(Int,Int)] -> DominoBoard -> [(Bool, Maybe (Domino, End),Int)]    
    loopOrderedDotsToBlock hand [] _= [(False,Nothing,10)]
    loopOrderedDotsToBlock hand ((dot,count):rest) board 
    --If the dot count is inbetween 0 and 2, then try place domino
        | count == 0 || count == 1 || count == 2 = (True,loopHandToBlock hand dot board,count) : loopOrderedDotsToBlock hand rest board 
        | otherwise = loopOrderedDotsToBlock hand rest board 

    --Takes a hand, dots needed to minimise opponents chances, domino board, player and returns a domino which can be placed and the end to place it.     
    loopHandToBlock :: Hand -> Int -> DominoBoard -> Maybe (Domino, End)
    loopHandToBlock [] _ _= Nothing
    loopHandToBlock (domino@(x1,x2):rest) dots board
        | x1 == dots && playDomLeft/=Nothing && (correctDotSideUsed domino dots board L 0)= Just (domino, L)
        | x2 == dots && playDomLeft/=Nothing && (correctDotSideUsed domino dots board L 1)= Just (domino, L)
        | x1 == dots && playDomRight/=Nothing && (correctDotSideUsed domino dots board R 0)= Just (domino, R)
        | x2 == dots && playDomRight/=Nothing && (correctDotSideUsed domino dots board R 1)= Just (domino, R)
        | otherwise = loopHandToBlock rest dots board
            where
            playDomLeft = playDom P1 domino board L
            playDomRight = playDom P1 domino board R
    
    {-Takes a domino, the dots needed, a board, which end it wants to be played and an int to specify which side of the domino 
    is equal to the dots needed (0 if left, 1 if right). It will then return bool whether the correct side is being used to
    block opponent  -}
    correctDotSideUsed :: Domino -> Int -> DominoBoard -> End -> Int -> Bool 
    correctDotSideUsed domino dots InitBoard end _ = True
    correctDotSideUsed (x1,x2) dots (Board (l,_) (_,r) _) end whichSide
        | end == L && whichSide==0 && (x2==l && x1==dots) = True
        | end == R && whichSide==1 && (x1==r && x2==dots) = True
        | end == L && whichSide==1 && (x1==l && x2==dots) = True
        | end == R && whichSide==0 && (x2==r && x1==dots) = True
        | otherwise = False
    
    {- Function which takes hand, board, player and a score which player wants to get.
    Will loop through all of the dominos and check whether the domino can be placed on the left or right side of the board
    Will take into account how many there are possible left for the opponent, only playing if its unlikely for the opponent
    Will return the first possible play to get that score -}
    checkScorePossible :: Hand -> DominoBoard -> Int -> Player -> Maybe(Domino,End)
    --If no plays possible, return Nothing
    checkScorePossible hand InitBoard _ _= Nothing
    checkScorePossible [] board _ _= Nothing
    checkScorePossible h@(d@(x1,x2):rest) board@(Board (l,_) (_,r) _) score player
        --If it is a double domino, then you don't need to check if the correct dot side is used
        | x1 == x2 && ((x1==l) || (x2==l)) && (x1+x2+r)==score && numberLeftForOpponentLeft<5 = Just(d,L)
        | x1 == x2 && ((x1==r) || (x2==r)) && (x1+x2+l)==score && numberLeftForOpponentLeft<5= Just(d,R)
        |playDomLeft/=Nothing && scoreBoard(fromJust playDomLeft)==score &&(correctDotSideUsed d x1 board L 0) && numberLeftForOpponentLeft<5= Just (d, L)
        |playDomLeft/=Nothing && scoreBoard(fromJust playDomLeft)==score && (correctDotSideUsed d x2 board L 1) && numberLeftForOpponentRight<5= Just (d, L)
        |playDomRight/=Nothing && scoreBoard(fromJust playDomRight)==score && (correctDotSideUsed d x1 board R 0) && numberLeftForOpponentLeft<5= Just (d, R)
        |playDomRight/=Nothing && scoreBoard(fromJust playDomRight)==score && (correctDotSideUsed d x2 board R 1) && numberLeftForOpponentRight<5= Just (d, R)
        | otherwise = checkScorePossible rest board score player
            where
            playDomLeft = playDom P1 d board L
            playDomRight = playDom P1 d board R
            --Gets the int of the amound of dominos of that dot type left 
            numberLeftForOpponentLeft = countNumberLeft x1 (opponentPossiblesMinusKnocks (checkWhereKnocked board player) (opponentPossibles board h))
            numberLeftForOpponentRight = countNumberLeft x2 (opponentPossiblesMinusKnocks (checkWhereKnocked board player) (opponentPossibles board h))
            

    {- Function which takes a hand and a board and will return a list of dominos which are likely to cause the opponent to knock
    Will return a bool value, stating whether the play is possible, then the play and then an integer score
    Keeps a counter of which domino its checking, so that it can iterate through the dominos and the dot count list in one function
    -}
    playUnlikelyDom :: Hand -> DominoBoard -> [(Bool, Maybe(Domino,End),Int)]
    playUnlikelyDom hand board = playUnlikelyDom' hand (reverse(countDotsInList(hand :: [Domino]))) 0 (length hand) hand board []
                           where
                           --End of dominos, return the list of plays
                           playUnlikelyDom' hand [] count handSize fullHand board playList = playList
                           playUnlikelyDom' [] dotslist count handSize fullHand board playList = playList
                           playUnlikelyDom' list@(d@(x1,x2):rest) dotList@(dc:restDc) count handSize fullHand board playList
                               --If a play is possible, add it to the new list
                               | haveFourOrMore && playDomLeft/=Nothing && (correctDotSideUsed d x1 board L 0)= playUnlikelyDom' rest dotList (count+1) handSize fullHand board newListLeft
                               | haveFourOrMore && playDomLeft/=Nothing && (correctDotSideUsed d x2 board L 1)= playUnlikelyDom' rest dotList (count+1) handSize fullHand board newListLeft
                               | haveFourOrMore && playDomRight/=Nothing && (correctDotSideUsed d x1 board R 0)= playUnlikelyDom' rest dotList (count+1) handSize fullHand board newListRight
                               | haveFourOrMore && playDomRight/=Nothing && (correctDotSideUsed d x2 board R 1)= playUnlikelyDom' rest dotList (count+1) handSize fullHand board newListRight
                               --If all count has exceeded hand size, then move onto next dot count, and return to full domino hand
                               | count>=handSize = playUnlikelyDom' fullHand restDc 0 (length(hand)) fullHand board playList
                               | otherwise = playUnlikelyDom' rest dotList (count+1) handSize fullHand board playList
                                   where
                                   haveFourOrMore = snd(dc)>=4
                                   newListLeft = (True,Just (d, L),scoreBoard(fromJust playDomLeft)) : playList
                                   newListRight = (True,Just (d, R),scoreBoard(fromJust playDomRight)) : playList
                                   playDomLeft = playDom P1 d board L
                                   playDomRight = playDom P1 d board R
    
    
    --Used to get the highest scoring play from the list of possible plays generated form playUnlikelyDom
    getHighestScoredSafePlay :: [(Bool, Maybe(Domino,End),Int)] -> Maybe (Domino,End)
    getHighestScoredSafePlay possiblePlays 
        --Gets the head of the list of plays, as it is the highest scoring
        | (length playList)/=0 = Just (fst(head(playList)))
        | otherwise = Nothing
            where
            --Sort the list, so that highest scored plays are first
            playList = reverse(sortBy (comparing (\(x,y) -> abs y)) [(fromJust play,score) | (bool,play,score) <- possiblePlays])
    
    --Used with loopOrderedDotsToBlock, gets the domino play with the highest chance of a block
    getMostLikelyBlock :: [(Bool, Maybe (Domino, End),Int)] -> Maybe (Domino,End)
    getMostLikelyBlock possiblePlays
        --Gets the head of the list, as its the most likely to block
        | (length playList)/=0 = Just (fst(head(playList)))
        | otherwise = Nothing
            where
            --Sort the list so that most likely to block plays are first
            playList = sortBy(comparing (\(x,y) -> abs y)) [(fromJust play,likelyhood) | (bool,play,likelyhood) <- possiblePlays, play/=Nothing]
    
    --Used get the highest possible scored play, not taking into account blocking opponent. Uses hand and domino board        
    getHighestScoreNormalPlay :: Hand -> DominoBoard -> Maybe (Domino,End)
    getHighestScoreNormalPlay hand board 
        | (length playList)/=0 = Just (fst(head(playList)))
        | otherwise = Nothing
            where
            --Sort the list, so highest scoring plays are first
            playList = reverse(sortBy (comparing (\(x,y) -> abs y)) [(play,score) | (play,score) <- (convertAllPossPlaysToList hand board)])

    --Function which takes a hand and a board and returns a list of all possible plays and their scores
    convertAllPossPlaysToList :: Hand -> DominoBoard -> [((Domino,End),Int)]
    convertAllPossPlaysToList hand board = convertAllPossPlaysToList' (possPlays hand board) [] --Uses possPlays to get a list of plays
                                            where
                                            convertAllPossPlaysToList' ([],[]) plays = plays
                                            --If the right plays list is empty, only iterate through left list
                                            convertAllPossPlaysToList' ((leftD:restL),[]) plays = convertAllPossPlaysToList' (restL,[])  ((((leftD,L),getScoreFromPlayEnd leftD board L)) :plays)
                                            --If the left plays list is empty, only iterate through right list
                                            convertAllPossPlaysToList' ([],(rightD:restR)) plays = convertAllPossPlaysToList' ([],restR)  ((((rightD,R),getScoreFromPlayEnd rightD board R)) :plays)
                                            --If both lists have plays, iterate through both
                                            convertAllPossPlaysToList' ((leftD:restL),(rightD:restR)) plays = convertAllPossPlaysToList' (restL,restR) newPlays
                                                where
                                                leftScore = getScoreFromPlayEnd leftD board L
                                                rightScore = getScoreFromPlayEnd rightD board R
                                                leftPlay = ((leftD,L),leftScore)
                                                rightPlay = ((rightD,R),rightScore)
                                                newPlays = leftPlay : rightPlay : plays
        
    --DomsPlayer who focuses on defending        
    defensivePlayer :: DomsPlayer
    defensivePlayer hand board player scores
        | board==InitBoard && includesDomino hand (5,4) = ((5,4),L) --If taking the first move, player will always play (5,4) if they have it
        --If the opponent is one move away from winning, will choose the play which is most likely to cause the opponent to block
        |(oneMoveAway otherPlayer scores) && getMostLikelyBlock(loopOrderedDotsToBlock hand dotCountList board)/=Nothing = fromJust(getMostLikelyBlock(loopOrderedDotsToBlock hand dotCountList board))
        --The player will look for a safe play which gets them the highest score and there are either 0,1,2 dominos left they could respond with
        | getHighestScoredSafePlay(playUnlikelyDom hand board)/=Nothing = fromJust (getHighestScoredSafePlay(playUnlikelyDom hand board))
        --If no dominos which could block, then just play the highest scoring domino
        | otherwise =  fromJust(getHighestScoreNormalPlay hand board)
            where
            otherPlayer = getOtherPlayer player
            dotCountList = countDotsInList(opponentPossiblesMinusKnocks(checkWhereKnocked board otherPlayer) (opponentPossibles board hand))
            scoreTo59 = 59 - (getPlayerScore player scores)
            scoreDifference = 61 - (getPlayerScore player scores)
    
            
    --DomsPlayer who focuses on attacking to get points        
    attackPlayer :: DomsPlayer
    attackPlayer hand board player scores
        | board==InitBoard && includesDomino hand (5,4) = ((5,4),L) --If taking the first move, player will always play (5,4)
        --If itself is one move away from winning, look for a play which will get them to win
        | (oneMoveAway player scores) && (checkScorePossible hand board scoreDifference player) /=Nothing = fromJust(checkScorePossible hand board scoreDifference player)
        --If itself is four points from getting to 59, see if there is a play to get them to exactly 59
        | playerScore<59 && playerScore>54 && checkScorePossible hand board scoreTo59 player /=Nothing = fromJust(checkScorePossible hand board scoreTo59 player)
        --If no other tactics are working, play the highest scoring domino
        | otherwise = fromJust(getHighestScoreNormalPlay hand board)
            where
            playerScore = getPlayerScore player scores
            scoreTo59 = 59 - (getPlayerScore player scores)
            scoreDifference = 61 - (getPlayerScore player scores)
            

            
            

        
                                          

    
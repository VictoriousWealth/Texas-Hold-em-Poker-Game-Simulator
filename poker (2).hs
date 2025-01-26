-- $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
            --This is an implementation of a simulation of poker texas hold'em no limit made solely by me
-- $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

import System.Random (randomRIO)
import System.IO.Unsafe
import Data.List (elemIndex, sort, (\\), nub, intersect)

-- #####################################################################################################################
                                    -- A list of meaningless helper function
-- #####################################################################################################################
contentSeparator = replicate 148 '='
displayPlayerInfo :: Player -> String
displayPlayerInfo p
    | (null.hand) p =
        (name p)
        ++ " with "
        ++ (show.chips) p
        ++ " chips"
    | otherwise =
        (name p)
        ++ " with "
        ++ (show.chips) p
        ++ " chips and hand "
        ++ (show.hand) p
printSeparator = putStrLn $ contentSeparator
printSeparatorStartLine = putStrLn $ "\n"++contentSeparator
printSeparatorEndLine = putStrLn $ contentSeparator++"\n"
specialPrint1 :: String -> IO()
specialPrint1 content = do
    printSeparatorStartLine
    putStrLn content
    printSeparatorEndLine
specialPrint2 :: String -> String -> IO()
specialPrint2 title content = do
    printSeparator
    putStrLn title
    printSeparator
    putStrLn content
    printSeparatorEndLine
specialPrint3 :: String -> String -> String -> IO()
specialPrint3 title content1 content2 = do
    printSeparatorStartLine
    putStrLn title
    printSeparator
    putStrLn content1
    putStrLn content2
    printSeparatorEndLine
update :: (Eq a) => a -> a -> [a] -> [a]
update _ _ [] = []
update old_element new_element list =
    let index = maybe (-1) (\z->z) $ elemIndex old_element list
    in insert new_element index [e | e <- list, e/=old_element]
    where
        insert :: a -> Int -> [a] -> [a]
        insert element index list = take index list ++ [element] ++ drop index list
shuffle :: [a] -> IO [a] -- helper function that shuffles a list
shuffle [] = pure []
shuffle list = do
    index <- randomRIO (0, length list - 1) -- random chosen index
    let (left, (middle:right)) = splitAt index list -- splits the list to get the desired components
    rest <- shuffle $ left ++ right -- recursively shuffles the remaining elements in the list
    pure (middle:rest) -- returns the randomly chosen element with the shuffled remaining elements
-- $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Enum, Bounded, Eq)
instance Show Suit where
    show Hearts = "H" -- Hearts
    show Diamonds = "D" -- Diamonds
    show Clubs = "C" -- Clubs
    show Spades = "S" -- Spades

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Enum, Bounded, Eq, Ord)
instance Show Rank where
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"

data Card = Card {
    rank :: Rank,
    suit :: Suit
} deriving (Bounded, Eq)
instance Show Card where
    show (Card r s) = show r ++ (show s)
instance Ord Card where
    compare (Card rank1 _) (Card rank2 _) = compare rank1 rank2

type Hand = [Card]
type Community = [Card]
type Deck = [Card]


{-
    Player data type
-}
data Player =
    Player {
        name :: String,
        hand :: Hand,
        chips :: Chips,
        betsPlaced :: [Chips], {- a list of sum of the chips contributed into the pot in a particular round
                                where index 0 refers to the sum contributed at PreFlop and so on
                                hence betsPlaced must be a list of 4 elements when Player constructor is used
                            -}
        isDealer :: Bool, -- to represent if they have the dealer button
        behaviour :: Behaviour, -- a behaviour from [Random, Passive, Aggressive, Smart, Human]
        actions :: [Action], -- a list of actions from [Check, Fold, Bet, Call, Raise, AllIn] made
                                    -- in the current round
        isOutOfTheGame :: Bool
    } deriving (Eq, Show)
--instance Show Player where
--    show (Player name _ chips _ True _ _ _) = "Player: " ++ name ++ " with £" ++ show chips ++ " has dealer button"
--    show (Player name _ chips _ False _ _ _) = "Player: " ++ name ++ " with £" ++ show chips

type Chips = Int

data Behaviour = Random | Passive | Aggressive | Smart | Human deriving (Show, Eq) -- show not needed

data Action = CheckPreFlop | CheckFlop | CheckTurn | CheckRiver |
    Fold | Bet | Call | Raise | AllIn deriving (Show, Eq, Enum, Bounded) -- show not needed

data BettingStage = PreFlop | Flop | Turn | River deriving(Eq, Show, Bounded, Enum)

data GameState =
    GameState {
        players :: [Player],
        deck :: Deck,
        community :: Community,
        pot :: Int,
        bets :: [Int], {- a list of the highest bet made in a particular round and by who,
                            where index 0 refers to the sum contributed at PreFlop and so on-}
        bettingStage :: Maybe BettingStage,
        dealerIndex :: Maybe Int,
        smallBlinderIndex :: Maybe Int,
        bigBlinderIndex :: Maybe Int,
        gamesPlayed :: Int
    } deriving Show


defaultPlayer :: String -> Behaviour -> Player
defaultPlayer playerName behaviour =
    Player {
        name = playerName,
        hand = [], chips = 1000,
        betsPlaced = [0, 0, 0, 0],
        isDealer = False,
        behaviour = behaviour,
        actions = [],
        isOutOfTheGame = False
    }
defaultRandomPlayer :: String -> Player
defaultRandomPlayer playerName =
    defaultPlayer playerName Random
defaultPassivePlayer :: String -> Player
defaultPassivePlayer playerName =
    defaultPlayer playerName Passive
defaultAggressivePlayer :: String -> Player
defaultAggressivePlayer playerName =
    defaultPlayer playerName Aggressive
defaultSmartPlayer :: String -> Player
defaultSmartPlayer playerName =
    defaultPlayer playerName Smart
defaultHumanPlayer :: String -> Player
defaultHumanPlayer playerName =
    defaultPlayer playerName Human


{-
    Creates a list of poker players based on the given configuration.

    Parameters:
    - `humanPlayerName`: Optional name for the human player.
    - `(r, p, a, s)`: A tuple specifying the number of players for each strategy:
      - `r`: Random players
      - `p`: Passive players
      - `a`: Aggressive players
      - `s`: Smart players

    Returns:
    - A list of players.

    Note: when the total number of players is more than 10 (exclusive) only the first 10 players will be returned
            starting from the human player (if any), random, passive, aggressive, smarts
    Note: when any of the integers in the tuple is non-negative, the players for that type won't be created
-}
createPlayers :: Maybe String -> (Int, Int, Int, Int) -> [Player]
createPlayers humanPlayerName (r, p, a, s) =
    let generateNames prefix n
            | n <= 0 = []
            | otherwise = take n [ prefix ++ show i | i <- [1..] ] {- the list comprehension returns a list of numbers
                                                                    1 to 4 as a list of strings
                                                                         so result is ["Random 1", "Random 2", ..-}

        rands = map defaultRandomPlayer $ generateNames "Random " r -- generating a list of r random players
        pasvs = map defaultPassivePlayer $ generateNames "Passive " p -- generating a list of p passive players
        aggrs = map defaultAggressivePlayer $ generateNames "Aggressive " a -- generating a list of a aggressive players
        smarts = map defaultSmartPlayer $ generateNames "Smart " s -- generating a list of s smart players

        human =
            case humanPlayerName of
                (Just name) -> [defaultHumanPlayer name]
                _           -> []
        players =
            human ++ (take (if null human then 10 else 9) $ rands++pasvs++aggrs++smarts) {- takes the first 10 players
                                                                                            if too many players have
                                                                                                been chosen -}
        adjustedPlayers =
            case (length players) of
                0 -> map defaultRandomPlayer $ generateNames "Random " 2 {- creates 2 random players to return,
                                                                            as number of returned players cannot be 0 -}
                1 -> players ++ (map defaultRandomPlayer $ generateNames "Random " 1) -- adds 1 random player on top
                _ -> players -- if its more than 2 players then allow to return

    in adjustedPlayers


{-
    creates a new game state with an empty deck
    and no dealer, small blinder, or big blinder indexes
    with an attribute that stores the number of games played
-}
createGameState :: GameState
createGameState =
    GameState {
        players=createPlayers Nothing (0, 1, 0, 1),
        deck=[],
        community=[],
        pot=0,
        bets=[0,0,0,0],
        bettingStage=Nothing,
        dealerIndex=Nothing,
        smallBlinderIndex=Nothing,
        bigBlinderIndex=Nothing,
        gamesPlayed=0
    }




{-
    Updates the game state with a newly generated deck of cards.
    The deck is created using all possible combinations of 'Rank' and 'Suit'.
-}
createDeck :: IO GameState -> IO GameState
createDeck ioGameState =
    let newDeck = [Card r s | r <- [minBound..], s <- [minBound..]] {- forms a deck using list comprehension
                                                                        where r stands for rank and s for suit-}
    in fmap (\g -> g{deck=newDeck}) ioGameState -- updates the game state with the new deck




{-
    Shuffles the deck in the current game state.
    The function retrieves the existing deck from the game state, shuffles it,
    and updates the game state with the new shuffled deck.
-}
shuffleDeck :: IO GameState -> IO GameState
shuffleDeck ioGameState = do
    oldDeck <- fmap deck ioGameState -- the current deck
    newDeck <- shuffle oldDeck -- which is the current deck but shuffled :)
    fmap (\g -> g{deck=newDeck}) ioGameState -- returns updated gameState with deck shuffled




{-
    The `dealCards` function handles two cases for distributing cards:
    1. Dealing cards to the community (shared cards on the table).
    2. Dealing cards to players (their private hand).
    The input is an `Either` type:
      - `Left (numberOfCards, gameState)`: Deals a specified number of cards to the community.
      - `Right gameState`: Deals two private cards to each player.

    Outputs the updated `GameState`.
-}
dealCards :: Either (Int, GameState) GameState -> GameState
dealCards (Left (numberOfCards, gameState)) = -- When dealing to the community
     let (updatedCommunity, updatedDeck) = splitAt numberOfCards $ deck gameState
     {-
        `splitAt` divides the deck into two parts:
            - `updatedCommunity`: The first `numberOfCards` from the deck, which are dealt to the community.
            - `updatedDeck`: The remaining cards after dealing.
     -}
     in gameState{community=(community gameState)++updatedCommunity, deck=updatedDeck}
dealCards (Right gameState) = -- When dealing to the players
    let (cardsGroupedBy2, updatedDeck) =
            splitAt (2*(length $ players gameState)) $ deck gameState
            {-
               `splitAt` divides the deck into:
               - `cardsGroupedBy2`: The number of cards required to deal 2 cards to each player.
               - `updatedDeck`: The remaining cards after dealing.
               `length (players gameState)` ensures 2 cards are dealt per player.
            -}

        updatedPlayersHands = zip cardsGroupedBy2 $ reverse cardsGroupedBy2
        {-
           Create pairs of cards such that:
               - The first card is paired with the last card,
               - The second card is paired with the second-to-last card, and so on.
        -}
        updatedPlayers = [p{hand=[c1, c2]} | (p, (c1, c2)) <- zip (players gameState) updatedPlayersHands]
         {-
           For each player, assign a tuple of cards `[c1, c2]` as their private hand.
           - `zip (players gameState) updatedPlayersHands` pairs each player with a tuple of two cards.
           - The list comprehension updates each player's `hand` field with their assigned cards.
        -}
    in gameState{players=updatedPlayers, deck=updatedDeck}




check :: GameState -> Player -> IO GameState
check gameState player = do
    let
        updatedActions =
            (actions player)++([[CheckPreFlop], [CheckFlop], [CheckTurn], [CheckRiver]] !! fromEnumBettingStage)
        updatedPlayer = player{actions=updatedActions}

        indexOfOutDatedPlayer = maybe (-1) (\x -> x) $ elemIndex player $ players gameState
        updatedPlayers =
            (take indexOfOutDatedPlayer $ players gameState) ++ [updatedPlayer] ++
            (drop (indexOfOutDatedPlayer+1) $ players gameState)

    specialPrint2 "Check" ((show player) ++ " has checked.")

    pure gameState{players=updatedPlayers}
    where
        fromEnumBettingStage =
            case (bettingStage gameState) of
                Just stage -> fromEnum stage

{-
    TODO comments
-}
fold :: GameState -> Player -> IO GameState
fold gameState player = do
    let updatedActions = (actions player)++[Fold]

        updatedPlayer = player{actions=updatedActions, hand = []}

        indexOfOutDatedPlayer = maybe (-1) (\x -> x) $ elemIndex player $ players gameState
        updatedPlayers =
            (take indexOfOutDatedPlayer $ players gameState) ++ [updatedPlayer] ++
            (drop (indexOfOutDatedPlayer+1) $ players gameState)

    specialPrint2 "Fold" ((show player) ++ " folded.")

    pure gameState{players=updatedPlayers}

allIn :: GameState -> Player -> IO GameState
allIn gameState player = do
    let updatedBetsPlaced =
            (take stageIndex $ betsPlaced player) ++
            [((betsPlaced player) !! stageIndex)+(chips player)] ++
            (drop (stageIndex+1) $ betsPlaced player)
        updatedActions = (actions player)++[AllIn]

        updatedPlayer = player{chips=0, betsPlaced=updatedBetsPlaced, actions=updatedActions}

        indexOfOutDatedPlayer =
            case (elemIndex player $ players gameState) of
                Just n -> n
        updatedPlayers =
            (take indexOfOutDatedPlayer $ players gameState) ++ [updatedPlayer] ++
            (drop (indexOfOutDatedPlayer+1) $ players gameState)

        updatedBets
            | (chips player) > (bets gameState) !! stageIndex
                = take stageIndex (bets gameState) ++ [chips player] ++ drop (stageIndex+1) (bets gameState)
            | otherwise
                = bets gameState


    specialPrint3 "All-in" ((show player) ++ " has gone all in with " ++ (show $ chips player))
        ((name player) ++ " has now " ++ (show $ chips updatedPlayer) ++ " chips.")

    pure gameState{players=updatedPlayers, bets=updatedBets}
    where
        stageIndex =
            case (bettingStage gameState) of
                Just b -> fromEnum b

{-
    TODO comments
-}
bet :: GameState -> Player -> Int -> IO GameState
bet gameState player amount
    | hasSomeoneBet = pure gameState
    | shouldICallAllIn = allIn gameState player
    | otherwise = do
        let updatedChips = (chips player)-amount
            updatedBetsPlaced =
                (take stageIndex $ betsPlaced player) ++
                [((betsPlaced player) !! stageIndex)+amount] ++
                (drop (stageIndex+1) $ betsPlaced player)
            updatedActions = (actions player)++[Bet]

            updatedPlayer = player{chips=updatedChips, betsPlaced=updatedBetsPlaced, actions=updatedActions}

            indexOfOutDatedPlayer = maybe (-1) (\x -> x) $ elemIndex player $ players gameState
            updatedPlayers =
                (take indexOfOutDatedPlayer $ players gameState) ++ [updatedPlayer] ++
                (drop (indexOfOutDatedPlayer+1) $ players gameState)

            updatedBets = (take stageIndex (bets gameState)) ++ [amount] ++ drop (stageIndex+1) (bets gameState)


        if (elemIndex updatedPlayer updatedPlayers) == (smallBlinderIndex gameState) && currentBettingStage == PreFlop
                && ((bets gameState) !! stageIndex)==0
            then specialPrint3 "Force Small Blind" (show player ++ " has been forced to make a bet of " ++ (show amount)
                ++ ".") ((name player) ++ " has now " ++ (show $ chips updatedPlayer) ++ " chips.")
        else specialPrint3 "Bet" (show player ++ " has made a bet of " ++ (show amount) ++ ".")
            ((name player) ++ " has now " ++ (show $ chips updatedPlayer) ++ " chips.")
        pure gameState{players=updatedPlayers, bets=updatedBets}
    where
        currentBettingStage =
            case (bettingStage gameState) of
                Just b -> b
        stageIndex = fromEnum currentBettingStage
        shouldICallAllIn = (chips player) <= amount
        hasSomeoneBet = ((bets gameState) !! stageIndex)/=0

{-
    TODO comments
-}
raise :: GameState -> Player -> Int -> IO GameState
raise gameState player toBeMatchedAmount
    | shouldICallAllIn = allIn gameState player
    | otherwise = do
        let updatedChips = (chips player)-amountToAdd
            updatedBetsPlaced =
                (take stageIndex $ betsPlaced player) ++
                [toBeMatchedAmount] ++
                (drop (stageIndex+1) $ betsPlaced player)
            updatedActions = (actions player)++[Raise]

            updatedPlayer = player{chips=updatedChips, betsPlaced=updatedBetsPlaced, actions=updatedActions}

            indexOfOutDatedPlayer =
                case (elemIndex player $ players gameState) of
                    Just n -> n
            updatedPlayers =
                (take indexOfOutDatedPlayer $ players gameState) ++ [updatedPlayer] ++
                (drop (indexOfOutDatedPlayer+1) $ players gameState)

            updatedBets =
                (take stageIndex (bets gameState)) ++ [toBeMatchedAmount] ++ drop (stageIndex+1) (bets gameState)


        if (elemIndex updatedPlayer updatedPlayers) == (bigBlinderIndex gameState) && currentBettingStage == PreFlop
                && ((bets gameState) !! stageIndex) == 1
            then specialPrint3 "Force Big Blind" (show player ++ " has been forced to raise the bet to "
            ++ (show amountToAdd) ++".") ((name player) ++ " has now " ++ (show $ chips updatedPlayer) ++ " chips.")
        else
            specialPrint3 "Raise" ((show player) ++ " has raise the bet to " ++ (show toBeMatchedAmount) ++
                " by putting in an additional " ++ (show amountToAdd))
                    ((name player) ++ " has now " ++ (show $ chips updatedPlayer) ++ " chips.")

        pure gameState{players=updatedPlayers, bets=updatedBets}
    where
        currentBettingStage =
            case (bettingStage gameState) of
                Just b -> b
                Nothing -> PreFlop
        stageIndex = fromEnum currentBettingStage
        amountToAdd = toBeMatchedAmount - ((betsPlaced player) !! stageIndex)
        shouldICallAllIn = (chips player) <= amountToAdd

{-
    TODO comments
-}
call :: GameState -> Player -> IO GameState
call gameState player
    | shouldICallAllIn = allIn gameState player
    | otherwise = do
        let updatedChips = (chips player)-amountToAdd
            updatedBetsPlaced =
                (take stageIndex $ betsPlaced player) ++
                [((betsPlaced player) !! stageIndex)+amountToAdd] ++
                (drop (stageIndex+1) $ betsPlaced player)
            updatedActions = (actions player)++[Call]

            updatedPlayer = player{chips=updatedChips, betsPlaced=updatedBetsPlaced, actions=updatedActions}

            indexOfOutDatedPlayer =
                case (elemIndex player $ players gameState) of
                    Just n -> n
            updatedPlayers =
                (take indexOfOutDatedPlayer $ players gameState) ++ [updatedPlayer] ++
                (drop (indexOfOutDatedPlayer+1) $ players gameState)


--        specialPrint3 "Call" ((show player) ++ " has called with " ++ (show amountToAdd) ++
--            " to match " ++ (show toBeMatchedAmount))
--            ((name player) ++ " has now " ++ (show $ chips updatedPlayer) ++ " chips.")

        pure gameState{players=updatedPlayers}
    where
        stageIndex =
            case (bettingStage gameState) of
                Just b -> fromEnum b
        toBeMatchedAmount = (bets gameState) !! stageIndex
        amountToAdd = toBeMatchedAmount - ((betsPlaced player) !! stageIndex)
        shouldICallAllIn = (chips player) <= amountToAdd

playerRandomStrategy :: GameState -> Player -> [Action] -> IO GameState
playerRandomStrategy gameState playerToAct validActions = do
    amount <- randomRIO(2*currentBet, chips playerToAct)

--    putStrLn $ show validActions
    action <- randomRIO (0 :: Int, length validActions - 1)
    case (validActions !! action) of
        Fold -> fold gameState playerToAct
        Call -> call gameState playerToAct
        Bet -> bet gameState playerToAct amount
        Raise -> raise gameState playerToAct amount
        AllIn -> allIn gameState playerToAct
        a | a `elem` [CheckPreFlop, CheckFlop, CheckTurn, CheckRiver] -> check gameState playerToAct
        _ -> error "Invalid Action"
    where
        currentBet = (bets gameState) !! fromEnumBettingStage
        fromEnumBettingStage =
            case bettingStage gameState of
                Just b -> fromEnum b

playerPassiveStrategy :: GameState -> Player -> [Action] -> IO GameState
playerPassiveStrategy gameState playerToAct validActions = do
    revisedValidActions <-
            shuffle $ concat $ map (\a -> if isCheck a then replicate 20 a else [a]) $ validActions \\
                [Raise, Bet, AllIn]

    action <- randomRIO (0 :: Int, length revisedValidActions - 1)
    case (revisedValidActions !! action) of
        Fold -> fold gameState playerToAct
        Call -> call gameState playerToAct
        a | isCheck a -> check gameState playerToAct
        _ -> error "Invalid Action"
    where
        currentBet = (bets gameState) !! fromEnumBettingStage
        fromEnumBettingStage =
            case bettingStage gameState of
                Just b -> fromEnum b
        isCheck a = a `elem` [CheckPreFlop, CheckFlop, CheckTurn, CheckRiver]

playerAggressiveStrategy :: GameState -> Player -> [Action] -> IO GameState
playerAggressiveStrategy gameState playerToAct validActions = do
    revisedValidActions <-
            shuffle $ concat
                $ map (\a -> if a==Call then replicate 10 a else if a==Bet || a==Raise then replicate 5 a else [a])
                    $ validActions

    action <- randomRIO (0 :: Int, length revisedValidActions - 1)
    amount <- randomRIO(2*currentBet, chips playerToAct)
    case (revisedValidActions !! action) of
        Fold -> fold gameState playerToAct
        Call -> call gameState playerToAct
        Bet -> bet gameState playerToAct amount
        Raise -> raise gameState playerToAct amount
        AllIn -> allIn gameState playerToAct
        a | a `elem` [CheckPreFlop, CheckFlop, CheckTurn, CheckRiver] -> check gameState playerToAct
        _ -> error "Invalid Action"
    where
        currentBet = (bets gameState) !! fromEnumBettingStage
        fromEnumBettingStage =
            case bettingStage gameState of
                Just b -> fromEnum b

playerHumanPlay :: GameState -> Player -> [Action] -> IO GameState
playerHumanPlay gameState playerToAct validActions = do
    pure gameState

playerSmartStrategy :: GameState -> Player -> [Action] -> IO GameState
playerSmartStrategy gameState playerToAct validActions =
    case (bettingStage gameState) of
         -- when betting round is PreFlop
            -- if highest bet >= 350 always fold
            -- else if hand are ONE_PAIR && any card is 8 or higher || both cards are in between ACE and EIGHT raise
            -- else if hand are ONE_PAIR || hand in sequence (i.e. all@[a, b] where all is sorted and b = succ a)
                -- then call
            -- else fold
         Just PreFlop -> do
            gameState' <- case gameState of
                _ | canCheck -> check gameState playerToAct
                  | (length [p | p <- players gameState, (not.null) (actions p) && (last $ actions p)/= Fold,
                    not(isOutOfTheGame p)]) <= 6
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | currentBet <= floor(0.45*(fromIntegral $ chips playerToAct)) && canCheck
                    -> check gameState playerToAct
                  | currentBet <= floor(0.45*(fromIntegral $ chips playerToAct)) && Fold `elem` validActions
                    -> fold gameState playerToAct
                  | greatestOnePair (hand playerToAct) /= Nothing && any (\c -> (rank c)>=Six) (hand playerToAct)
                    && Raise `elem` validActions
                        -> raise gameState playerToAct amount
                  | all (\c -> Six<=rank c) (hand playerToAct)
                    && Raise `elem` validActions
                         -> raise gameState playerToAct amount
                  | greatestOnePair (hand playerToAct) /= Nothing
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | (rank $ minimum $ hand playerToAct) == (pred $ rank $ maximum $ hand playerToAct)
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | (maximum $ map (rank) $ hand playerToAct) < Five && canCheck
                    -> check gameState playerToAct
                  | otherwise -> fold gameState playerToAct
            pure gameState'
         Just Flop -> do
            case gameState of
                _ | canCheck -> check gameState playerToAct
                  | (length [p | p <- players gameState, (not.null) (actions p) && (last $ actions p)/= Fold,
                    not(isOutOfTheGame p)]) <= 2
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | Nothing /= greatestRoyalFlush cards && AllIn `elem` validActions -> allIn gameState playerToAct
                  | Nothing /= greatestStraightFlush cards && AllIn `elem` validActions
                    -> allIn gameState playerToAct
                  | currentBet <= floor(0.70*(fromIntegral $ chips playerToAct)) && Fold `elem` validActions
                    -> fold gameState playerToAct
                  | Nothing /= greatestFourOfAKind cards && Raise `elem` validActions
                    -> raise gameState playerToAct amount
                  | Nothing /= (greatestFullHouse cards) && currentBet <= floor(0.55*(fromIntegral $ chips playerToAct))
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | Nothing /= (greatestFlush cards) && currentBet <= floor(0.55*(fromIntegral $ chips playerToAct))
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | Nothing /= (greatestStraight cards) && currentBet <= floor(0.35*(fromIntegral $ chips playerToAct))
                    && Raise `elem` validActions
                        -> raise gameState playerToAct amount
                  | Nothing /= (greatestStraight cards) && currentBet <= floor(0.65*(fromIntegral $ chips playerToAct))
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | Nothing /= (greatestThreeOfAKind cards) && currentBet <= floor(0.55*(fromIntegral $ chips playerToAct))
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | Nothing /= (greatestTwoPairs cards) && currentBet <= floor(0.5*(fromIntegral $ chips playerToAct))
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | otherwise -> fold gameState playerToAct
         Just Turn -> do
            case gameState of
                _ | canCheck -> check gameState playerToAct
                  | (length [p | p <- players gameState, (not.null) (actions p) && (last $ actions p)/= Fold,
                    not(isOutOfTheGame p)]) <= 4
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | Nothing /= greatestRoyalFlush cards && Raise `elem` validActions
                    -> raise gameState playerToAct amount
                  |  Nothing /= greatestStraightFlush cards && Raise `elem` validActions
                    -> raise gameState playerToAct amount
                  | Nothing /= greatestFourOfAKind cards && Raise `elem` validActions
                    -> raise gameState playerToAct amount
                  | Nothing /= greatestFullHouse cards && currentBet <= floor(0.75*(fromIntegral $ chips playerToAct))
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | Nothing /= greatestFlush cards && currentBet <= floor(0.65*(fromIntegral $ chips playerToAct))
                    && Raise `elem` validActions
                        -> raise gameState playerToAct amount
                  | Nothing /= greatestFlush cards && currentBet <= floor(0.55*(fromIntegral $ chips playerToAct))
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | Nothing /= greatestStraight cards && currentBet <= floor(0.5*(fromIntegral $ chips playerToAct))
                    && Raise `elem` validActions
                        -> raise gameState playerToAct amount
                  | Nothing /= greatestStraight cards && currentBet <= floor(0.65*(fromIntegral $ chips playerToAct))
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | Nothing /= greatestThreeOfAKind cards && currentBet <= floor(0.70*(fromIntegral $ chips playerToAct))
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | Nothing /= greatestTwoPairs cards && currentBet <= floor(0.45*(fromIntegral $ chips playerToAct))
                    && Call `elem` validActions
                        -> call gameState playerToAct
                  | otherwise -> fold gameState playerToAct
         Just River -> do
             case gameState of
                 _ | canCheck -> check gameState playerToAct
                   | (length [p | p <- players gameState, (not.null) (actions p) && (last $ actions p)/= Fold,
                    not(isOutOfTheGame p)]) <= 5
                     && Call `elem` validActions
                         -> call gameState playerToAct
                   | Nothing /= greatestRoyalFlush cards && Raise `elem` validActions
                     -> raise gameState playerToAct amount
                   |  Nothing /= greatestStraightFlush cards && Raise `elem` validActions
                     -> raise gameState playerToAct amount
                   | Nothing /= greatestFourOfAKind cards && Raise `elem` validActions
                     -> raise gameState playerToAct amount
                   | Nothing /= greatestFullHouse cards && currentBet <= floor(0.85*(fromIntegral $ chips playerToAct))
                     && Call `elem` validActions
                         -> call gameState playerToAct
                   | Nothing /= greatestFlush cards && currentBet <= floor(0.75*(fromIntegral $ chips playerToAct))
                     && Raise `elem` validActions
                         -> raise gameState playerToAct amount
                   | Nothing /= greatestFlush cards && currentBet <= floor(0.65*(fromIntegral $ chips playerToAct))
                     && Call `elem` validActions
                         -> call gameState playerToAct
                   | Nothing /= greatestStraight cards && currentBet <= floor(0.65*(fromIntegral $ chips playerToAct))
                     && Raise `elem` validActions
                         -> raise gameState playerToAct amount
                   | Nothing /= greatestStraight cards && currentBet <= floor(0.55*(fromIntegral $ chips playerToAct))
                     && Call `elem` validActions
                         -> call gameState playerToAct
                   | Nothing /= greatestThreeOfAKind cards &&
                    currentBet <= floor(0.60*(fromIntegral $ chips playerToAct))
                     && Call `elem` validActions
                         -> call gameState playerToAct
                   | Nothing /= greatestTwoPairs cards && currentBet <= floor(0.45*(fromIntegral $ chips playerToAct))
                     && Call `elem` validActions
                         -> call gameState playerToAct
                   | otherwise -> fold gameState playerToAct
    where
        currentBet = (bets gameState) !! fromEnumBettingStage
        amount = (2*currentBet)
        fromEnumBettingStage =
            case bettingStage gameState of
                Just b -> fromEnum b
        cards = (hand playerToAct)++(community gameState)
        canCheck = any (\a -> a `elem` validActions) [CheckPreFlop, CheckFlop, CheckTurn, CheckRiver]




playersTakeAction :: GameState -> Int -> [Player] -> IO GameState
playersTakeAction gameState indexOfPlayer playersInTurn -- make sure u update playersInTurn so that it
    | indexOfPlayer == length playersInTurn = pure gameState -- return gameState as end of the list has been reached

    -- if player is out of game then go on to the next player
    | isOutOfTheGame playerToAct
        = playersTakeAction gameState (indexOfPlayer+1) playersInTurn

    -- go to the next player as player has folded or went all in and can't do any more possible actions
    | (not(null $ actions playerToAct) && (last $ actions playerToAct) `elem` [Fold, AllIn])
        = playersTakeAction gameState (indexOfPlayer+1) playersInTurn

    -- if all players in playersInTurn have last action check(+bettingStage)
        -- then end recursive call
    | all (\p-> (not.null) (actions p) &&
        (last (actions p) == [CheckPreFlop, CheckFlop, CheckTurn, CheckRiver] !! fromEnumBettingStage
        || last (actions p) == Fold)) allPlayers
            = pure gameState

    -- if all players in playersInTurn have either match the current bet or outOfGame or folded or allin
        -- then end recursive call
    | all
        (\p-> (
            (not(null $ actions p) && (last $ actions p) `elem` [Fold, AllIn])
            || isOutOfTheGame p
            || ((betsPlaced p) !! fromEnumBettingStage)==((bets gameState) !! fromEnumBettingStage)
            ) && ((bets gameState) !! fromEnumBettingStage) /= 0
        )

            allPlayers = pure gameState

    -- before player takes action check whether it is the only player who hasn't folded or out of the game
        -- if it is then return gameState{bettingState=Nothing} and end recursive call
    | all (\p -> isOutOfTheGame p || (not(null $ actions p) && (last $ actions p) == Fold))
        (filter (/=playerToAct) playersInTurn) = pure gameState

    -- if the player is out of game or last action elem fold, allIn then return gameState
        -- and recursively call playersTakeAction
    | (not(null $ actions playerToAct) && (last $ actions playerToAct) `elem` [Fold, AllIn]) ||
        isOutOfTheGame playerToAct
            = playersTakeAction gameState (indexOfPlayer+1) playersInTurn

    | otherwise = do
        let
            availableActions =
                [Fold, Call, Raise, Bet, AllIn]
                    ++ ([[CheckPreFlop], [CheckFlop], [CheckTurn], [CheckRiver]] !! fromEnumBettingStage)
            -- collect all actions into a list of options
            betToBeMatched = ((bets gameState) !! fromEnumBettingStage)
            validActions = filter (\a ->
                    -- check if it's eligible to bet by checking that there is no bet been made for the bettingStage
                    (a==Bet && betToBeMatched == 0) ||
                    -- check if it's eligible to check by checking that there is no bet been made for the bettingStage
                    (a `elem` [CheckPreFlop, CheckFlop, CheckRiver, CheckTurn] &&  betToBeMatched == 0) ||
                    -- check if it's eligible to raise by checking that a bet has been made and that
                        -- it has enough chips to raise
                    (a==Raise && betToBeMatched /= 0 && (fromIntegral $ chips playerToAct)>=(2*betToBeMatched)) ||
                    -- check if it's eligible to call by checking that a bet has been made
                    (a==Call && betToBeMatched /= 0) ||
                    -- check if it's eligible to fold by checking if a bet has been made
                    (a==Fold && betToBeMatched /= 0)
                ) availableActions

        updatedGameState <-
            case (behaviour playerToAct) of
                -- submit these options to the player strategy
                Random -> playerRandomStrategy gameState playerToAct validActions
                Passive -> playerPassiveStrategy gameState playerToAct validActions
                Aggressive -> playerAggressiveStrategy gameState playerToAct validActions
                Smart -> playerSmartStrategy gameState playerToAct validActions
                Human -> playerHumanPlay gameState playerToAct validActions

        -- use this updated gameState to recursively call playersTakeAction,
            -- but make sure that playersInTurn is updated if someone raises or bets
                -- in such a way that you add all the players that need to match the new bet
                    -- and the last player is now the player that's before this player in playersInTurn

        let partiallyUpdatedPlayersInTurn =
                foldl (\acc (oldP, newP) -> update oldP newP acc)
                    playersInTurn $ zip allPlayers $ players updatedGameState
            updatedPlayersInTurn =
                partiallyUpdatedPlayersInTurn ++ (take indexOfPlayer partiallyUpdatedPlayersInTurn)
        playersTakeAction updatedGameState (indexOfPlayer+1) updatedPlayersInTurn

    where
        fromEnumBettingStage =
            case (bettingStage gameState) of
                Just b -> fromEnum b
        allPlayers = players gameState
        playerToAct = playersInTurn !! indexOfPlayer




data HandRanking =
    HighCard (Rank) [Rank]
        | OnePair (Rank) [Rank] --
        | TwoPair (Rank, Rank) [Rank]
        | ThreeOfAKind (Rank) [Rank]
        | Straight (Rank) [Rank] -- a straight is 5 consecutive cards (special case with Ace)
        | Flush (Suit) (Rank, Rank, Rank, Rank, Rank) [Rank]
        | FullHouse (Rank, Rank) [Rank] -- so it's three of a kind && one pair in that order
        | FourOfAKind (Rank) [Rank]
        | StraightFlush (Rank) [Rank]
        | RoyalFlush [Rank] -- royal flush doesn't need the rank tuple as it's going to be the same
                                -- for every possible royal flush
            deriving (Show, Eq)
instance Ord HandRanking where
    compare (HighCard (rank1) kickers1) (HighCard (rank2) kickers2)
        | result == EQ = compare kickers1 kickers2
        | otherwise = result
        where result = compare rank1 rank2
    compare (OnePair (rank1) [kickers1]) (OnePair (rank2) [kickers2])
        | result == EQ = compare kickers1 kickers2
        | otherwise = result
        where result = compare rank1 rank2
    compare (TwoPair (rank1a, rank1b) [kickers1]) (TwoPair (rank2a, rank2b) [kickers2])
    -- assuming ran1a >= rank1b and rank2a >= rank2b
        | resultA == EQ && resultB == EQ = compare kickers1 kickers2
        | resultA == EQ = resultB
        | otherwise = resultA
        where
            resultA = compare rank1a rank2a
            resultB = compare rank1b rank2b
    compare (ThreeOfAKind (rank1) [kickers1]) (ThreeOfAKind (rank2) [kickers2])
        | result == EQ = compare kickers1 kickers2
        | otherwise = result
        where result = compare rank1 rank2
    compare (Straight (Ace) kickers1) (Straight (Ace) kickers2)
        = compare kickers1 kickers2
    compare (Straight (Ace) kickers1) (Straight (_) kickers2)
        = LT
    compare (Straight (_) kickers1) (Straight (Ace) kickers2)
        = GT
    compare (Straight (rank1) kickers1) (Straight (rank2) kickers2)
        | result == EQ = compare kickers1 kickers2
        | otherwise = result
        where result = compare rank1 rank2
    compare (Flush _ (rank1a, rank1b, rank1c, rank1d, rank1e) kickers1)
        (Flush _ (rank2a, rank2b, rank2c, rank2d, rank2e) kickers2)
            | result == EQ = compare kickers1 kickers2
            | otherwise = result
            where
                sortedRanks1 = reverse $ sort [rank1a, rank1b, rank1c, rank1d, rank1e]
                sortedRanks2 = reverse $ sort [rank2a, rank2b, rank2c, rank2d, rank2e]
                result = compare sortedRanks1 sortedRanks2
    compare (FullHouse (rank1a, rank1b) kickers1) (FullHouse (rank2a, rank2b) kickers2)
        | threeOfAKindResult == EQ && onePairResult == EQ = compare kickers1 kickers2
        | threeOfAKindResult == EQ = onePairResult
        | otherwise = threeOfAKindResult
        where
            onePairResult = compare rank1a rank2b
            threeOfAKindResult = compare rank1a rank2a
    compare (FourOfAKind (rank1) kickers1) (FourOfAKind (rank2) kickers2)
        | result == EQ = compare kickers1 kickers2
        | otherwise = result
        where result = compare rank1 rank2
    compare (StraightFlush (Ace) kickers1) (StraightFlush (Ace) kickers2)
        = compare kickers1 kickers2
    compare (StraightFlush (Ace) kickers1) (StraightFlush (_) kickers2)
        = LT
    compare (StraightFlush (_) kickers1) (StraightFlush (Ace) kickers2)
        = GT
    compare (StraightFlush startingRank1 kickers1) (StraightFlush startingRank2 kickers2)
        | result == EQ = compare kickers1 kickers2
        | otherwise = result
        where
            result = compare startingRank1 startingRank2
    compare (RoyalFlush kickers1) (RoyalFlush kickers2)
        = compare kickers1 kickers2
    compare (RoyalFlush _) _ = GT
    compare _ (RoyalFlush _) = LT
    compare (StraightFlush _ _) (_) = GT
    compare (_) (StraightFlush _ _) = LT
    compare (FourOfAKind _ _) (_) = GT
    compare (_) (FourOfAKind _ _) = LT
    compare (FullHouse _ _) (_) = GT
    compare (_) (FullHouse _ _) = LT
    compare (Flush _ _ _) (_) = GT
    compare (_) (Flush _ _ _) = LT
    compare (Straight _ _) (_) = GT
    compare (_) (Straight _ _) = LT
    compare (ThreeOfAKind _ _) (_) = GT
    compare (_) (ThreeOfAKind _ _) = LT
    compare (TwoPair _ _) (_) = GT
    compare (_) (TwoPair _ _) = LT
    compare (OnePair _ _) (_) = GT
    compare (_) (OnePair _ _) = LT




greatestHighCard :: [Card] -> Maybe HandRanking
greatestHighCard cards =
    case (null cards) of
        True -> Nothing
        _    ->
            let ranksOfCards = map (rank) cards
                maximumRank = maximum ranksOfCards
                kickerRanks = ranksOfCards \\ [maximumRank]
            in Just $ HighCard (maximumRank) kickerRanks




greatestOnePair :: [Card] -> Maybe HandRanking
greatestOnePair cards =
    case (containsOnePair) of
        True -> Just $ OnePair (head validPair) kickers
        _    -> Nothing
    where
        containsOnePair = not(null validPair)
        validPair = concat $ take 1 $ filter (\l -> length l == 2) $ map (take 2) sortedGroupedByRankRanks
        sortedGroupedByRankRanks = reverse $ sort $ nub [filter (==r) ranksOfCards | r <- ranksOfCards]
        ranksOfCards = map (rank) cards
        kickers = ranksOfCards \\ validPair




greatestTwoPairs :: [Card] -> Maybe HandRanking
greatestTwoPairs cards =
    case (containsTwoPairs) of
        True -> Just $ TwoPair (head $ head validTwoPairs, last $ last validTwoPairs) kickers
        _    -> Nothing
    where
        containsTwoPairs = (length validTwoPairs) == 2
        validTwoPairs = take 2 $ filter (\l -> length l == 2) $ map (take 2) sortedGroupedByRankRanks
        sortedGroupedByRankRanks = reverse $ sort $ nub [filter (==r) ranksOfCards | r <- ranksOfCards]
        ranksOfCards = map (rank) cards
        kickers = ranksOfCards \\ (concat validTwoPairs)




greatestThreeOfAKind :: [Card] -> Maybe HandRanking
greatestThreeOfAKind cards =
    case (containsThreeOfAKind) of
        True -> Just $ ThreeOfAKind (head validThreeOfAKind) kickers
        _    -> Nothing
    where
        containsThreeOfAKind = (length validThreeOfAKind)==3
        validThreeOfAKind = concat $ take 1 $ filter (\l -> length l == 3) $ map (take 3) sortedGroupedByRankRanks
        sortedGroupedByRankRanks = reverse $ sort $ nub [filter (==r) ranksOfCards | r <- ranksOfCards]
        ranksOfCards = map (rank) cards
        kickers = ranksOfCards \\ validThreeOfAKind




greatestStraight :: [Card] -> Maybe HandRanking
greatestStraight cards
    | foldl (\acc r -> acc && any (==r) orderedRanks) True $ take 5 [Nine .. ]
        = Just $ Straight (Nine) (ranksOfCards \\ (take 5 [Nine .. ]))
    | foldl (\acc r -> acc && any (==r) orderedRanks) True $ take 5 [Eight .. ]
        = Just $ Straight (Eight) (ranksOfCards \\ (take 5 [Eight .. ]))
    | foldl (\acc r -> acc && any (==r) orderedRanks) True $ take 5 [Seven .. ]
        = Just $ Straight (Seven) (ranksOfCards \\ (take 5 [Seven .. ]))
    | foldl (\acc r -> acc && any (==r) orderedRanks) True $ take 5 [Six .. ]
        = Just $ Straight (Six) (ranksOfCards \\ (take 5 [Six .. ]))
    | foldl (\acc r -> acc && any (==r) orderedRanks) True $ take 5 [Five .. ]
        = Just $ Straight (Five) (ranksOfCards \\ (take 5 [Five .. ]))
    | foldl (\acc r -> acc && any (==r) orderedRanks) True $ take 5 [Four .. ]
        = Just $ Straight (Four) (ranksOfCards \\ (take 5 [Four .. ]))
    | foldl (\acc r -> acc && any (==r) orderedRanks) True $ take 5 [Three .. ]
        = Just $ Straight (Three) (ranksOfCards \\ (take 5 [Three .. ]))
    | foldl (\acc r -> acc && any (==r) orderedRanks) True $ take 5 [Two .. ]
        = Just $ Straight (Two) (ranksOfCards \\ (take 5 [Two .. ]))
    | foldl (\acc r -> acc && any (==r) orderedRanks) True $ [Two .. Five]++[Ace]
        = Just $ Straight (Ace) (ranksOfCards \\ (Ace:[Two .. Five]))
    | otherwise = Nothing
    where
        orderedRanks = concat $ map (take 1) sortedGroupedByRankRanks
        sortedGroupedByRankRanks = reverse $ sort $ nub [filter (==r) ranksOfCards | r <- ranksOfCards]
        ranksOfCards = map (rank) cards




greatestFlush :: [Card] -> Maybe HandRanking
greatestFlush cards =
    case (containsValidFlush) of
        True ->
            let validFlushRanks = [r | (Card r s) <- head validFlushes]
                (s, flush@[r1, r2, r3, r4, r5]) = (suit $ head $ head validFlushes, validFlushRanks)

            in Just $ Flush s (r1, r2, r3, r4, r5) ([rank c | c <- cards] \\ flush)
        _    -> Nothing
    where
        containsValidFlush = not(null validFlushes)
        validFlushes = filter (\l -> length l == 5) $ map (take 5) sortedGroupedBySuitCards
        sortedGroupedBySuitCards = map (reverse.sort) $ sort $ nub
            [filter (\(Card r s') -> s'==s) cards | s <- [minBound ..]]




greatestFullHouse :: [Card] -> Maybe HandRanking
greatestFullHouse cards =
    case (containsFullHouse) of
        True ->
            let
                (triceRepeatedRank, kickerRanks) =
                    case (maybeThreeOfAKind) of
                        Just (ThreeOfAKind triceRepeatedRank' kickerRanks') -> (triceRepeatedRank', kickerRanks')

                (twiceRepeatedRank, kickerKickerRanks) =
                    case (greatestOnePair [Card r Diamonds | r <- kickerRanks]) of
                        Just (OnePair twiceRepeatedRank' kickerKickerRanks') -> (twiceRepeatedRank', kickerKickerRanks')
            in Just $ FullHouse (triceRepeatedRank, twiceRepeatedRank) kickerKickerRanks
        _    -> Nothing
    where
        containsFullHouse = maybeThreeOfAKind /= Nothing && maybeOnePair /= Nothing
        maybeThreeOfAKind = greatestThreeOfAKind cards
        maybeOnePair =
            case (maybeThreeOfAKind) of
                Just (ThreeOfAKind repeatedRank kickerRanks) -> greatestOnePair [Card r Diamonds | r <- kickerRanks]
                Nothing -> Nothing




greatestFourOfAKind :: [Card] -> Maybe HandRanking
greatestFourOfAKind cards =
    case (containsFourOfAKind) of
        True -> Just $ FourOfAKind (head validFourOfAKind) kickers
        _    -> Nothing
    where
        containsFourOfAKind = (length validFourOfAKind)==4
        validFourOfAKind = concat $ take 1 $ filter (\l -> length l == 4) $ map (take 4) sortedGroupedByRankRanks
        sortedGroupedByRankRanks = reverse $ sort $ nub [filter (==r) ranksOfCards | r <- ranksOfCards]
        ranksOfCards = map (rank) cards
        kickers = ranksOfCards \\ validFourOfAKind



greatestStraightFlush :: [Card] -> Maybe HandRanking
greatestStraightFlush cards =
    case (containsStraightFlush) of
        True ->
            let
                (startingRankOfStraight, kickerRanks) =
                    case (head validStraightFlushes) of
                        Just (Straight startingRankOfStraight' kickerRanks') -> (startingRankOfStraight', kickerRanks')

            in Just $ StraightFlush (startingRankOfStraight) kickerRanks

        _    -> Nothing
    where
        -- first we group by suit
        sortedGroupedBySuitCards = map (reverse.sort) $ sort $ nub
            [filter (\(Card r s') -> s'==s) cards | s <- [minBound ..]]
        validStraightFlushes= filter (/=Nothing) $ map (\l -> greatestStraight l) sortedGroupedBySuitCards
        containsStraightFlush = not(null validStraightFlushes)



greatestRoyalFlush :: [Card] -> Maybe HandRanking
greatestRoyalFlush cards =
    case (containsRoyalFlush) of
        True -> Just $ RoyalFlush (ranksOfCards \\ [Ten ..])

        _    -> Nothing
    where
        -- first we group by suit
        ranksOfCards = map (rank) cards
        containsRoyalFlush = foldl (\acc rankInRoyal -> acc && rankInRoyal `elem` ranksOfCards) True [Ten .. ]




evaluateHand :: [Card] -> HandRanking
evaluateHand cards =
    let
        handRankings = [
                greatestRoyalFlush cards,
                greatestStraightFlush cards,
                greatestFourOfAKind cards,
                greatestFullHouse cards,
                greatestFlush cards,
                greatestStraight cards,
                greatestThreeOfAKind cards,
                greatestTwoPairs cards,
                greatestOnePair cards,
                greatestHighCard cards
            ]
        highestHandRanking =
            case (maximum handRankings) of
                Just hh -> hh
    in highestHandRanking




-- #####################################################################################################################
    {-
        The `assignDealerAndBlinds` function is responsible for two critical tasks:
        1. Assigning the dealer button to a player:
           - If a dealer exists, it moves the button to the player on the left.
           - If no dealer exists, the button is assigned to the last player in the list.
        2. Assigning the small and big blinds:
           - The small blind is the player to the left of the dealer (or the dealer themselves in a two-player game).
           - The big blind is the player to the left of the small blind.

        Input:
          - `gameState`: The current state of the game, which includes player information, the current dealer,
                and blind indices.

        Output:
          - An updated `GameState` with the new dealer and blind indices, and updated player states.
    -}
-- #####################################################################################################################
assignDealerAndBlinds :: GameState -> GameState
assignDealerAndBlinds gameState =
    let
        -- Assign the dealer button:
        -- If a dealer exists, move the button to the player on the left.
        -- If no dealer exists, assign it to the last player in the list by default.
        (updatedPlayers, updatedDealerIndex) =
            case (dealerIndex gameState) of
                (Just n) ->
                    let
                        prevDealer = listOfPlayers !! n
                        nextDealerIndex = getClosestActivePlayer n -- Determine the index of the next dealer.
                        nextDealer = listOfPlayers !! nextDealerIndex -- Select the player at the new dealer index.

                        -- Create a list of players where no one has the dealer button.
                        playersWithoutDealer =
                            (take n listOfPlayers) ++ [prevDealer{isDealer=False}] ++ (drop (n+1) listOfPlayers)
                    in
                        -- Update the list of players with the new dealer and return the updated index.
                        ((take nextDealerIndex playersWithoutDealer) ++
                            [nextDealer{isDealer=True}] ++
                                (drop (nextDealerIndex+1) playersWithoutDealer), nextDealerIndex)
                _        ->
                    -- If no dealer exists, assign the button to the last player in the list.
                    let nextDealer = last listOfPlayers
                    in ((init listOfPlayers) ++ [nextDealer{isDealer=True}], length listOfPlayers - 1)

        -- Determine the small blind index:
        -- If there are more than two players, the small blind is the player to the left of the dealer.
        -- Otherwise, the dealer is also the small blind.
        updatedSmallBlinderIndex
            | (length listOfPlayers) > 2 = getClosestActivePlayer updatedDealerIndex
            | otherwise = updatedDealerIndex

        -- Determine the big blind index:
        -- The big blind is always to the left of the small blind.
        updatedBigBlinderIndex = getClosestActivePlayer updatedSmallBlinderIndex
    in
        -- Update the `GameState` with the new dealer, small blind, and big blind indices.
        gameState {
            players = updatedPlayers,
            dealerIndex = Just updatedDealerIndex,
            smallBlinderIndex = Just updatedSmallBlinderIndex,
            bigBlinderIndex = Just updatedBigBlinderIndex
        }
    where
        listOfPlayers = players gameState
        getClosestActivePlayer n
            | not $ isOutOfTheGame $ listOfPlayers !! (mod (n-1) $ length listOfPlayers)
                = (mod (n-1) $ length listOfPlayers)
            | otherwise = getClosestActivePlayer (n-1)




-- #####################################################################################################################
    {-
        This function will have to handle the different betting rounds such as preFlop, flop, turn, and river.
        Hence it will call the playersTakeAction (takes in a tuple of available options)
        function over a list of players, sorted in from the first player in turn to the last player in
        turn (the dealer). This list will be different based on the betting stage of the current game.
    -}
-- #####################################################################################################################
bettingRound :: IO GameState -> IO GameState
bettingRound ioGameState = do
    gameState <- ioGameState
    case (bettingStage gameState) of
        Just PreFlop -> do
            specialPrint1 "Pre-Flop"
            -- force small blind
            let secondToLastPlayerIndex =
                    case smallBlinderIndex gameState of
                        Just n -> n
            gameState1 <- bet gameState ((players gameState) !! secondToLastPlayerIndex) 1

            -- force big blind
            let lastPlayerIndex =
                    case bigBlinderIndex gameState1 of
                        Just n -> n
            gameState2 <- raise gameState1 ((players gameState1) !! lastPlayerIndex) 2

            -- other players can play now
            let listOfPlayers = playersInOrderOfTurns lastPlayerIndex $ players gameState2
            gameState3 <- playersTakeAction gameState2 0 listOfPlayers

            -- getting how many chips need to be in the pot
            let updatedPot = foldl (\acc p -> acc+(sum $ betsPlaced p)) 0 (players gameState3)

            bettingRound $ pure $ dealCards $ Left (3, gameState3{bettingStage=Just Flop, pot=updatedPot})

        Just Flop -> do
            specialPrint1 "Flop"
            let lastPlayerIndex =
                    case dealerIndex gameState of
                        Just n -> n
                listOfPlayers = playersInOrderOfTurns lastPlayerIndex $ players gameState
            gameState1 <- playersTakeAction gameState 0 listOfPlayers

            -- getting how many chips need to be in the pot
            let updatedPot = foldl (\acc p -> acc+(sum $ betsPlaced p)) 0 (players gameState1)

            bettingRound $ pure $ dealCards $ Left (1, gameState1{bettingStage=Just Turn, pot=updatedPot})

        Just Turn -> do
            specialPrint1 "Turn"
            let lastPlayerIndex =
                    case dealerIndex gameState of
                        Just n -> n
                listOfPlayers = playersInOrderOfTurns lastPlayerIndex $ players gameState
            gameState1 <- playersTakeAction gameState 0 listOfPlayers


            -- getting how many chips need to be in the pot
            let updatedPot = foldl (\acc p -> acc+(sum $ betsPlaced p)) 0 (players gameState1)

            bettingRound $ pure $ dealCards $ Left (1, gameState1{bettingStage=Just River, pot=updatedPot})

        Just River -> do
            specialPrint1 "River"
            let lastPlayerIndex =
                    case dealerIndex gameState of
                        Just n -> n
                listOfPlayers = playersInOrderOfTurns lastPlayerIndex $ players gameState
            gameState1 <- playersTakeAction gameState 0 listOfPlayers

            -- getting how many chips need to be in the pot
            let updatedPot = foldl (\acc p -> acc+(sum $ betsPlaced p)) 0 (players gameState1)

            bettingRound $ pure gameState1{bettingStage=Nothing, pot=updatedPot}

        Nothing -> ioGameState
    where
        playersInOrderOfTurns :: Int -> [Player] -> [Player]
        playersInOrderOfTurns lastPlayerIndex listOfPlayers = (reverse $ take lastPlayerIndex listOfPlayers)
                        ++ (reverse $ drop lastPlayerIndex listOfPlayers)




-- #####################################################################################################################
                        {- This function handles the process of the pot being shared across
                                    the different eligible winners and tiers-}
-- #####################################################################################################################
sharePot :: GameState -> IO GameState
sharePot gameState = do
    let contributionsPerPlayer = [(p, sum $ betsPlaced p) | p <- players gameState]
        potsPerEligiblePlayers = findSidePotsAndEligiblePlayers contributionsPerPlayer

        -- the game state after the pots have been shared across the eligible winners and tiers for the pots
        gameState1 = foldl (\acc (sidePot, contributors) -> sharePot' (sidePot, contributors) acc)
            gameState potsPerEligiblePlayers

        mainPot = (fst.head) potsPerEligiblePlayers
        sidePots = map (fst) $ tail potsPerEligiblePlayers

    putStrLn $ "Main Pot is " ++ (show mainPot) ++ "\nSide Pots are " ++ (show sidePots)
    putStrLn "Sharing the pots among the winner(s) from the eligible players for each pot."

    pure gameState1
    where
        {-
            This function finds the different main or side pots and the players eligible for each main or side pots
        -}
        findSidePotsAndEligiblePlayers :: [(Player, Int)] -> [(Int, [Player])]
        findSidePotsAndEligiblePlayers [] = []
        findSidePotsAndEligiblePlayers contributorsAndContributions =
            (pot, contributors)
                : ((findOtherPots.updateContributions.removeZeros) contributorsAndContributions)
            where
                removeZeros = filter (\(p, c) -> c/=0)
                contributions = (snd.unzip) contributorsAndContributions
                contributors = (fst.unzip) contributorsAndContributions
                potShare = minimum contributions
                pot = (length $ removeZeros contributorsAndContributions)*potShare
                findOtherPots = findSidePotsAndEligiblePlayers.removeZeros
                updateContributions = map (\(p, c) -> (p, c-potShare))


        sharePot' :: (Int, [Player]) -> GameState -> GameState
        sharePot' (sidePot, eligiblePlayers) gameState
            | sidePot == 0 = gameState
            | otherwise =
                let
                    evaluateHands = [evaluateHand $ (hand p)++(community gameState) | p <- eligiblePlayers]
                    playersWithEvaluatedHands = [(p, eh) | (p, eh) <- zip eligiblePlayers evaluateHands]
                    playersWithBestHands = [p | (p, eh) <- playersWithEvaluatedHands, eh==maximum evaluateHands]

                    shareDueToEveryone = div sidePot $ length playersWithBestHands
                    leftOvers = mod sidePot $ length playersWithBestHands
                    closestWinnerToDealer n
                            | name ((players gameState) !! n) `elem` (map (name) playersWithBestHands) =
                                ((players gameState) !! n)
                            | otherwise = closestWinnerToDealer (mod (n-1) $ length $ players gameState)
                    indexOfDealer =
                        case (dealerIndex gameState) of
                            Just i -> i
                    closestWinner = closestWinnerToDealer $ (mod (indexOfDealer-1) $ length $ players gameState)
                    updatedPlayers =
                        map (\p ->
                            if name p `elem` map (name) playersWithBestHands
                                then p{chips=(chips p)+shareDueToEveryone}
                            else p) $ players gameState
                    updatedPlayers' =
                        map (\p ->
                            if name p == name closestWinner
                                then p{chips=(chips p)+leftOvers}
                                    else p) updatedPlayers
                in gameState{players=updatedPlayers'}





-- #####################################################################################################################
        {- This function handles the actual game loop by controlling indirectly what process happen per game
                                         and after a game has ended -}
-- #####################################################################################################################
determineWinner :: IO GameState -> IO GameState
determineWinner ioGameState = do
    gameState <- ioGameState
                                -- The Show down stage
    let activePlayers = [p | p <- players gameState, not(isOutOfTheGame p), (last $ actions p)/=Fold]
        evaluateHands = [evaluateHand $ (hand p)++(community gameState) | p <- activePlayers]
        playersWithEvaluatedHands = [(p, eh) | (p, eh) <- zip activePlayers evaluateHands]
    specialPrint2 "Show Down" $ displayPlayerInfoWithHandRanking playersWithEvaluatedHands

    updatedGameState <- sharePot gameState

    let updatedPlayers = -- resets the players field to prepared for next betting round
            [p{hand=[], betsPlaced=[0, 0, 0, 0], actions=[], isOutOfTheGame=(chips p)==0}
                | p <- players updatedGameState]

    if any (isOutOfTheGame) updatedPlayers
        then putStrLn $ "Players that are out of the game: "++show [p | p <- updatedPlayers, isOutOfTheGame p]
    else
        putStr ""

    -- checks whether there is only active player left,
        -- and if so changes gamesPlayed to 1000 causing the exit of the loop
    case (length $ [p | p <- updatedPlayers, not (isOutOfTheGame p)]) of
        1 -> do
            pure updatedGameState{bets=[0, 0, 0, 0], gamesPlayed=1000, players=updatedPlayers}

        _ -> pure updatedGameState{bets=[0, 0, 0, 0], players=updatedPlayers}
    where
        displayPlayerInfoWithHandRanking :: [(Player, HandRanking)] -> String
        displayPlayerInfoWithHandRanking playersWithRankedHands =
            show $ map (\(p, hr) -> (displayPlayerInfo p, hr)) playersWithRankedHands




-- #####################################################################################################################
        {- This function handles the actual game loop by controlling indirectly what process happen per game
                                         and after a game has ended -}
-- #####################################################################################################################
gameLoop' :: GameState -> IO GameState
gameLoop' changingGameState = do
    let n = gamesPlayed changingGameState
    case n of
        _ | n<100 ->
            do
                specialPrint1 $ "Playing game number " ++ show (n+1)

                {-These chaining of methods take care of all the steps and actions one game of poker has to take-}
                changingGameState' <-
                    determineWinner $
                        bettingRound
                            (fmap
                                (\g-> assignDealerAndBlinds $ dealCards $ Right g{bettingStage=Just PreFlop})
                                    (shuffleDeck.createDeck $ pure changingGameState))

                let updatedGamesPlayed = 1+(gamesPlayed changingGameState')
                gameLoop' changingGameState'{gamesPlayed=updatedGamesPlayed, bets=[0,0,0,0], community=[], pot=0}
          | otherwise -> pure changingGameState




-- #####################################################################################################################
        -- This function takes the final game state and notifies the viewer that the game has ended and which
                                        --- player is the overall winner
-- #####################################################################################################################
endGame :: GameState -> IO()
endGame gameState = do
    specialPrint1 "End of Game"


    let indexOfDealer = -- unwrapping the index of the dealer from Maybe
            case (dealerIndex gameState) of
                Just n -> n
                Nothing -> 0

        orderedPlayers@(p:ps) = -- players in order of their turns
            (reverse $ take indexOfDealer $ players gameState) ++ (reverse $ drop indexOfDealer $ players gameState)

        winner = foldl (\acc p -> if (chips p) > (chips acc) then p else acc) p ps
        -- awards the win to the player with the most chips,
            -- if 2 or more player have the same number of chips the win goes to the player closest to the dealer

    specialPrint2 "Overall Winner" $ displayPlayerInfo winner




-- #####################################################################################################################
                                -- the MAIN function to run the whole program
-- #####################################################################################################################
gameLoop :: IO()
gameLoop = do
    let gameState = createGameState
    endingGameState <- gameLoop' gameState -- this function initialises the actual loop
    endGame endingGameState -- ends the game and prints appropriate info about it

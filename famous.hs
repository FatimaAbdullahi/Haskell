{-
Gustav Nicander, Fatima Abdullahi, Mattias Samuelsson  
-}
import Data.Either ()
import System.IO.Error (tryIOError, isDoesNotExistError)


-- QA: recursive datatype for the questions and answers tree
data QA = Name String
        | Question String QA QA
        deriving (Show, Read)

--function to play again, also updates the QA
playPlayAgain :: QA -> IO QA
playPlayAgain qaIn = do
    qa2 <- play qaIn 
    b <- yesNoQuestion "Play again?"
    if b then do
        qa3 <- playPlayAgain qa2
        return qa3
    else do 
        return qa2 

--function to add new QA's and for pc win
play :: QA -> IO QA
play (Name s) = do
    b <- yesNoQuestion ("Is it " ++ s)
    if b then do 
        putStrLn "I win!"
        return (Name s)
    else do
        n <- question "Who was it?"
        q <- question (
         "Give me a question where the answer for it is yes when thinking of "
         ++ s ++ 
         " and no when thinking of " 
         ++ n
         )
        return (Question q (Name n) (Name s))
--function for moving to next question
play (Question s q1 q2) = do
    b <- yesNoQuestion s
    if b then do
        qa <- play q1
        return (Question s qa q2)
    else do 
        qa <- play q2
        return (Question s q1 qa)

-- asks a question
question :: String -> IO String
question q = do
    putStrLn q 
    a <- getLine
    return a

-- asks a yes no question and doesn't accept an answer that isn't yes or no
yesNoQuestion :: String -> IO Bool
yesNoQuestion q = do
    a <- question q
    if a == "yes" || a == "no"
        then return (a == "yes")
    else do 
        putStrLn "please answer yes or no. "
        yesNoQuestion q

-- main: reads the file famous.qa if it exists otherwise it plays the game with the default tree and creates a new file   
main :: IO ()
main = do 
    e <- tryIOError (readFile "famous.qa")
    f <- either ( \x -> (if isDoesNotExistError x then
                                return (show q1)
                                else error "Main: Unidentified error when reading file"
                                ) 
                                )
                                (return) e
    
    qa <- playPlayAgain (read f)
    writeFile "famous.qa" (show qa)
    return ()

--default questions
q1 :: QA
q1 = (Question "Think of a famous person. \nIs she from Europe?  " q2 q3)

q2 :: QA
q2 = (Question "Is she a scientist? " (Name "Marie Curie") (Name "Queen Elizabeth II"))

q3 :: QA
q3 = (Question "Is she an actress? " (Name "Marilyn Monroe") (Name "Hillary Clinton"))
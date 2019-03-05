{-
    The following program is a game that asks the user to think of a number between 
    one and ten. The program asks a series of yes or no questions to figure out what
    the number is. The program is not very clever.
    
    Your task is to modify this program so that it asks the user to think of a number
    between one and one thousand! Your program should be able to guess the number 
    in only eleven questions or fewer! 
    
    Hint: You can ask questions like, "Is your number greater than 500?"
    Hint: You can change as much of this program as you want. It is only
          here to serve as a simple example.
          
    Here is some sample output (I am thinking of the number 889):

    Think of a number between 1 and 1000 and I will guess it.
    Is your number greater than 500? (answer "yes" or "no") 
    yes
    Is your number greater than 750? (answer "yes" or "no") 
    yes
    Is your number greater than 875? (answer "yes" or "no") 
    yes
    Is your number greater than 938? (answer "yes" or "no") 
    no
    Is your number greater than 907? (answer "yes" or "no") 
    no
    Is your number greater than 891? (answer "yes" or "no") 
    no
    Is your number greater than 883? (answer "yes" or "no") 
    yes
    Is your number greater than 887? (answer "yes" or "no") 
    yes
    Is your number greater than 889? (answer "yes" or "no") 
    no
    Is your number 888? (answer "yes" or "no") 
    no
    Your number is 889, right? (answer "yes" or "no") 
    yes
    I KNEW IT! Thank you.
-}

guessIt :: [Int] -> IO ()
guessIt [] = putStrLn "Wait.. I have already guessed everything! Cheater."
guessIt nums = do
    if length nums > 2
        then do
            let mid = nums !! (quot (length nums) 2 - 1)
            putStrLn ("Is your number greater than " ++ show mid ++ "? (answer \"yes\" or \"no\")")
            answer <- getLine
            if answer == "yes"
                then
                    guessIt [x| x <- nums, x > mid]
                else
                    guessIt [x| x <- nums, x <= mid]
        else do
            if length nums == 2
                then do
                    putStrLn ("Is your number " ++ show (last nums) ++ "? (answer \"yes\" or \"no\")")
                    answer <- getLine
                    if answer == "yes"
                        then do
                            putStrLn "I KNEW IT! Thank you."
                            return ()
                        else
                            guessIt [head nums]
                else do
                    putStrLn ("Your number is " ++ show (last nums) ++ ", right? (answer \"yes\" or \"no\")")
                    answer <- getLine
                    if answer == "yes"
                        then do
                            putStrLn "I KNEW IT! Thank you."
                            return ()
                        else guessIt []

main = do
    putStrLn "Think of a number between 1 and 1000 and I will guess it."
    guessIt [1..1000]

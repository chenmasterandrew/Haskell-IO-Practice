{-
    This program will build on dictionary.hs and wordsToPhone from a previous
    assignment. You can copy your wordsToPhone source code here or you can simply
    include the line:
    
    import PTfuncsyntax
    
    and run this program in the same directory with your PFfuncsyntax.hs file.
    
    This program will ask the user to enter a 4-digit number. It will then list 
    off all of the english words that can be formed from that number on a standard 
    telephone keypad.
    
    Example of use:
    
    *Main> main
    Type a four-digit number:
    2376
    "Afro"
    "Bern"
    "berm"
    *Main> 

-}
charToPhoneDigit :: Char -> Int
charToPhoneDigit c
    | elem c ['a','b','c','A','B','C'] = 2
    | elem c ['d','e','f','D','E','F'] = 3
    | elem c ['g','h','i','G','H','I'] = 4
    | elem c ['j','k','l','J','K','L'] = 5
    | elem c ['m','n','o','M','N','O'] = 6
    | elem c ['p','q','r','s','P','Q','R','S'] = 7
    | elem c ['t','u','v','T','U','V'] = 8
    | elem c ['w','x','y','z','W','X','Y','Z'] = 9
    | otherwise = 0

numListToNum :: [Int] -> Int
numListToNum nums = read (numList nums)::Int
numList :: [Int] -> String
numList [] = []
numList nums = (show (head nums) ++ numList (tail nums))

wordsToPhone :: String -> Int
wordsToPhone s = numListToNum (wordsToPhoneList s)
wordsToPhoneList :: String -> [Int]
wordsToPhoneList s
    | s == [] = []
    | charToPhoneDigit (head s) == 0 = wordsToPhoneList (tail s)
    | otherwise = [charToPhoneDigit (head s)] ++ wordsToPhoneList (tail s)

--printer :: [String] -> IO ()
printer [] = putStrLn ""
printer l = do
    putStrLn (show (head l))
    printer (tail l)

func x n = wordsToPhone x == n

main = do
    dictionary <- readFile "/usr/share/dict/american-english"
    let dict = words dictionary
    putStrLn "Type a four-digit number:"
    num <- readLn
    let wordList = filter (`func` num) (dict)
    printer wordList
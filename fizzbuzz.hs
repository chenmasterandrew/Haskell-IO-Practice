{- 
    Write an interactive program that does the FizzBuzz problem that was in
    an earlier homework assignment. Here is an example of how the program should work:

    How many numbers shall we print? 25
    For multiples of what number shall we print 'Fizz'? 3
    For multiples of what number shall we print 'Buzz'? 5
    1
    2
    Fizz
    4
    Buzz
    Fizz
    7
    8
    Fizz
    Buzz
    11
    Fizz
    13
    14
    FizzBuzz
    16
    17
    Fizz
    19
    Buzz
    Fizz
    22
    23
    Fizz
    Buzz
    
    Tips:
    
    * Example of getting an Integer (as opposed to a String) from the user: 
        numVariable  <- readLn
      (Clarification: It gets whatever data type is required.)
    
    * Use your fizzBuzz function from the lists homework assignment. You can
      copy that code into this file.
      
    * Use mapM_ with putStrLn to print the result of calling fizzBuzz.

-}

-- Copy your fizzBuzz function and supporting functions here
isDivisor n d = if n `mod` d == 0 then True else False
isFizzBuzz n f b = if isDivisor n f && isDivisor n b then "FizzBuzz" else if isDivisor n f then "Fizz" else if isDivisor n b then "Buzz" else show n
fizzBuzz n f b = [isFizzBuzz x f b| x <-[1..n]]

main = do  
    putStrLn "How many numbers shall we print? "
    n <- readLn
    putStrLn "For multiples of what number shall we print 'Fizz'? "
    f <- readLn
    putStrLn "For multiples of what number shall we print 'Buzz'? "
    b <- readLn
    mapM_ putStrLn (fizzBuzz n f b)



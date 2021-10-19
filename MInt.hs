import Data.Char
import Control.Applicative
import System.IO
import Data.List
import Debug.Trace

data Variable = Variable { name :: String,
                           vtype :: String,
                           value :: Int }
                           deriving Show
                           
type Env = [Variable]

updateEnv :: Variable -> Parser String
updateEnv var = P (\env input -> case input of
                     xs -> [((modifyEnv env var),"",xs)])

modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar = if (name x) == (name newVar) then
                               [newVar] ++ xs
                          else
                               [x] ++ modifyEnv xs newVar
                               
-- Return the value of a variable given the name
readVariable :: String -> Parser Int
readVariable name = P (\env input -> case searchVariable env name of
                                          [] -> []
                                          [value] -> [(env, value, input)])
                                          
-- Search the value of a variable stored in the Env. given the name
searchVariable :: Env -> String -> [Int]
searchVariable [] queryname = []
searchVariable (x:xs) queryname = if (name x) == queryname
                                       then [(value x)]
                                       else
                                            searchVariable xs queryname

-- Defining functions to work on array

saveArray :: String -> [Int] -> Parser String
saveArray var val = P(\env input -> [(updateArray env var val, "", input)])

updateArray :: Env -> String -> [Int] -> Env
updateArray env var val = foldl (modifyEnv) env l
                          where l = zipWith (\a i ->
                                   Variable { name=var ++ "{" ++ (show i) ++ "}"
                                            , vtype="array"
                                            , value= a}) val [0..]


searchArray :: Env -> String -> [Int]
searchArray env array =
     case searchVariable env x of
          []    -> []
          value -> concat([value] ++ map (\var -> searchVariable env var) xs)
     where (x:xs) = map (\i -> (array ++ "{" ++ (show i) ++ "}")) [0..l]
           l = countElem env
           countElem [] = 0
           countElem (x:xs) = if (array ++ "{") `isPrefixOf` (name x)
                                then 1 + countElem xs
                                else countElem xs
                                             
            
                                                 
readArray :: String -> Parser [Int]
readArray name = P(\env input -> case searchArray env name of
                    [] -> []
                    value -> [(env, value, input)])
                    
                    
                                            

-- Defining the type Parser                                            

newtype Parser a = P (Env -> String -> [(Env,a,String)])

parse :: Parser a -> Env -> String -> [(Env,a,String)]
parse (P p) env inp = p env inp
 
instance Functor Parser where
 fmap g p = P (\env inp -> case parse p env inp of
  [] -> []
  [(env,v,out)] -> [(env,g v, out)])
  
instance Applicative Parser where
 pure v = P (\env inp -> [(env,v,inp)])
 pg <*> px = P (\env inp -> case parse pg env inp of
  [] -> []
  [(env,g,out)] -> parse (fmap g px) env out)
  
instance Monad Parser where
 return v = P (\env inp -> [(env,v,inp)])
 p >>= f = P (\env inp -> case parse p env inp of
  [] -> []
  [(env,v,out)] -> parse (f v) env out)
  
 
instance Alternative Parser where
 empty = P (\env inp -> [])
 p <|> q = P (\env inp -> case parse p env inp of
  [] -> parse q env inp
  [(env,v,out)] -> [(env,v,out)])



-- Defining Parser to handle input


item :: Parser Char
item = P (\env inp -> case inp of 
 [] -> []
 (x:xs) -> [(env,x,xs)])

satisfies :: (Char -> Bool) -> Parser Char
satisfies p =  do x <- item
                  if p x
                   then return (x)
                   else empty
 
                   
removeSpaces :: Parser ()
removeSpaces = 
 do {
    many (satisfies isSpace);
    return ();
    }


digit :: Parser Int
digit = do x <- satisfies isDigit
           return $ digitToInt x

lower :: Parser Char
lower = satisfies isLower

upper :: Parser Char
upper = satisfies isUpper

letter :: Parser Char
letter = satisfies isAlpha

integer :: Parser Int
integer = (do symbol "-"
              n <- natural
              return (-n))
          <|>
              natural
              
natural :: Parser Int
natural = (do d <- digit
              n <- natural
              return (read (show d ++ show n) ::Int))
          <|>
           digit

alphaNum :: Parser Char
alphaNum = satisfies isAlphaNum

identifier :: Parser String
identifier = do {
                 removeSpaces;
                 x <- lower;
                 xs <- many alphaNum;
                 removeSpaces;
                 return (x:xs);
                }

symbol :: String -> Parser String
symbol [] = return ""
symbol (x:xs) = do satisfies (x ==)
                   symbol xs
                   return (x:xs)
                   
                   
-- Defining Parser for Aexp without evaluating

parseAexp :: Parser String
parseAexp = (do
                t <- parseAterm
                symbol "+"
                e <- parseAexp
                return (t ++ "+" ++ e))
           <|>
            (do
                t <- parseAterm
                symbol "-"
                e <- parseAexp
                return (t ++ "-" ++ e))
           <|>
            parseAterm
            
parseFactor :: Parser String
parseFactor = 
  do {
  symbol "(";
  e <- parseAexp;
  symbol ")";
  return ("(" ++ e ++ ")");
  }
  <|>
  do {
   symbol "-";
   f <- parseFactor;
   return ("-" ++ f);
  }
  <|>
  do {
   i <- identifier;
   symbol "{";
   index <- parseAexp;
   symbol "}";
   return $ i ++ "{" ++ index ++ "}";
   }
  <|>
  do {
   i <- identifier;
   return i;
  }
  <|> 
  do {
   i <- integer;
   return (show i);
  }
 
parseAterm :: Parser String
parseAterm =
 do {
  f <- parseFactor;
  do {
   symbol "*";
   t <- parseAterm;
   return (f ++ "*" ++ t);
  }
  <|>
  do {
   symbol "/";
   t <- parseAterm;
   return (f ++ "/" ++ t);
  }
  <|> 
  return f
  }
  
-- Defining Parser for Bexp without evaluating

parseBexp :: Parser String
parseBexp =
 do {
  p1 <- parseBexp2;
  symbol "||";
  p2 <- parseBexp;
  return (p1 ++ "||" ++ p2);
 }
 <|>
 do {
  p <- parseBexp2;
  return p;
 }

parseBexp2 :: Parser String
parseBexp2 =
 do {
  p1 <- parseBexp3;
  symbol "&&";
  p2 <- parseBexp2;
  return (p1 ++ "&&" ++ p2);
 }
 <|>
 do {
  p <- parseBexp3;
  return p;
 }
 
parseBexp3 :: Parser String
parseBexp3 =
 do {
  symbol "(";
  p <- parseBexp;
  symbol ")";
  return ("(" ++ p ++ ")");
 }
 <|>
 do {
  c <- parseCompareTo;
  return c;}
 <|>
 do {
  symbol "True";
  return "True";
 }
 <|>
 do {
  symbol "False";
  return "False";
 }
 <|>
 do {
  symbol "!";
  p <- parseBexp3;
  return ("!" ++ p);  
 }
 
 
-- compareTo parser

parseCompareTo :: Parser String
parseCompareTo =
 do {
  a1 <- parseAexp;
  symbol "<";
  a2 <- parseAexp;
  return (a1 ++ "<" ++ a2);
 }
 <|>
 do {
  a1 <- parseAexp;
  symbol ">";
  a2 <- parseAexp;
  return (a1 ++ ">" ++ a2);
 }
 <|>
 do {
  a1 <- parseAexp;
  symbol ">=";
  a2 <- parseAexp;
  return (a1 ++ ">=" ++ a2);
 }
 <|>
 do {
  a1 <- parseAexp;
  symbol "<=";
  a2 <- parseAexp;
  return (a1 ++ "<=" ++ a2);
 }
 <|>
 do {
  a1 <- parseAexp;
  symbol "==";
  a2 <- parseAexp;
  return (a1 ++ "==" ++ a2);
 }
 <|>
 do {
  a1 <- parseAexp;
  symbol "!=";
  a2 <- parseAexp;
  return (a1 ++ "!=" ++ a2);
 }                   


-- Defining Parser for Commands without evaluating

parseCommand :: Parser String
parseCommand = parseAssignment <|> parseIfThenElse <|> parseWhile
          <|> symbol "skip"

parseProgram :: Parser String
parseProgram = (do x <- parseCommand
                   symbol ";"
                   y <- parseProgram
                   return $ x ++ ";" ++ y) <|>
          (do x <- parseCommand
              symbol ";"
              return $ x ++ ";")
          <|> parseCommand


parseAssignment :: Parser String
parseAssignment = -- y = x{1}
              (do id <- identifier
                  symbol "="
                  id2 <- identifier
                  symbol "{"
                  index <- parseAexp
                  symbol "}"
                  return $ id ++ "=" ++ id2 ++ "{" ++ index ++ "}")
              <|>
              -- x{1} = y{1}
              (do id <- identifier
                  symbol "{"
                  index <- parseAexp
                  symbol "}"
                  symbol "="
                  id2 <- identifier
                  symbol "{"
                  index2 <- parseAexp
                  symbol "}"
                  return $ id ++ "{" ++ index ++ "}=" ++ id2 ++ "{" ++ index2 ++ "}" )
              <|>
                  (do id <- identifier
                      symbol "="
                      a <- parseAexp
                      return $ id ++ "=" ++ a)
              <|>
            -- x{1} = y
              (do id <- identifier
                  symbol "{"
                  index <- parseAexp
                  symbol "}"
                  symbol "="
                  val <- parseAexp
                  array <- readArray id
                  return $ id ++ "{" ++ index ++ "}=" ++ val )
            <|>
              -- x={1,2,3}
              (do id <- identifier
                  symbol "="
                  arr <- parseArray
                  return $ id ++ "=" ++ arr )
             <|>
              -- x = y++z
              (do id <- identifier
                  symbol "="
                  ar1 <- parseArray
                  symbol "++"
                  ar2 <- parseArray
                  return $ id ++ "=" ++ ar1 ++ "++" ++ ar2)  

parseIfThenElse :: Parser String
parseIfThenElse = do symbol "if "
                     b <- parseBexp
                     symbol " then "
                     x <- parseProgram
                     symbol "else "
                     y <- parseProgram
                     symbol "endif"
                     return $ "if " ++ b ++ " then " ++ x ++ "else " ++ y ++ "endif"

parseWhile :: Parser String
parseWhile = do symbol "while "
                b <- parseBexp
                symbol " do "
                x <- parseProgram
                symbol "end"
                return $ "while " ++ b ++ " do " ++ x ++ "end"


-- Defining Parser for Aexp
aexp :: Parser Int
aexp = 
 (do 
   t <- aterm
   symbol "+"
   e <- aexp
   return (t + e))   
  <|>
  (do 
     t <- aterm
     symbol "-";
     e <- aexp;
     return (t - e))
  <|>
   aterm;
 
aterm :: Parser Int
aterm = 
 do {
  f <- factor;
  t <- aterm1 f;
  return (t);
 }

factor :: Parser Int
factor = 
 do {
 symbol "(";
 e <- aexp;
 symbol ")";
 return e;
 }
 <|>
 do {
  i <- identifier;
  symbol "{";
  index <- aexp;
  symbol "}";
  readVariable $ i ++ "{" ++ (show index) ++ "}";
  }
 <|> 
 do {
  i <- identifier;
  readVariable i;
 }
 <|>
 integer
 
aterm1 :: Int -> Parser Int;
aterm1 t1 =
 do {
  symbol "*";
  f <- factor;
  t <- aterm1 (t1 * f);
  return (t);
 }
 <|>
 do {
  symbol "/";
  f <- factor;
  t <- aterm1 (div t1 f);
  return (t);
 }
 <|>
 return t1;
           
 
                     
assignment :: Parser String
assignment = 
              -- x = y++z
              (do id <- identifier
                  symbol "="
                  ar1 <- array
                  symbol "++"
                  ar2 <- array
                  saveArray id (ar1 ++ ar2))
            <|>
              (do id <- identifier
                  symbol "="
                  a <- aexp
                  updateEnv Variable{name = id, vtype="int", value=a})
            <|>
            -- x{1} = y
              (do id <- identifier
                  symbol "{"
                  index <- aexp
                  symbol "}"
                  symbol "="
                  val <- aexp
                  array <- readArray id
                  if length array <= index
                  then empty
                  else updateEnv Variable{ name= (id ++ "{" ++ (show index) ++ "}")
                                         , vtype="array"
                                         , value=val
                                         })
            <|>
            -- y = x{1}
              (do id <- identifier
                  symbol "="
                  id2 <- identifier
                  symbol "{"
                  index <- aexp
                  symbol "}"
                  val <- readVariable (id2 ++ "{" ++ (show index) ++ "}")
                  updateEnv Variable{name = id, vtype="int", value=val})
            <|>
            -- x{1} = y{1}
              (do id <- identifier
                  symbol "{"
                  index <- aexp
                  symbol "}"
                  symbol "="
                  id2 <- identifier
                  symbol "{"
                  index2 <- aexp
                  symbol "}"
                  val <- readVariable (id2 ++ "{" ++ (show index2) ++ "}") 
                  updateEnv Variable{name = (id ++ "{" ++ (show index) ++ "}"), vtype="array", value=val})
              <|>
              -- x={1,2,3}
              (do id <- identifier
                  symbol "="
                  arr <- array
                  saveArray id arr)
            
--  Defining Parser for Bexp
bexp :: Parser Bool
bexp = ( do  bt <- bterm
             symbol "OR"
             b1 <- bexp
             return ( bt || b1))
         <|>
         bterm
         
bterm :: Parser Bool
bterm = ( do bf <- bfactor
             symbol "AND"
             bt <- bterm
             return (bf && bt))
         <|>
         bfactor

bfactor :: Parser Bool
bfactor = (do symbol "!"
              b <- bfactor
              return $ not b)
          <|>
          (do symbol "("
              b <- bexp
              symbol ")"
              return b)
          <|>
          (do symbol "True"
              return True)
          <|>
          (do symbol "False"
              return False)
          <|> bcomparison
          
bcomparison :: Parser Bool
bcomparison = (do a <- aexp
                  symbol "=="
                  b <- aexp
                  return $ a == b)
              <|>
              (do a <- aexp
                  symbol "<="
                  b <- aexp
                  return $ a <= b)
               <|>
              (do a <- aexp
                  symbol ">="
                  b <- aexp
                  return $ a >= b)
               <|>
              (do a <- aexp
                  symbol ">"
                  b <- aexp
                  return $ a > b)
              <|>
              (do a <- aexp
                  symbol "<"
                  b <- aexp
                  return $ a < b)             
                 
-- Defining Parser for Commands

program :: Parser String
program = (do command
              symbol ";"
              program)
          <|>
          (do command
              symbol ";")
          <|>
              command
              
command :: Parser String
command = assignment
          <|>
          ifThenElse
          <|>
          while
          <|>
          symbol "skip"
              

                  
ifThenElse :: Parser String
ifThenElse = (do symbol "if "
                 b <- bexp
                 symbol " then "
                 if(b) then
                            (do  program
                                 symbol "else " 
                                 parseProgram
                                 symbol "endif"
                                 return "")
                 else
                      (do parseProgram
                          symbol "else "
                          program
                          symbol "endif"
                          return ""))
                      

executeWhile :: String -> Parser String
executeWhile c = P(\env input -> [(env, "", c ++ input)])

while :: Parser String
while = (do w <- parseWhile
            executeWhile w
            symbol "while "
            b <- bexp
            symbol " do "
            if (b) then
                 (do program
                     symbol "end"
                     executeWhile w
                     while)
            else
                 (do parseProgram
                     symbol "end"
                     return ""))
                     
                     
                     
eval :: String -> Env
eval c = case parse program [] c of
             []          -> error "Invalid input"
             [(e, _, [])]  -> e
             [(e, _, out)] -> error $ "Invalid input: unused '" ++ out ++ "'"
             

-- Defining Vectors
-- x = {1,2,3,4}
-- x{1} = 2 -- access from index
-- concatenate 2 arrays
-- x = y -- save an array into another
-- y = x{i} + y -- use an element of an array as a factor


parseConcArray :: Parser String
parseConcArray = (do a <- parseArray
                     symbol "++"
                     b <- parseConcArray
                     return ( a ++ b))
                 <|>
                     parseArray 



parseArrayItems :: Parser String
parseArrayItems = (do a <- parseAexp
                      symbol ","
                      b <- parseArrayItems
                      return (a ++ "," ++ b))
                <|> parseAexp

parseArray :: Parser String
parseArray = (do symbol "{"
                 a <- parseArrayItems
                 symbol "}"
                 return ("{" ++ a ++ "}"))
              <|> identifier
                 
                  
concArray :: Parser [Int]
concArray = (do a <- array
                symbol "++"
                b <- concArray
                symbol ";"
                return (a ++ b))
            <|> array
            
arrayItems :: Parser [Int]
arrayItems = (do a <- aexp
                 symbol ","
                 as <- arrayItems
                 return ([a] ++ as))
             <|>
             (do a <- aexp
                 return [a])
                              
                 

array :: Parser [Int]
array =  (do symbol "{"
             a <- arrayItems
             symbol "}"
             return a)
          <|>
          do i <- identifier
             readArray i   
             
logo :: IO String
logo = do {
 putStrLn "     $$\\      $$\\ $$$$$$\\            $$\\ ";     
 putStrLn "     $$$\\    $$$ |\\_$$  _|           $$ |";    
 putStrLn "     $$$$\\  $$$$ |  $$ |  $$$$$$$\\ $$$$$$\\ ";   
 putStrLn "     $$\\$$\\$$ $$ |  $$ |  $$  __$$\\\\_$$  _|";  
 putStrLn "     $$ \\$$$  $$ |  $$ |  $$ |  $$ | $$ |";    
 putStrLn "     $$ |\\$  /$$ |  $$ |  $$ |  $$ | $$ |$$\\ "; 
 putStrLn "     $$ | \\_/ $$ |$$$$$$\\ $$ |  $$ | \\$$$$  |";
 putStrLn "     \\__|     \\__|\\______|\\__|  \\__|  \\____/"; 
 putStrLn "";                                      
 putStrLn "          Montanaro Graziano Interpreter";
 putStrLn "";                                     
 putStrLn "Please type the code to be evaluated, or type 'exit'\nto quit.";
 menu;
}

menu :: IO String
menu = do {
 putStr "MInt> ";
 hFlush stdout;
 input <- getLine;
 
 if (input == "exit") then return "Bye!"
 else do {
  print (eval input);
  menu;
 }
}

main = logo;             

             

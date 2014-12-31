import Parser
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Applicative
import Compile

main :: IO ()
main = do
    (file:_) <- getArgs
    program <- getLines file
    case parseFile program of
        Right ss -> do
            --putStrLn preamble
            print ss
            putStrLn $ code $ foldl statement initialBuffer ss
            --putStrLn postamble
        Left err -> putStrLn err

parseFile :: String -> Either String [Statement]
parseFile file = case parse statements "" file of
    Right ss -> Right ss
    Left e -> Left $ "Error: " ++ show e

getLines :: FilePath -> IO String
getLines file = concat . lines <$> readFile file

import Parser
import Compile
import Text.ParserCombinators.Parsec (parse)
import System.Environment (getArgs)
import Control.Applicative ((<$>))

main :: IO ()
main = do
    (file:_) <- getArgs
    program <- getLines file
    case parseFile program of
        Right ss -> do
            putStrLn preamble
            putStrLn $ code $ foldl statement compiler ss
            putStrLn postamble
        Left err -> putStrLn err

parseFile :: String -> Either String [Statement]
parseFile file = case parse statements "" file of
    Right ss -> Right ss
    Left e -> Left $ "Error: " ++ show e

getLines :: FilePath -> IO String
getLines file = concat . lines <$> readFile file

module Lectures.FfthLecture (mainTool) where

import           Control.Applicative      ((<**>))
import           Options.Applicative      (Parser, ParserInfo, argument, auto,
                                           execParser, fullDesc, header, help,
                                           helper, info, long, metavar, option,
                                           progDesc, short, value)
import           System.Environment       (getArgs)
import           System.Environment.Blank (getProgName)

import           Lectures.ScndLecture     (solveSquare)

data Language = En | Fr
  deriving (Show, Read)

data Options = Options
  { a,b,c    :: Double
  , language :: Language
  }

-- | Solve equation of kind @ax² + bx + c@ within a terminal representation
mainTool :: IO ()
mainTool = do
  Options{a = a, b = b, c = c, language = language} <- execParser parserInfo
  let solution =
        case language of
          En -> "Solution"
          Fr -> "La solution"
  putStrLn $ solution ++ ": " ++ show (solveSquare a b c)

parserInfo :: ParserInfo Options
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <> header "Solve square equation")

parser :: Parser Options
parser =
  Options
  <$> argument auto (metavar "A" <> help "x² coefficient")
  <*> argument auto (metavar "B" <> help "x coefficient")
  <*> argument auto (metavar "C" <> help "free coefficient")
  <*> option   auto (   metavar "LANGUAGE"
                    <>  help "Language: En (default), Fr"
                    <>  value En
                    <>  long "language"
                    <>  short 'l'
                    )

-- argument и option - это парсеры

-- Есть два типа программных элементов:
-- • функция    - это вычисление значений 
-- • действие   - это эффект

-- <$> - fmap

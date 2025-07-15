module Markup(Document, Structure(..)) where
import GHC.Natural ( Natural )
import Data.Maybe

type Document =
    [Structure]

data Structure 
    = Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]
    deriving (Show)

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
    case txts of 
        -- Done
        [] -> maybeToList context

        --Heading 1
        ('*' : ' ': line) :rest -> maybe id (:) context (Heading 1 (trim  line) : parseLines Nothing rest)

        -- Unordered list
        ('-' : ' ': line) :rest -> 
            case context of
                Just (UnorderedList list) -> parseLines (Just (UnorderedList (list <> [trim line]))) rest
                _ -> maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest) 

        -- Ordered list
        ('#' : ' ': line) :rest -> 
            case context of
                Just (OrderedList list) -> parseLines (Just (OrderedList (list <> [trim line]))) rest
                _ -> maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest) 
        
        -- Code block
        ('>' : ' ': line) :rest -> 
            case context of
                Just (CodeBlock list) -> parseLines (Just (CodeBlock (list <> [line]))) rest
                _ -> maybe id (:) context (parseLines (Just (CodeBlock [line])) rest) 
        
        --Paragraph
        currentLine : rest -> 
            let line = trim currentLine 
            in  if line == ""
                    then case context of 
                        Nothing -> parseLines Nothing rest
                        Just structure -> structure : parseLines Nothing rest
                    else   case context of
                        Just (Paragraph paragraph) -> parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
                        Nothing -> maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
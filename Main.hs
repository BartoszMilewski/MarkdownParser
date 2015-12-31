import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Language

import Data.List

data Document = Document [Block] [Section]
   deriving Show

data Section = Section Heading [Block]
   deriving Show

data Heading = Heading Int String
   deriving Show

data Block = Para [MdString]
           | Listing String [String]
           | List [ListItem]
           deriving Show

-- should nest
data ListItem = Item [MdString]
    deriving Show

data MdString = Code String
              | Link String String
              | Image String String
              | Emph Int String
              | Text String
              deriving Show

restOfLine :: GenParser Char st String
restOfLine = many (noneOf "\n")

document :: GenParser Char st Document
document = do
    intro <- many block
    sects <- many section
    eof
    return $ Document intro sects

section :: GenParser Char st Section
section = do
    hd <- heading
    blocks <- many block
    return $ Section hd blocks

heading :: GenParser Char st Heading
heading = do 
    pre <- many1 (char '#')
    title <- restOfLine
    newline
    return $ Heading (length pre) title

block :: GenParser Char st Block
block = try listing <|> list <|> para

listing :: GenParser Char st Block
listing = do
   fence
   hd <- line
   lines <- linesOfCode
   return $ Listing hd lines
   
fence :: GenParser Char st String
fence = string "```"

linesOfCode :: GenParser Char st [String]
linesOfCode = do 
    try fence 
    return []
  <|> do
    ln <- line 
    rest <- linesOfCode
    return (ln : rest)

line = try controlLine <|> validLine 

validLine = do
    str <- restOfLine
    newline
    return str

controlLine = do
    string "--"
    spaces
    string "show" <|> string "/show"
    restOfLine
    newline
    return []

list :: GenParser Char st Block
list = do 
    items <- many1 listItem
    return $ List items

-- should nest
listItem = do
   itemStart
   str <- many mdstring
   char '\n'
   return $ Item str

itemStart :: GenParser Char st ()
itemStart = do
    many1 $ oneOf "0123456789"
    option ' ' $ char '.'
    spaces
    return ()

para :: GenParser Char st Block
para = do
    line <- many mdstring
    char '\n'
    return $ Para line

mdstring :: GenParser Char st MdString
mdstring = try image <|> try link <|> emph <|> code <|> text

text :: GenParser Char st MdString
text = do
    tx <- many1 $ noneOf "\n`*#"
    return $ Text tx

image = do
    char '!'
    (Link title url) <- link
    return $ Image title url
    
link :: GenParser Char st MdString
link = do
    char '['
    title <- many $ noneOf "]\n"
    char ']'
    char '('
    url <- many $ noneOf ")\n"
    char ')'
    return $ Link title url

emph = do
    beg <- many1 $ char '*'
    tx <- many $ noneOf "*\n"
    string beg
    return $ Emph (length beg) tx

code = do
    char '`'
    tx <- many $ noneOf "`\n"
    char '`' <?> "a backtick closing inline code \"" ++ tx ++ "\""
    return $ Code tx

-- translation

transDoc :: Document -> String
transDoc (Document blocks sections) = transBlocks blocks ++ concat (fmap transSection sections)


transSection (Section hd blocks) = transHeading hd ++ transBlocks blocks
transHeading (Heading n str) =
    "<h" ++ show level ++ ">" ++ str ++ "</h" ++ show level ++ ">\n"
  where
    level = n - 1

transBlocks :: [Block] -> String
transBlocks blocks = concat $ intersperse "\n" $ fmap transBlock blocks

transBlock (Para mdstrs) = concat $ fmap transMdstr mdstrs

transBlock (Listing _ lines) = "<pre>" ++ concat (intersperse "\n" lines) ++ "\n</pre>"

transBlock (List items) = 
    "<ol>\n" ++ concat (fmap transListItem items) ++ "</ol>\n"

transBlock _ = "block"

transListItem (Item mdstrs) = 
    "  <li>" ++ concat (fmap transMdstr mdstrs) ++ "</li>\n"

transMdstr (Text str) = str
transMdstr (Code str) = "<code>" ++ str ++ "</code>"
transMdstr (Emph 1 str) = "<em>" ++ str ++ "</em>"
transMdstr (Emph 2 str) = "<b>" ++ str ++ "</b>"
transMdstr (Emph _ str) = error $ "Overemphasis of " ++ "str"
transMdstr (Link title url) = "<a href=\"" ++ url ++ "\">" ++ title ++ "</a>"
transMdstr (Image title url) = 
    "[caption width=\"632\" align=\"aligncenter\"]"  ++
    "<img src=\"" ++ url ++ "\" alt=\"" ++ title ++ "\" />" ++ 
    title ++ "[/caption]"

transMdstr _ = "mdstr"

main = do
    file <- readFile "in.md"
    case parse document "Syntax" file of
        Left err -> putStrLn $ "Error: " ++ show err
        Right doc -> putStr $ transDoc doc

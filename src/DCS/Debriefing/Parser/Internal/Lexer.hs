module DCS.Debriefing.Parser.Internal.Lexer where

import Control.Monad (void)
import Data.Char (isAlphaNum)
import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)
import Data.Maybe (listToMaybe)
import Data.Scientific (FPFormat (..), Scientific, formatScientific)
import Data.Text (Text)
import Data.Text.IO as T (readFile)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.HashMap.Strict qualified as HM
import Data.IntMap qualified as IM
import Data.Text qualified as T
import Text.Megaparsec.Char.Lexer qualified as L


lexerHarness :: FilePath -> IO (Either String RawDebriefing)
lexerHarness fp = do
    input <- T.readFile fp
    pure $ case parse @Void parseRawDebriefing fp input of
        Left e ->
            Left $ errorBundlePretty e
        Right r ->
            Right r


data RawDebriefing = RawDebriefing [Variable]
    deriving stock (Show, Eq)


parseRawDebriefing :: (MonadParsec e Text m) => m RawDebriefing
parseRawDebriefing = RawDebriefing <$> some parseVariable


-- TODO: should really be called "assignment", "global value", or something
-- - doesn't change so not a variable...
data Variable
    = Variable
    { varName :: Text
    , varValue :: RawValue
    }
    deriving stock (Show, Eq)


parseVariable :: (MonadParsec e Text m) => m Variable
parseVariable = do
    varName <- parseVariableName <?> "top-level variable name"
    void $ symbol "="
    varValue <- parseRawValue <?> "variable value"
    pure Variable {..}


parseVariableName :: (MonadParsec e Text m) => m Text
parseVariableName = lexeme (takeWhile1P Nothing (\c -> isAlphaNum c || c == '_')) <?> "letter or underscore"


parseKeyName :: (MonadParsec e Text m) => m Text
parseKeyName =
    choice
        [ char '"' *> lexeme (takeWhile1P Nothing (/= '"')) <* char '"'
        , parseVariableName
        ]


data RawValue
    = String Text
    | Boolean Bool
    | Number Scientific
    | Array [RawValue]
    | SparseArray (IntMap RawValue)
    | Map (HashMap Text RawValue)
    deriving stock (Show, Eq)


displayRawValue :: RawValue -> Text
displayRawValue = \case
    String s ->
        s
    Boolean b ->
        if b then "true" else "false"
    Number n ->
        T.pack $ formatScientific Generic Nothing n
    Array xs ->
        "[" <> T.intercalate ", " (map displayRawValue xs) <> "]"
    SparseArray im ->
        "["
            <> T.intercalate
                ", "
                ( ( \(ix, v) ->
                        "[" <> T.pack (show ix) <> "] => " <> displayRawValue v
                  )
                    <$> IM.assocs im
                )
            <> "]"
    Map hm ->
        "{"
            <> T.intercalate
                ", "
                ( ( \(k, v) ->
                        "\"" <> k <> "\": " <> displayRawValue v
                  )
                    <$> HM.toList hm
                )
            <> "}"


parseRawValue :: forall e m. (MonadParsec e Text m) => m RawValue
parseRawValue =
    lexeme $
        choice
            [ Number <$> L.signed spaceConsumer L.scientific
            , (Boolean True <$ symbol "true") <|> (Boolean False <$ symbol "false")
            , String <$> parseString
            , -- TODO: can ditch the trys here by just parsing the {[" by hand to
              -- determine which branch to follow
              try parseArray
            , try parseArrayList
            , Map <$> try parseMap
            ]
  where
    parseString :: m Text
    parseString =
        betweenQuotes (takeWhileP (Just "non double-quote") (/= '"'))
    -- Array format of @[<int>] = <val>@ can be either in order arrays or
    -- sparse arrays. So we have to determine which kind of array we have.
    --
    -- Not sure why there is also a separate sparse array format of
    -- @["<int>"] = <val>@. Maybe those inner values aren't always ints.
    parseArray :: m RawValue
    parseArray = do
        ixVals <- betweenBraces $ commaList $ do
            ix <- betweenBrackets L.decimal <?> "array index"
            void $ symbol "="
            val <- parseRawValue <?> "array value"
            pure (ix, val)
        case ixVals of
            [] -> pure $ Array []
            (ix, v) : rest -> case listToMaybe $ reverse rest of
                Nothing ->
                    if ix == 1
                        then
                            pure $ Array [v]
                        else
                            pure $ SparseArray $ IM.singleton ix v
                Just (ixLast, _) ->
                    if ix == 1 && (ixLast - ix + 1 == length ixVals)
                        then
                            pure $ Array $ map snd ixVals
                        else
                            pure $ SparseArray $ IM.fromList ixVals
    -- Another format of arrays is simply @{ <value>, <value> }@
    parseArrayList :: m RawValue
    parseArrayList = do
        fmap Array . betweenBraces $ commaList $ (parseRawValue <?> "array value")
    -- Usually maps are encoded in an array-key format:
    --
    -- @["<key>"] = <value>@
    --
    -- And sometimes in a simpler @<key> = <value>@ format.
    --
    -- But sometimes both are used in the same map.
    parseMap :: m (HashMap Text RawValue)
    parseMap = fmap HM.fromList . betweenBraces $ commaList $ do
        key <- choice [betweenBrackets parseKeyName, parseKeyName] <?> "field key"
        void $ symbol "="
        val <- parseRawValue <?> "field value value"
        pure (key, val)


-- Helpers

spaceConsumer :: (MonadParsec e Text m) => m ()
spaceConsumer = L.space space1 (L.skipLineComment "--") (failure Nothing mempty)


lexeme :: (MonadParsec e Text m) => m a -> m a
lexeme = L.lexeme spaceConsumer


symbol :: (MonadParsec e Text m) => Text -> m Text
symbol = L.symbol spaceConsumer


commaList :: (MonadParsec e Text m) => m a -> m [a]
commaList = flip sepEndBy (symbol ",")


betweenBraces :: (MonadParsec e Text m) => m a -> m a
betweenBraces = between (symbol "{") (symbol "}")


betweenBrackets :: (MonadParsec e Text m) => m a -> m a
betweenBrackets = between (symbol "[") (symbol "]")


betweenQuotes :: (MonadParsec e Text m) => m a -> m a
betweenQuotes = between (symbol "\"") (symbol "\"")

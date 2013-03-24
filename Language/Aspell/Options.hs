module Language.Aspell.Options (
    ACOption(..),
    WordListSize(..),
    Encoding(..),
    SuggestMode(..),
    NormalizeForm(..)
) where

import Data.ByteString

data WordListSize = Tiny
                  | ReallySmall
                  | Small
                  | MediumSmall
                  | Medium
                  | MediumLarge
                  | Large
                  | Huge
                  | Insane

data Encoding = UTF8 | Latin1

data SuggestMode = Ultra
                 | Fast
                 | Normal
                 | Slow
                 | BadSpellers

data NormalizeForm = None
                   | NFD
                   | NFC
                   | Composed

data ACOption = MasterDict ByteString
              | WordListDir ByteString
              | Lang ByteString
              | Size WordListSize
              | PersonalWordList ByteString
              | ReplacementsList ByteString
              | Encoding Encoding
              | Normalize Bool
              | NormalizeStrict Bool
              | NormalizeForm NormalizeForm
              | NormalizeRequired Bool
              | Ignore Integer
              | IgnoreReplace Bool
              | SaveReplace Bool
              | KeyboardDef ByteString
              | SuggestMode SuggestMode
              | IgnoreCase Bool
              | IgnoreAccents Bool
              | FilterMode ByteString
              | EmailMargin Integer
              | TeXCheckComments Bool
              | ContextVisibleFirst Bool
              | RunTogether Bool
              | RunTogetherLimit Integer
              | RunTogetherMin Integer
              | MainConfig ByteString
              | MainConfigDir ByteString
              | DataDir ByteString
              | LocalDataDir ByteString
              | HomeDir ByteString
              | PersonalConfig ByteString
              | Layout ByteString
              | Prefix ByteString
              | SetPrefix Bool
              | MakeBackup Bool
              | Time Bool
              | ByteOffsets Bool
              | Reverse Bool
              | KeyMapping ByteString
              | Guess Bool
              | Suggest Bool

module Language.Aspell (
    -- * Constructors
    SpellChecker,
    spellChecker,
    spellCheckerWithOptions,
    spellCheckerWithDictionary,

    -- * Using the spell checker
    check,
    suggest
) where

import Data.ByteString.Char8
import Foreign
#if !MIN_VERSION_base(4,7,0)
     hiding (unsafePerformIO)
#endif
import Foreign.C.String
import Foreign.C.Types
import Language.Aspell.Options
import System.IO.Unsafe

type AspellConfig = ForeignPtr ()
type SpellChecker = ForeignPtr ()

type CAspellConfig = Ptr ()
type CSpellChecker = Ptr ()
type CAspellCanHaveError = Ptr ()
type CWordList = Ptr ()
type CStringEnumeration = Ptr ()

newConfig :: IO AspellConfig
newConfig = do
    cf <- new_aspell_config
    newForeignPtr delete_aspell_config cf

setOpts :: [ACOption] -> AspellConfig -> IO AspellConfig
setOpts (Dictionary dict:opts) pt = setOpt "master" dict pt >>= setOpts opts
setOpts (WordListDir dir:opts) pt = setOpt "dict-dir" dir pt >>= setOpts opts
setOpts (Lang lang:opts) pt = setOpt "lang" lang pt >>= setOpts opts
setOpts (Size size:opts) pt = setOpt "size" newSize pt >>= setOpts opts
    where
        newSize = case size of
                       Tiny -> "+10"
                       ReallySmall -> "+20"
                       Small -> "+30"
                       MediumSmall -> "+40"
                       Medium -> "+50"
                       MediumLarge -> "+60"
                       Large -> "+70"
                       Huge -> "+80"
                       Insane -> "+90"
setOpts (PersonalWordList wl:opts) pt = setOpt "personal" wl pt >>= setOpts opts
setOpts (ReplacementsList rl:opts) pt = setOpt "repl" rl pt >>= setOpts opts
setOpts (Encoding encoding:opts) pt = setOpt "encoding" enc pt >>= setOpts opts
    where
        enc = case encoding of
                   UTF8 -> "utf-8"
                   Latin1 -> "iso-8859-1"
setOpts (Normalize n:opts) pt = setOptBool "normalize" n pt >>= setOpts opts
setOpts (NormalizeStrict n:opts) pt = setOptBool "norm-strict" n pt >>= setOpts opts
setOpts (NormalizeForm form:opts) pt = setOpt "norm-form" nform pt >>= setOpts opts
    where
        nform = case form of
                     None -> "none"
                     NFD -> "nfd"
                     NFC -> "nfc"
                     Composed -> "comp"
setOpts (NormalizeRequired b:opts) pt = setOptBool "norm-required" b pt >>= setOpts opts
setOpts (Ignore i:opts) pt = setOptInteger "ignore" i pt >>= setOpts opts
setOpts (IgnoreReplace b:opts) pt = setOptBool "ignore-repl" b pt >>= setOpts opts
setOpts (SaveReplace b:opts) pt = setOptBool "save-repl" b pt >>= setOpts opts
setOpts (KeyboardDef s:opts) pt = setOpt "keyboard" s pt >>= setOpts opts
setOpts (SuggestMode sm:opts) pt = setOpt "sug-mode" mode pt >>= setOpts opts
    where
        mode = case sm of
                    Ultra -> "ultra"
                    Fast -> "fast"
                    Normal -> "normal"
                    Slow -> "slow"
                    BadSpellers -> "bad-spellers"
setOpts (IgnoreCase b:opts) pt = setOptBool "ignore-case" b pt >>= setOpts opts
setOpts (IgnoreAccents b:opts) pt = setOptBool "ignore-accents" b pt >>= setOpts opts
setOpts (FilterMode s:opts) pt = setOpt "mode" s pt >>= setOpts opts
setOpts (EmailMargin n:opts) pt = setOptInteger "email-margin" n pt >>= setOpts opts
setOpts (TeXCheckComments b:opts) pt = setOptBool "tex-check-comments" b pt >>= setOpts opts
setOpts (ContextVisibleFirst b:opts) pt = setOptBool "context-visible-first" b pt >>= setOpts opts
setOpts (RunTogether b:opts) pt = setOptBool "run-together" b pt >>= setOpts opts
setOpts (RunTogetherLimit n:opts) pt = setOptInteger "run-together-limit" n pt >>= setOpts opts
setOpts (RunTogetherMin n:opts) pt = setOptInteger "run-together-min" n pt >>= setOpts opts
setOpts (MainConfig s:opts) pt = setOpt "conf" s pt >>= setOpts opts
setOpts (MainConfigDir s:opts) pt = setOpt "conf-dir" s pt >>= setOpts opts
setOpts (DataDir s:opts) pt = setOpt "data-dir" s pt >>= setOpts opts
setOpts (LocalDataDir s:opts) pt = setOpt "local-data-dir" s pt >>= setOpts opts
setOpts (HomeDir s:opts) pt = setOpt "home-dir" s pt >>= setOpts opts
setOpts (PersonalConfig s:opts) pt = setOpt "per-conf" s pt >>= setOpts opts
setOpts (Layout s:opts) pt = setOpt "keyboard" s pt >>= setOpts opts
setOpts (Prefix s:opts) pt = setOpt "prefix" s pt >>= setOpts opts
setOpts (SetPrefix b:opts) pt = setOptBool "set-prefix" b pt >>= setOpts opts
setOpts [] pt = return pt

setOpt :: ByteString
       -> ByteString
       -> AspellConfig
       -> IO AspellConfig
setOpt key value pt = do
    withForeignPtr pt $ \ac ->
        useAsCString key $ \k ->
            useAsCString value $ aspell_config_replace ac k
    return pt

setOptBool :: ByteString -> Bool -> AspellConfig -> IO AspellConfig
setOptBool k v = setOpt k (if v then "true" else "false")

setOptInteger :: ByteString -> Integer -> AspellConfig -> IO AspellConfig
setOptInteger k v = setOpt k (pack $ show v)

-- | Creates a spell checker with default options.
--
-- @
-- 'spellChecker' = 'spellCheckerWithOptions' []
-- @
--
spellChecker :: IO (Either ByteString SpellChecker)
spellChecker = spellCheckerWithOptions []

-- | Creates a spell checker with a custom set of options.
spellCheckerWithOptions :: [ACOption] -> IO (Either ByteString SpellChecker)
spellCheckerWithOptions opts = do
    cf <- newConfig
    setOpts opts cf
    canError <- withForeignPtr cf new_aspell_speller
    (errNum :: Int) <- fromIntegral `fmap` aspell_error_number canError
    if errNum > 0
       then do
           errMsg <- aspell_error_message canError >>= packCString
           return $ Left errMsg
       else do
           speller <- to_aspell_speller canError
           for <- newForeignPtr delete_aspell_speller speller
           return $ Right for

-- | Convenience function for specifying a dictionary.
--
-- You can determine which dictionaries are available to you with @aspell dump dicts@.
--
-- @
-- 'spellCheckerWithDictionary' dict = 'spellCheckerWithOptions' ['Dictionary' dict]
-- @
spellCheckerWithDictionary :: ByteString -> IO (Either ByteString SpellChecker)
spellCheckerWithDictionary b = spellCheckerWithOptions [Dictionary b]

-- | Checks if a word has been spelled correctly.
check :: SpellChecker -> ByteString -> Bool
check checker word = unsafePerformIO $
    withForeignPtr checker $ \ck ->
        useAsCString word $ \w -> do
            res <- aspell_speller_check ck w $ negate 1
            return $ res == 1

-- | Lists suggestions for misspelled words.
--
-- If the input is not misspelled according to the dictionary, returns @[]@.
suggest :: SpellChecker -> ByteString -> IO [ByteString]
suggest checker word = withForeignPtr checker $ \ck ->
    useAsCString word $ \w -> do
        wlist <- aspell_speller_suggest ck w (negate 1)
        elems <- aspell_word_list_elements wlist
        suggestions <- strEnumToList elems
        delete_aspell_string_enumeration elems
        return suggestions

strEnumToList :: CStringEnumeration -> IO [ByteString]
strEnumToList enum = go enum
    where go e = do
            nw <- aspell_string_enumeration_next enum
            if nw == nullPtr
               then return []
               else do
                   curWord <- packCString nw
                   next <- go e
                   return $ curWord:next

foreign import ccall unsafe "aspell.h &delete_aspell_config"
    delete_aspell_config :: FunPtr (CAspellConfig -> IO ())

foreign import ccall unsafe "aspell.h &delete_aspell_speller"
    delete_aspell_speller :: FunPtr (CSpellChecker -> IO ())

foreign import ccall unsafe "aspell.h delete_aspell_string_enumeration"
    delete_aspell_string_enumeration :: CStringEnumeration -> IO ()

foreign import ccall unsafe "aspell.h new_aspell_config"
    new_aspell_config :: IO CAspellConfig

foreign import ccall unsafe "aspell.h aspell_config_replace"
    aspell_config_replace :: CAspellConfig
                          -> CString
                          -> CString
                          -> IO CAspellConfig

foreign import ccall unsafe "aspell.h new_aspell_speller"
    new_aspell_speller :: CAspellConfig
                       -> IO CAspellCanHaveError

foreign import ccall unsafe "aspell.h aspell_error_number"
    aspell_error_number :: CAspellCanHaveError
                        -> IO CUInt

foreign import ccall unsafe "aspell.h aspell_error_message"
    aspell_error_message :: CAspellCanHaveError
                         -> IO CString

foreign import ccall unsafe "aspell.h to_aspell_speller"
    to_aspell_speller :: CAspellCanHaveError
                      -> IO CSpellChecker

foreign import ccall unsafe "aspell.h aspell_speller_check"
    aspell_speller_check :: CSpellChecker
                         -> CString
                         -> CInt
                         -> IO CInt

foreign import ccall unsafe "aspell.h aspell_speller_suggest"
    aspell_speller_suggest :: CSpellChecker
                           -> CString
                           -> CInt
                           -> IO CWordList

foreign import ccall unsafe "aspell.h aspell_word_list_elements"
    aspell_word_list_elements :: CWordList
                              -> IO CStringEnumeration

foreign import ccall unsafe "aspell.h aspell_string_enumeration_next"
    aspell_string_enumeration_next :: CStringEnumeration
                                   -> IO CString

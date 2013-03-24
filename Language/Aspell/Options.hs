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

data ACOption = Dictionary ByteString -- ^ Base name of the dictionary to use. If this option is specified then Aspell will either use this dictionary or die. 
              | WordListDir ByteString -- ^ Location of the main word list.
              | Lang ByteString -- ^ Language to use. It follows the same format of the LANG environment variable on most systems. It consists of the two letter ISO 639 language code and an optional two letter ISO 3166 country code after a dash or underscore. The default value is based on the value of the @LC_MESSAGES@ locale. 
              | Size WordListSize -- ^ The preferred size of the word list.
              | PersonalWordList ByteString -- ^ Personal word list file name. 
              | ReplacementsList ByteString -- ^ Replacements list file name.
              | Encoding Encoding -- ^ The encoding the input text is in. When using the Aspell utility the default encoding is based on the current locale. Thus if your locale currently uses the @utf-8@ encoding than everything will be in UTF-8.
              | Normalize Bool -- ^ Perform Unicode normalization. Enabled by default. 
              | NormalizeStrict Bool -- ^ Avoid lossy conversions when normalizing. Lossy conversions includes compatibility mappings such as splitting the letter @OE@ (U+152) into @O@ and @E@ (when the combined letter is not available), and mappings which will remove accents. Disabled by default except when creating dictionaries. 
              | NormalizeForm NormalizeForm -- ^ The normalization form the output should be in. This option primarily effects the normalization form of the suggestions as when spell checkering as the actual text is unchanged unless there is an error. Valid values are 'None', 'NFD' for full decomposition (Normalization Form D), 'NFC' for Normalization Form C, or 'Composed' for fully composed. 'Composed' is like 'NFC' except that full composition is used rather than canonical composition. The normalize option must be enabled for this option to be used. 
              | NormalizeRequired Bool -- ^ Set to true when the current language requires Unicode normalization. This is generally the case when private use characters are used internally by Aspell or when Normalization Form C is not the same as full composition.
              | Ignore Integer -- ^ Ignore words with N characters or less.
              | IgnoreReplace Bool -- ^ Ignore commands to store replacement pairs.
              | SaveReplace Bool -- ^ Save the replacement word list on save all.
              | KeyboardDef ByteString -- ^ The base name of the keyboard definition file to use (see <http://aspell.net/man-html/Notes-on-Typo_002dAnalysis.html#Notes-on-Typo_002dAnalysis>).
              | SuggestMode SuggestMode -- ^ Suggestion mode = @'Ultra' | 'Fast' | 'Normal' | 'Slow' | 'BadSpeller'@ (see <http://aspell.net/man-html/Notes-on-the-Different-Suggestion-Modes.html#Notes-on-the-Different-Suggestion-Modes>).
              | IgnoreCase Bool -- ^ Ignore case when checking words.
              | IgnoreAccents Bool -- ^ Ignore accents when checking words (currently ignored).
              | FilterMode ByteString -- ^ Sets the filter mode. Possible values include, but not limited to, @none@, @url@, @email@, @sgml@, or @tex@.
              | EmailMargin Integer -- ^ The number of characters that can appear before the quote character.
              | TeXCheckComments Bool -- ^ Check TeX comments.
              | ContextVisibleFirst Bool -- ^ Switches the context which should be visible to Aspell. Per default the initial context is assumed to be invisible as one would expect when spell checking source files of programs where relevant parts are contained in string constants and comments but not in the remaining code. If set to true the initial context is visible while the delimited ones are hidden. 
              | RunTogether Bool -- ^ Consider run-together words valid.
              | RunTogetherLimit Integer -- ^ Maximum number of words that can be strung together.
              | RunTogetherMin Integer -- ^ Minimal length of interior words.
              | MainConfig ByteString -- ^ Main configuration file. This file overrides Aspell's global defaults.
              | MainConfigDir ByteString -- ^ Location of main configuration file.
              | DataDir ByteString -- ^ Location of language data files.
              | LocalDataDir ByteString -- ^ Alternative location of language data files. This directory is searched before 'DataDir'. It defaults to the same directory the actual main word list is in (which is not necessary 'Dictionary').
              | HomeDir ByteString -- ^ Location for personal files.
              | PersonalConfig ByteString -- ^ Personal configuration file. This file overrides options found in the global 'MainConfig' file.
              | Layout ByteString -- ^ Use this keyboard layout for suggesting possible words. These spelling errors happen if a user accidently presses a key next to the intended correct key. The default is keyboard standard. If you are creating documents, you may want to set it according to your particular type of keyboard. If spellchecking documents created elsewhere, you might want to set this to the keyboard type for that locale. If you are not sure, just leave this as standard.
              | Prefix ByteString -- ^ Prefix directory.
              | SetPrefix Bool -- ^ Set the prefix based on executable location (only works on WIN32 and when compiled with @--enable-win32-relocatable@).

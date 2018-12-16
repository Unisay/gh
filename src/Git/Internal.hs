module Git.Internal where

import Preamble hiding (some)

import Control.Monad.Except (MonadError)
import Text.GitConfig.Parser (Section (..), parseConfig)
import Text.Megaparsec (Parsec, parseMaybe, some)
import Text.Megaparsec.Char (alphaNumChar, char, string)

import qualified Data.HashMap.Strict as Map
import qualified Data.Text.IO as TIO
import qualified GitHub as GH

data ConfigError
  = ParsingFailure Text
  | CantFindOriginSection
  | CantFindOriginUrl
  | GithubUrlParsingFailure Text
  deriving Show

data Config
  = Config
  { repoOwner :: GH.Name GH.Owner
  , repoName  :: GH.Name GH.Repo
  } deriving Show

config :: (MonadIO m, MonadError ConfigError m) => m Config
config = do
  file <- liftIO $ TIO.readFile ".git/config" -- Walk directories up
  sections <- parseConfig file
    & either (throwError . ParsingFailure . show) pure
  Section _ keyVals <- find origin sections
    & maybe (throwError CantFindOriginSection) pure
  url <- Map.lookup "url" keyVals
    & maybe (throwError CantFindOriginUrl) pure
  (owner, repo) <- parseGithubUrl url
    & maybe (throwError $ GithubUrlParsingFailure url) pure
  return $ Config (GH.mkName (Proxy :: Proxy GH.Owner) owner)
                  (GH.mkName (Proxy :: Proxy GH.Repo) repo)


origin :: Section -> Bool
origin (Section ["remote", "origin"] _) = True
origin _                                = False

type Owner = Text
type Repo = Text

parseGithubUrl :: Text -> Maybe (Owner, Repo)
parseGithubUrl = parseMaybe parser
  where parser :: Parsec Void Text (Owner, Repo)
        parser = do -- git@github.com:owner/repo.git
          void $ string "git@github.com:"
          owner <- some alphaNumChar
          void $ char '/'
          repo <- some alphaNumChar
          void $ string ".git"
          return (toS owner, toS repo)

module Main
  ( run
  ) where

import Preamble

import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Except (runExceptT)
import GitHub.Data.PullRequests (CreatePullRequest (..))
import GitHub.Endpoints.PullRequests (createPullRequest)

import qualified Git
import qualified GitHub as GH


data GithubException
  = PullRequestCreationFailure GH.Error
  | GitConfigError Git.ConfigError
  deriving Show

instance Exception GithubException

run :: IO ()
run = do
  let request = CreatePullRequest
        { createPullRequestTitle =
          "The title of the pull request"
        , createPullRequestHead  =
          "The name of the branch where your changes are implemented"
        , createPullRequestBase  =
          "The name of the branch you want the changes pulled into"
        , createPullRequestBody  =
          "Please pull this in!"
        }
  creds <- credentials
  Git.Config { repoOwner, repoName } <- runExceptT Git.config
    >>= either (throwM . GitConfigError) pure

  pullRequest <- createPullRequest creds repoOwner repoName request
    >>= either (throwM . PullRequestCreationFailure) pure
  putText $ "Pull request created: " <> show pullRequest

credentials :: IO GH.Auth
credentials = return $ GH.OAuth "123" --TODO



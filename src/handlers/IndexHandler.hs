module IndexHandler (handleIndex)
where

import           Control.Applicative

import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist

import           Heist
import qualified Heist.Interpreted as I

import           Application

import Data.Monoid((<>))
import DbFunctions(DomPost, DomPublic, domPostText, domPostAuthorName, domPostImage, domPostPostId, domPublicLinkName, domPublicPublicId, loadPosts)

import Data.Text.Encoding(decodeUtf8)
import           Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.IO.Class(liftIO)

handleIndex :: Handler App App ()
handleIndex = render "app"

module Unison.Aslak where

import Unison.Prelude

import Unison.Prelude

import Unison.Codebase.MainTerm ( nullaryMain, mainTypes, getMainTerm )
import qualified Unison.Codebase.MainTerm as MainTerm
import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.DisplayThing
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Editor.SlurpResult (SlurpResult(..))
import qualified Unison.Codebase.Editor.SlurpResult as Slurp
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent(..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import Unison.Codebase.Editor.RemoteRepo (RemoteRepo, printNamespace, RemoteNamespace)

import           Control.Lens
import           Control.Lens.TH                ( makeLenses )
import           Control.Monad.State            ( StateT )
import           Control.Monad.Trans.Except     ( ExceptT(..), runExceptT)
import           Data.Bifunctor                 ( second )
import           Data.Configurator              ()
import qualified Data.List                      as List
import           Data.List                      ( partition, sortOn )
import           Data.List.Extra                ( nubOrd, sort )
import           Data.Maybe                     ( fromJust )
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified Text.Megaparsec               as P
import qualified Data.Set                      as Set
import           Data.Sequence                  ( Seq(..) )
import qualified Unison.ABT                    as ABT
import qualified Unison.Codebase.BranchDiff    as BranchDiff
import qualified Unison.Codebase.Editor.Output.BranchDiff as OBranchDiff
import           Unison.Codebase.Branch         ( Branch(..)
                                                , Branch0(..)
                                                )
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Codebase.BranchLoadMode ( BranchLoadMode(FailIfMissing, EmptyIfMissing) )
import qualified Unison.Codebase.BranchUtil    as BranchUtil
import qualified Unison.Codebase.Causal        as Causal
import qualified Unison.Codebase.Metadata      as Metadata
import           Unison.Codebase.Patch          ( Patch(..) )
import qualified Unison.Codebase.Patch         as Patch
import           Unison.Codebase.Path           ( Path
                                                , Path'(..) )
import qualified Unison.Codebase.Path          as Path
import qualified Unison.Codebase.NameSegment   as NameSegment
import qualified Unison.Codebase.Reflog        as Reflog
import           Unison.Codebase.SearchResult   ( SearchResult )
import qualified Unison.Codebase.SearchResult  as SR
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.DataDeclaration        as DD
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import qualified Unison.Name                   as Name
import           Unison.Name                    ( Name )
import           Unison.Parser                  ( Ann(..) )
import           Unison.Reference               ( Reference(..) )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent )
import qualified Unison.Referent               as Referent
import qualified Unison.ShortHash as SH
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.Result                 as Result
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Find              as Find
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import           Unison.Util.List               ( uniqueBy )
import qualified Unison.Util.Relation          as R
import qualified Unison.Util.Relation4          as R4
import           Unison.Util.TransitiveClosure  (transitiveClosure)
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import qualified Unison.Codebase.TypeEdit as TypeEdit
import Unison.Codebase.TermEdit (TermEdit(..))
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Typechecker as Typechecker
import qualified Unison.PrettyPrintEnv as PPE
import           Unison.Runtime.IOSource       ( isTest )
import qualified Unison.Runtime.IOSource as IOSource
import qualified Unison.Util.Star3             as Star3
import qualified Unison.Util.Pretty            as P
import qualified Unison.Util.Monoid            as Monoid
import Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.Codebase.Editor.TodoOutput as TO
import qualified Unison.Lexer as L
import Unison.Codebase.Editor.SearchResult' (SearchResult')
import qualified Unison.Codebase.Editor.SearchResult' as SR'
import qualified Unison.LabeledDependency as LD
import Unison.LabeledDependency (LabeledDependency)
import Unison.Type (Type)
import qualified Unison.Builtin as Builtin
import Unison.Codebase.NameSegment (NameSegment(..))
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.Editor.Propagate as Propagate
import qualified Unison.Codebase.Editor.UriParser as UriParser
import Data.Tuple.Extra (uncurry3)
import qualified Unison.CommandLine.DisplayValues as DisplayValues
import qualified Control.Error.Util as ErrorUtil
import Unison.Codebase.GitError (GitError)

-- resolveConfiguredGitUrl :: PushPull -> Path' -> Maybe RemoteNamespace -> Action' m v (Either _ RemoteNamespace)
-- resolveConfiguredGitUrl pushPull destPath' = \case
--       Just ns -> pure $ Right ns
--       Nothing -> do
--         let destPath = resolveToAbsolute destPath'
--         let configKey = gitUrlKey destPath
--         (eval . ConfigLookup) configKey >>= \case
--           Just url ->
--             case P.parse UriParser.repoPath (Text.unpack configKey) url of
--               Left e ->
--                 pure . Left $
--                   ConfiguredGitUrlParseError pushPull destPath' url (show e)
--               Right (repo, Just sbh, remotePath) ->
--                 pure . Left $
--                   ConfiguredGitUrlIncludesShortBranchHash pushPull repo sbh remotePath
--               Right ns ->
--                 pure . Right $ ns
--           Nothing ->
--             pure . Left $ NoConfiguredGitUrl pushPull destPath'

gitUrlKey p = Text.intercalate "." . toList $ "GitUrl" :<| fmap
        NameSegment.toText
        (Path.toSeq $ Path.unabsolute p)
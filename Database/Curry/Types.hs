{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.Curry.Types (
  -- DBM monad
  DBMT, unDBMT,
  DBMS,
  liftSTM,

  -- State and lenses
  DBMState(..),
  dbmTable,
  dbmUpdate,
  dbmLogger,
  dbmConfig,

  -- Configuration
  Config(..), def,
  SaveStrategy(..),
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Logger
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Identity
import           Data.Conduit
import           Data.Default
import qualified Filesystem.Path.CurrentOS    as FP
import           Language.Haskell.TH.Syntax   (Loc (..))
import           System.Log.FastLogger

import           Database.Curry.HashMap

type DBMT v m = DBMT_ (StateT (DBMState v) m)
type DBMS v = DBMT v STM

newtype DBMT_ m a =
  DBMT_ { unDBMT :: IdentityT m a }
  deriving
    ( Functor, Applicative, Monad
    , Alternative
    , MonadIO, MonadTrans, MonadBase b
    , MonadThrow, MonadResource
    )

deriving instance MonadState (DBMState v) m => MonadState (DBMState v) (DBMT_ m)

instance MonadTransControl DBMT_ where
  newtype StT DBMT_ a =
    StDBMT { unStDBM :: a }
  liftWith f =
    DBMT_ $ lift $ f $ liftM StDBMT . runIdentityT . unDBMT
  restoreT =
    DBMT_ . lift . liftM unStDBM

instance MonadBaseControl b m => MonadBaseControl b (DBMT_ m) where
  newtype StM (DBMT_ m) a = StMT { unStMT :: ComposeSt DBMT_ m a }
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM     = defaultRestoreM   unStMT

instance MonadIO m => MonadLogger (DBMT v m) where
  monadLoggerLog loc level msg = do
    logger <- gets _dbmLogger
    date <- liftIO $ loggerDate logger
    let (row, col) = loc_start loc
    liftIO $ loggerPutStr logger
      [ toLogStr date, LB " "
      , LB "[", LS (show level), LB "] "
      , toLogStr (loc_module loc), LB ":", LS (show row), LB ":", LS (show col), LB ": "
      , toLogStr msg
      , LB "\n"
      ]

data DBMState v
  = DBMState
    { _dbmTable  :: HashMap v
    , _dbmUpdate :: STM ()
    , _dbmLogger :: Logger
    , _dbmConfig :: Config
    }

data Config
  = Config
    { configPath         :: Maybe FP.FilePath
    , configSaveStrategy :: [SaveStrategy]
    , configVerbosity    :: LogLevel
    }

data SaveStrategy
  = SaveByFrequency
    { freqSecond  :: Int
    , freqUpdates :: Int
    }

makeLenses ''DBMState

instance Default Config where
  def = Config
    { configPath = Nothing
    , configSaveStrategy = []
    , configVerbosity = LevelInfo
    }

liftSTM :: STM a -> DBMS v a
liftSTM = lift . lift
{-# INLINE liftSTM #-}

-- This is in a separate module to work around TH breaking when loaded from a
-- module that also depends on foreign symbols.
module Schema.Utils.TH (mkEntities) where

import Language.Haskell.TH (Dec, Q)
import Database.Persist.Types (EntityDef)
import qualified Database.Persist.TH as TH

mkEntities :: String -> [EntityDef] -> Q [Dec]
mkEntities name = TH.share
    [TH.mkPersist TH.sqlSettings, TH.mkSave name]

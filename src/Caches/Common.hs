module Caches.Common where

import           Xcode.DWARF



data DWARFOperationError = ErrorGettingDwarfUUIDs
                         | FailedDwarfUUIDs [(DwarfUUID, String)]


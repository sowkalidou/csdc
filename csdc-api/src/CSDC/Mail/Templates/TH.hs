module CSDC.Mail.Templates.TH where

import Language.Haskell.TH
import Text.Mustache.Compile.TH

templateFor :: String -> Q Exp
templateFor name =
  compileMustacheFile $ "src/CSDC/Mail/Templates/" <> name <> ".md"

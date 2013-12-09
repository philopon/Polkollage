{-# LANGUAGE CPP, NoMonomorphismRestriction, OverloadedStrings #-}

module Polkollage.URL(urlSplice) where

import Heist
import Data.Monoid
import Text.XmlHtml(Node(..))
import Data.Text(Text)

text :: Monad m => Text -> m [Node]
text t = return [TextNode t]

urlSplice :: Monad m => Splices (m [Node])
#ifdef PRODUCTION
urlSplice =
  ("bootstrap.css" ## text "//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css") <>
  ("bootstrap.js"  ## text "//netdna.bootstrapcdn.com/bootstrap/3.0.3/js/bootstrap.min.js") <>
  ("jquery.js"     ## text "//ajax.googleapis.com/ajax/libs/jquery/2.0.3/jquery.min.js") <>
  ("angular.js"    ## text "//ajax.googleapis.com/ajax/libs/angularjs/1.2.4/angular.min.js")
#else
urlSplice = 
  ("bootstrap.css" ## text "/css/bootstrap.min.css") <>
  ("bootstrap.js"  ## text "/js/bootstrap.min.js") <>
  ("jquery.js"     ## text "/js/jquery-2.0.3.min.js") <>
  ("angular.js"    ## text "/js/angular.min.js")
#endif

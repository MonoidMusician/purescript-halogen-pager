module Main where

import Prelude
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Record as Record
import Prim.Row as Row
import Prim.RowList as RowList
import Data.Symbol (class IsSymbol, SProxy(..))
import Type.Row (RLProxy(..))
import Halogen as H
import Halogen.HTML as HH

class Pager
  (rl :: RowList.RowList)
  (slots :: # Type)
  (components :: # Type)
  (page :: # Type)
  (q :: Type) (m :: Type -> Type) | rl -> page where
  renderRL :: forall rlp. rlp rl ->
    Record components ->
    Variant.Variant page ->
    H.ComponentHTML' q slots m

instance pagerNil :: Pager RowList.Nil slots components () q m where
  renderRL _ _ = Variant.case_

instance pagerCons ::
  ( IsSymbol s
  , Ord p
  , Row.Cons s (Tuple (H.Component HH.HTML f i o m) (o -> Maybe q)) components' components
  , Row.Cons s (H.Slot f o p) slots' slots
  , Pager rl slots components page q m
  , Row.Cons s (Tuple p i) page page'
  ) => Pager (RowList.Cons s (Tuple p i) rl) slots components page' q m where
    renderRL _ r = renderRL (RLProxy :: RLProxy rl) r #
      let s = SProxy :: SProxy s in
      Variant.on s \(Tuple p i) ->
        let Tuple c adapt = Record.get s r in
        HH.slot s p c i adapt

renderPage ::
  forall rl slots components page q m.
    RowList.RowToList page rl =>
    Pager rl slots components page q m =>
  Record components -> Variant.Variant page -> H.ComponentHTML' q slots m
renderPage = renderRL (RLProxy :: RLProxy rl)

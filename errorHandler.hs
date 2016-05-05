module ErrorHandler
  where

  data E a = Ok a | Failed String

  thenE :: E a -> (a -> E b) -> E b m `thenE` k =
    case m of
      Ok a -> k a
      Failed e -> Failed e


  -- returnE :: a -> E a
  -- returnE a = Ok a

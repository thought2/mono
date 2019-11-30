module GraphLib where

type Id
  = String

type GraphSpec_ g e n
  = { graph :: { label :: g }
    , nodes :: Array { id :: Id, label :: n }
    , edges :: Array { fromId :: Id, toId :: Id, label :: e }
    }

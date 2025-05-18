module User.Handler where

import Shared.Models.User (User (..))

users :: [User]
users =
  [ User "Isaac Newton" 372 "isaac@newton.co.uk",
    User "Albert Einstein" 136 "ae@mc2.org"
  ]

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk"

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org"

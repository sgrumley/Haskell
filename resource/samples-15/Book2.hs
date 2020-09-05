type Author = String
type Title = String
type Edition = Int
type Year = Int

data Book = Book Author Title Edition Year

aBook = Book "Fred Smith" "How To Haskell" 2 1993

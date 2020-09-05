data Book = Book {
               author :: String,
               title :: String,
               edition :: Int,
               year :: Int
            }

aBook = Book {
           author = "Fred Smith",
           title = "How To Haskell",
           edition = 3,
           year = 2001
        }
        
anotherBook = aBook {edition = 4, year = 2010}

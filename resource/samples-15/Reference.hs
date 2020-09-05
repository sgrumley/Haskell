data Reference =
     Monograph {
        author :: String,
        title :: String,
        edition :: Int,
        year :: Int
     }
   | BookEd {
        editor :: String,
        title :: String,
        edition :: Int,
        year :: Int
     }
   | Journal {
        title :: String,
        volume :: Int,
        year :: Int
     }
   | Chapter {
        author :: String,
        title :: String,
        book :: Reference,
        chapter :: Int
     }
   | Article {
        author :: String,
        title :: String,
        journal :: Reference,
        page :: Int
     }

anArticle = Article {
               author = "Fred Smith",
               title = "Why Haskell?",
               journal = Journal {
                            title = "J. Prog",
                            volume = 50,
                            year = 2020   
                         },
               page = 4559 
            }

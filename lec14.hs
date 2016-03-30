data Date = Date { month :: Int, day :: Int, year :: Int }
data Citations = Book { author :: String, title :: String, publication :: String, date_pub :: Date, pages :: [Int] }
                |Article { author :: String, article_title :: String, journal_title :: String, date_pub :: Date, pages :: [Int] }
                |Website { author :: String, article_title :: String, publication :: String, url :: String, date_acc :: Date }



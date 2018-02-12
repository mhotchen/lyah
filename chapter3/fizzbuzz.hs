fizzbuzz :: [(Int, String)] -> [Int] -> [String]
fizzbuzz _     []   = []
fizzbuzz tests ints = fizzbuzz' tests ints []

fizzbuzz' :: [(Int, String)] -> [Int] -> [String] -> [String]
fizzbuzz' _     []          strings = strings
fizzbuzz' tests (head:rest) strings = test tests head "" : fizzbuzz' tests rest strings

test :: [(Int, String)] -> Int -> String -> String
test []          int ""     = show int
test []          _   string = string
test (head:rest) int string = test rest int (string ++ if int `mod` (fst head) == 0 then snd head else "")

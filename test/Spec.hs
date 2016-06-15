import Parse

runTest :: (String, String) -> IO ()
runTest (q, exp) = do
    res <- runParse q
    if res == exp
        then print "Passed"
        else do
                print "...Failed: "
                print q
                print exp
                print res

tests :: [(String, String)]
tests = [("select a from b", "SELECT a\nFROM b"),
         ("select a1, a2 from b", "SELECT\n\ta1,\n\ta2\nFROM b"),
         ("select a, sum(c) from b group by 1",
          "SELECT\n\ta,\n\tsum(c)\nFROM b\nGROUP BY 1"),
         ("\n\n\nselect \n\n\n\n1\n\n\n",
          "SELECT 1")
        ]

main :: IO ()
main = do
    print ""
    mapM_ runTest tests

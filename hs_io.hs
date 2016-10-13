import System.IO

foo = do
  withFile "foo.txt" ReadMode $ \fd -> do
    contents <- hGetContents fd
    print contents
    -- "foo\n"

bar = do
  contents <- withFile "foo.txt" ReadMode hGetContents
  print contents
  -- ""

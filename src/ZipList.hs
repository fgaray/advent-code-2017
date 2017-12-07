module ZipList where



data ZipList a = ZipList [a] a [a]
  deriving (Show)


const :: a -> ZipList a
const x = ZipList [] x []

insert :: ZipList a -> a -> ZipList a
insert (ZipList xs cur ys) y = ZipList xs cur (y : ys)

moveN :: Integral a => a -> ZipList a -> Maybe (ZipList a)
moveN 0 z = Just z
moveN n (ZipList (x:xs) cur (y:ys))
  | n < 0 = moveN (n + 1) (ZipList xs x (cur : y : ys))
  | n > 0 = moveN (n - 1) (ZipList (cur : x : xs) y ys)
moveN _ (ZipList [] cur []) = Nothing
moveN n (ZipList (x:xs) cur [])
  | n < 0 = moveN (n + 1) (ZipList xs x ([cur]))
  | n > 0 = Nothing
moveN n (ZipList [] cur (y:ys))
  | n < 0 = Nothing
  | n > 0 = moveN (n - 1) (ZipList ([cur]) y ys)

current :: ZipList a -> a
current (ZipList _ x _) = x

setCurrent :: ZipList a -> a -> ZipList a
setCurrent (ZipList xs _ ys) x = ZipList xs x ys

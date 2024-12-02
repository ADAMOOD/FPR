data Component = TextBox {name :: String, text :: String}
               | Button {name :: String, value :: String}
               | Container {name :: String, childern :: [Component]} deriving (Show)

listAllButtons :: Component -> [Component]
listAllButtons (Button x y) = [Button x y]
listAllButtons (TextBox _ _) = []
listAllButtons (Container _ childern) = concat (map listAllButtons childern)

gui :: Component
gui = Container "my App" [
    Container "Menu" [
        Button "btn_new" "New",
        Button "btn_open" "Open",
        Button "btn_close" "Close"],
    Container "Body" [TextBox "textbox_1" "Some text goes here"],
    Container "Footer" []]

listButtonNames :: Component -> [String]
listButtonNames (Button x _) = [x]
listButtonNames (TextBox _ _) = []
listButtonNames (Container _ childern) = concat (map listButtonNames childern)

countButtons :: Component -> Int
countButtons (Button x _) = 1
countButtons (TextBox _ _) = 0
countButtons (Container _ childern) = sum (map countButtons childern)

countComponents :: Component -> Int
countComponents (Button _ _) = 1
countComponents (TextBox _ _) = 1
countComponents (Container _ childern) = (sum (map countComponents childern)) +1

data Tree a = Leaf a 
            | Branch a (Tree a) (Tree a) deriving (Show, Eq)

myTree ::  Tree Int
myTree = (Branch 5 (Branch 6 (Leaf 1) (Leaf 2)) (Leaf 7))

sum' :: Tree Int -> Int
sum' (Leaf x) = x
sum' (Branch x l p) = x + (sum' l) + (sum' p)

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Branch x l r) = x : toList l ++ toList r

maxTree :: Ord a => Tree a -> a
maxTree (Leaf x) = x
maxTree (Branch x l r) = maximum(toList (Branch x l r))

depthTree :: Tree a -> Int
depthTree (Leaf x) = 1
depthTree (Branch x l r) = max (depthTree l) (depthTree r) + 1
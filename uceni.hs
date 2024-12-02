data Tree a = Leaf a 
            | Branch a (Tree a) (Tree a) deriving (Show, Eq)


myTree ::  Tree Int
myTree = (Branch 5 (Branch 6 (Leaf 1) (Leaf 2)) (Leaf 7))

sum' :: Tree Int -> Int
sum' (Leaf a) = a
sum' (Branch x l r) = x + sum' l + sum' r

toList :: Tree a -> [a]
toList (Leaf a) = [a]
toList (Branch x l r) = [x] ++ (toList l) ++ (toList r)

maxTree :: Ord a => Tree a -> a
maxTree (Leaf a) = a
maxTree (Branch x l r) =  maximum (toList ((Branch x l r)))

depthTree :: Tree a -> Int
depthTree (Leaf a) = 1
depthTree (Branch x l r) = max (depthTree l) (depthTree r) + 1

getGreaterElements :: Ord a => Tree a -> a -> [a]
getGreaterElements (Leaf x) n | x > n = [x]
                              | otherwise = []
getGreaterElements (Branch x l r) n = filter (>n) (toList (Branch x l r))       

toString :: Show a => Tree a -> String
toString (Leaf a) = show a
toString (Branch x l r) = show x ++ "(" ++ (toString l) ++ "," ++ (toString r) ++ ")"

leafCount :: Tree a -> Int
leafCount (Leaf a) = 1
leafCount (Branch x l r) = (leafCount l) + (leafCount r)

branchCount :: Tree a -> Int
branchCount (Leaf a) = 0
branchCount (Branch x l r) = (branchCount l) + (branchCount r) + 1

isLeaf ::Tree a -> Bool
isLeaf (Leaf _) = True
isLeaf (Branch _ _ _) = False 

myTree2 :: (Eq a, Num a) => Tree a
myTree2 = Branch 5 (Branch 6 (Leaf 1) (Leaf 2)) (Leaf 7)

contains :: Eq a => Tree a -> a -> Bool
contains (Leaf a) x = elem x (toList ((Leaf a)))
contains (Branch z l r) x = elem x (toList ((Branch z l r)))

greaterThan :: Ord a => Tree a -> a -> Int
greaterThan (Leaf x) l | x>l = 1
                       | otherwise = 0
greaterThan (Branch x l r) d = length (filter (>d) (toList (Branch x l r)))

--1

data Entity = Point Double Double 
            | Circle (Double, Double) Double 
            | Container [Entity]
            deriving (Show)

entity1 :: Entity
entity1 = Point 1 2

entity2 :: Entity
entity2 = Circle (1, 2) 5

entityContainer :: Entity
entityContainer = Container [entity1,entity2]

--2
data Component = TextBox {name :: String, text :: String}
               | Button {name :: String, value :: String}
               | Container {name :: String, childern :: [Component]} deriving (Show)


listAllButtons :: Component -> [Component]
listAllButtons (Button _ _) = (Button _ _)
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

countButtons :: Component -> Int
countButtons (Button _ _) = 1
countButtons (TextBox _ _) = 0
countButtons (Container _ children) = sum (map countButtons children)


--3
--            kam           co           id
addElement :: Component -> Component -> String -> Component
addElement (TextBox name text) _ _ = TextBox name text 
addElement (Button name value) _ _ = Button name value
addElement (Container name children) newElement targetName
    | name == targetName = Container name (newElement : children) 
    | otherwise = Container name (map (\child -> addElement child newElement targetName) children)


gui' :: Component
gui' = addElement gui (TextBox "textbox_2" "New Text") "Body"


--killing myself
data Html  = Attribute {name :: String, value :: String}
            | Tag {name :: String, attributes :: [Attribute] ,innerTags :: [Tag]}
            | HTMLDocument {tags :: [Tag]}

data Html  = Attribute {name :: String, value :: String}
            | Tag {name :: String, attributes :: [Html] ,innerTags :: [Html]}
            | HTMLDocument {tags :: [Html]}

htmlExample :: Html
htmlExample = HtmlDocument [
    Tag "html" [] [
        Tag "head" [] [
            Tag "title" [] [Tag "text" [Attribute "value" "Example Page"] []]
        ],
        Tag "body" [Attribute "class" "main-content"] [
            Tag "h1" [] [Tag "text" [Attribute "value" "Welcome!"] []],
            Tag "p" [] [Tag "text" [Attribute "value" "This is an example paragraph."] []]
        ]
    ]
]

data Element = Button {name :: String, text :: String}
             | Text {text :: String}
             | Panel {elements :: [Element]} 
             deriving (Show)

muj = Panel { elements = [Button {name = "B1", text = "tlacitko 1"},Text {text="Tohle je text"}] }


data Point = Point {column::Int,row::Int} 

data Position = Position {leftTopCorner :: Point, width :: Int, height :: Int} 

data Component = TextBox {name :: String, position :: Position, text :: String}
  | Button {name :: String, position :: Position, text :: String}
  | Container {name :: String, children :: [Component]} 

instance Show Point where
     show (Point c r) =  "(" ++ show c ++ "," ++ show r ++")"

instance Show Position where
     show (Position corner width height) = show corner ++ "[" ++ show width ++ "," ++ show height ++ "]"

showComponent :: Int -> Component -> String
showComponent indent (Button name pos text) =
  replicate indent ' ' ++ show pos ++ " Button[" ++ name ++ "]: " ++ text ++ "\n"
showComponent indent (TextBox name pos text) =
  replicate indent ' ' ++ show pos ++ " TextBox[" ++ name ++ "]: " ++ text ++ "\n"
showComponent indent (Container name children) =
  replicate indent ' ' ++ "Container - " ++ name ++ "\n"
    ++ concatMap (showComponent (indent + 4)) children

gui :: Component
gui =
  Container "My App"
    [ Container "Menu"
        [ Button "btn_new" (Position (Point 0 0) 100 20) "New",
          Button "btn_open" (Position (Point 100 0) 100 20) "Open",
          Button "btn_close" (Position (Point 200 0) 100 20) "Close"
        ],
      Container "Body" [TextBox "textbox_1" (Position (Point 0 20) 300 500) "Some text goes here"],
      Container "Footer" []
    ]

ghci> gui
Container - My App
        Container - Menu
                (0,0)[100,20] Button[btn_new]: New
                (100,0)[100,20] Button[btn_open]: Open
                (200,0)[100,20] Button[btn_close]: Close
        Container - Body
                (0,20)[300,500] TextBox[textbox_1]: Some text goes here
        Container - Footer

type Maze = [String]

printMaze :: Maze -> IO ()
printMaze x = putStr (concat (map (++"\n") x))

sample1 :: Maze
sample1 = ["*********",
           "* *   * *",
           "* * * * *",
           "* * * * *",
           "*   *   *",
           "******* *",
           "        *",
           "*********"]
sample2 :: Maze
sample2 = ["       ",
           "       ",
           "  ***  ",
           "  ***  ",
           "  ***  ",
           "       ",
           "       "]
sample3 :: Maze
sample3 = ["  * *  ",
           " ##### ",
           "  ***  ",
           "  * *  ",
           "  ***  ",
           "     * ",
           "       "]
sample4 :: Maze
sample4 = ["*********",
           "*s*   *e*",
           "* *   * *",
           "* *   * *",
           "*       *",
           "******* *",
           "        *",
           "*********"]
arrow :: Maze
arrow = [ "....#....",
          "...###...",
          "..#.#.#..",
          ".#..#..#.",
          "....#....",
          "....#....",
          "....#####"]


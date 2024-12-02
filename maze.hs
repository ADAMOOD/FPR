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


-- Funkce `above` spojí dvě bludiště vertikálně (svisle).
-- Spojí první maze s druhým.
above :: Maze -> Maze -> Maze
above first second = first ++ second
-- `first ++ second` jednoduše připojí seznamy `first` a `second` do jednoho.

-- Funkce `sideBySide` spojí dvě bludiště horizontálně (vedle sebe).
-- Pro každý řádek v obou maze spojí odpovídající řádky.
sideBySide :: Maze -> Maze -> Maze
sideBySide (f:fs) (s:ss) = (f ++ s) : (sideBySide fs ss)
sideBySide _ _ = []
-- Tento zápis spojuje první řádek z `first` s prvním řádkem z `second` (`f ++ s`).
-- Poté rekurzivně provádí toto spojení pro všechny následující řádky.
-- Pokud některý seznam skončí (prázdný), výstup bude prázdný seznam.

-- Funkce `getCol` vrátí sloupec z bludiště na zadané pozici `num`.
getCol :: Maze -> Int -> [Char]
getCol (f:fs) num = (f !! num) : (getCol fs num)
getCol [] _ = []
-- Pro každý řádek v maze vezme znak na pozici `num` (`f !! num`),
-- a rekurzivně přidává další znaky z dalších řádků.
-- Pokud je seznam prázdný, vrátí prázdný seznam.

-- Funkce `rotateR` otočí bludiště o 90 stupňů doprava.
rotateR :: Maze -> Maze
rotateR maze = map reverse (tmp 0 maze)
  where
    tmp _ [] = []
    tmp num maze
      | num < length (head maze) = (getCol maze num) : (tmp (num + 1) maze)
      | otherwise = []
-- Nejprve pomocí funkce `tmp` vezmeme sloupce (pomocí `getCol`), což vytvoří nový seznam, který je vertikálním převrácením původního.
-- Poté použijeme `map reverse`, abychom otočili každý sloupec naopak (aby to odpovídalo otočení bludiště o 90 stupňů).

-- Funkce `rotateL` otočí bludiště o 90 stupňů doleva.
rotateL :: Maze -> Maze
rotateL maze = reverse (tmp 0 maze)
  where
    tmp _ [] = []
    tmp num maze
      | num < length (head maze) = (getCol maze num) : (tmp (num + 1) maze)
      | otherwise = []
-- Stejně jako u `rotateR`, ale na konci použijeme `reverse`, abychom otočili celý výstup (což odpovídá otočení bludiště doleva).



-- Funkce `getFromMaze` vrátí znak z bludiště na specifikované pozici (x, y).
getFromMaze :: Maze -> (Int, Int) -> Char
getFromMaze maze (x, y) = (maze !! x) !! y
-- Používáme operátory `!!` pro získání řádku na pozici `x` a poté znaku na pozici `y` v tomto řádku.




-- Funkce `putInList` změní znak v seznamu (`list`) na pozici `x` na nový znak `c`.
putInList :: [Char] -> (Int, Char) -> [Char]
putInList list (x, c) = tmp 0 list
  where
    tmp num (l:ls)
      | x == num  = c : ls  -- Pokud je aktuální pozice `num` stejná jako `x`, nahradíme znak `c`.
      | otherwise = l : tmp (num + 1) ls  -- Jinak pokračujeme rekurzivně v seznamu.
    tmp _ [] = []  -- Pokud dosáhneme konce seznamu, vrátíme prázdný seznam.

-- Funkce `putMoreInList` mění více znaků v seznamu `list` podle seznamu změn.
putMoreInList :: [Char] -> [(Int, Char)] -> [Char]
putMoreInList list [] = list
putMoreInList list ((x, c):xs) = putMoreInList (putInList list (x, c)) xs
-- Pro každý prvek v seznamu změn aplikuje `putInList` a následně rekurzivně volá `putMoreInList` pro další změny.

-- Funkce `putIntoMaze` změní znaky v bludišti podle seznamu trojic `(x, y, char)`.
putIntoMaze :: Maze -> [(Int, Int, Char)] -> Maze
putIntoMaze maze [] = maze  -- Pokud seznam změn je prázdný, vrátí původní maze.
putIntoMaze maze ((fx, fy, fch) : xs) =
  putIntoMaze updatedMaze xs  -- Rekurzivně volá `putIntoMaze` s aktualizovaným maze.
  where
    updatedRow = putInList (maze !! fx) (fy, fch)  -- Pro řádek `fx` změníme znak na pozici `fy`.
    updatedMaze = take fx maze ++ [updatedRow] ++ drop (fx + 1) maze
    -- Aktualizujeme řádek `fx` a spojíme ho zpět do původního maze:
    -- `take fx maze` vezme všechny řádky před `fx`, `updatedRow` přidáme na pozici `fx` a zbytek maze získáme pomocí `drop (fx + 1)`.





-- Funkce getPart extrahuje obdélníkovou část bludiště definovanou pozicí
-- levého horního rohu a velikostí (výškou a šířkou).
getPart :: Maze -> (Int, Int) -> (Int, Int) -> Maze
getPart maze (startRow, startCol) (height, width) =
  -- List comprehension: pro každý řádek (row) z části bludiště
  [ take width (drop startCol row)  -- Vezmeme prvních 'width' znaků po přeskočení 'startCol' znaků
  | row <- take height (drop startRow maze)  -- Pro každý řádek od 'startRow' dál vezmeme 'height' řádků
  ]

-- Vysvětlení krok za krokem:
-- 1. `drop startRow maze`: 
--    - Tato část přeskočí první 'startRow' řádků z původního bludiště (maze).
--    - Výsledek je seznam řádků, který začíná na pozici 'startRow'.
--
-- 2. `take height`: 
--    - Tato část vezme prvních 'height' řádků z výsledku předchozího kroku.
--    - Tím se omezí výřez na požadovaný počet řádků.
--
-- 3. Pro každý řádek v tomto zúženém seznamu:
--    - `drop startCol row`: 
--      - Tato část přeskočí první 'startCol' znaků v aktuálním řádku.
--      - Výsledek je řádek, kde první 'startCol' znaků byly odstraněny.
--    - `take width`:
--      - Vezmeme prvních 'width' znaků z výsledného řádku.
--      - Tímto způsobem omezíme šířku výřezu na požadovaný počet znaků.
--
-- 4. Výsledek je seznam řádků, které tvoří požadovaný výřez.

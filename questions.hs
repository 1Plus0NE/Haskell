module Questions where

-- 1. Construir uma lista definida por dois limites

enumFromToX:: Int -> Int -> [Int]
enumFromToX x y = if(x > y) then [] else x:enumFromToX (x+1) y

-- 2. Construir uma lista definida por dois limites espaçados por um valor constante

enumFromThenToX:: Int -> Int -> Int -> [Int]
enumFromThenToX a b c | a > c && b >= a || a < c && b <= a = []
                      | otherwise = a:enumFromThenToX b (2*b-a) c

-- 3. Concatenar 2 listas

con:: [a] -> [a] -> [a]
con [] l2 = l2
con (x:xs) l = x:con xs l

-- 4. Retornar a posiçao de um elemento

findX:: [a] -> Int -> a
findX (x:xs) 0 = x
findX (x:xs) n = findX xs (n-1)

-- 5. Reverter uma lista

reverseL:: [a] -> [a]
reverseL [] = []
reverseL (x:xs) = reverseL xs++[x]

-- 6. Lista com os N primeiros elementos de uma lista

takeX:: Int -> [a] -> [a]
takeX _ [] = []
takeX x (h:t) = if(x<=0) then [] else h: takeX (x-1) t

-- 7. Lista sem os N primeiros elementos de uma lista

dropX:: Int -> [a] -> [a]
dropX _ [] = []
dropX x (h:t) = if(x<=0) then h:t else dropX (x-1) t

-- 8. Constroi uma lista de pares atraves de duas listas

zipList:: [a] -> [b] -> [(a,b)]
zipList [] _ = []
zipList _ [] = []
zipList (x:xs) (y:ys) = (x,y):zipList xs ys

-- 9. Replica um dado elemento N vezes

replicateX:: Int -> a -> [a]
replicateX 0 _ = []
replicateX n x = if(n<0) then [] else x:replicateX (n-1) x

-- 10. Dado um elemento e uma lista, constroi uma lista em que o elemento esta intercalado entre os elementos da lista

intercalado:: a -> [a] -> [a]
intercalado _ [] = []
intercalado x (h:t) = if(length t == 1) then h:x:t else h:x:intercalado x t

-- ou

inter1:: a -> [a] -> [a]
inter1 _ [] = []
inter1 _ [h] = [h]
inter1 n (h:t) = h:n:inter1 n t

-- 11. Agrupa elementos iguais e consecutivos numa lista 
{-
groupX:: Eq a => [a] -> [[a]]
groupX [] = [[]]
groupX [x] = [[x]]
groupX (x:xs) | elem x (head r) = (x:head r):tail r
              | otherwise = [x]:r
    where r = groupX xs
-}

group:: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = takeWhile' x xs: group(dropWhile' x (x:xs))

takeWhile':: Eq a => a -> [a] -> [a]
takeWhile' a [] = [a]
takeWhile' a (x:xs) = if(a == x) then x:takeWhile' a xs else [a]

dropWhile':: Eq a => a -> [a] -> [a]
dropWhile' a [] = []
dropWhile' a (x:xs) = if(a == x) then dropWhile' a xs else x:xs

-- 12. Concatenar as listas de uma lista

concatX:: [[a]] -> [a]
concatX [] = []
concatX (x:xs) = x++concatX xs

-- 13. Calcula a lista de prefixos de uma lista

initsX:: [a] -> [[a]]
initsX [] = [[]]
initsX l = initsX (init l)++[l]

-- 14. Calcula a lista de sufixos de uma lista

tailsX:: [a] -> [[a]]
tailsX [] = [[]]
tailsX (x:xs) = (x:xs):tailsX xs

-- 15. Calcula uma lista com o primeiro elemento de cada lista

heads':: [[a]] -> [a]
heads' [] = []
heads' ([]:xs) = heads' xs
heads' ((h:t):xs) = h:heads' xs

-- 16. Conta o total de elementos numa lista

total':: [[a]] -> Int
total' [] = 0
total' ([]:xs) = total' xs
total' ((h:t):xs) = 1 + total' (t:xs)

-- 17. Recebe uma lista de triplos e produz a lista de pares com o primeiro e ultimo elemento de cada triplo

fun:: [(a,b,c)] -> [(a,c)]
fun [] = [] 
fun ((a,b,c):t) = (a,c):fun t   

-- 18. Concatena as strings que estao na primeira componente de uma lista

cola:: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):t) = a++cola t

{- 19. Tendo um ano e uma idade como parametros e tendo uma lista de pares como nomes e datas de nascimento
        retornar a lista de nomes em que esse ano atingirao ou ultrapassaram a idade indicada -}

idade:: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade year age ((x,y):t) = if(year - y >= age) then x:idade year age t else idade year age t

-- 20. Dado um valor n e m constroi a lista de n^0 ate n^m-1

powerEnumFrom:: Int -> Int -> [Int]
powerEnumFrom n m = aux n m m

aux:: Int -> Int -> Int -> [Int]
aux n m 1 = [n^(m-1)]
aux n m c = n^(m-c):aux n m (c-1)

    -- n^m-1  

-- 21. Determina se um numero e primo 

isPrime:: Int -> Bool
isPrime n | n >= 2 = auxPrime n 2
          | otherwise = False

auxPrime:: Int -> Int -> Bool
auxPrime n x | x*x>n = True
             | mod n x == 0 = False
             | otherwise = auxPrime n (x+1)

-- 22. Testar se uma lista e prefixo de outra

isPrefix:: Eq a => [a] -> [a] -> Bool
isPrefix [] l2 = True
isPrefix l1 [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys 

-- 23. Testar se uma lista e sufixo da outra

isSufix:: Eq a => [a] -> [a] -> Bool
isSufix [] l2 = True
isSufix l1 [] = False
isSufix l (h:t) = l == (h:t) || isSufix l t 

-- 24. Testar se os elementos de uma lista ocorrem noutra pela mesma ordem relativa 

isSub:: Eq a => [a] -> [a] -> Bool
isSub [] l2 = True
isSub l1 [] = False
isSub (x:xs) (y:ys) = x == y && isSub xs ys || isSub (x:xs) ys

-- 25. Calcular a lista de posiçoes em que um dado elemento ocorre numa lista

-- elemIndex':: Eq a => a -> [a] -> [Int]
-- elemIndex' x l = [ n | n <- [0..(length l - 1)], x == (l !! n)]

-- ou

elemIndex:: Eq a => a -> [a] -> [Int]
elemIndex x l = getPos x l 0

getPos:: Eq a => a -> [a] -> Int -> [Int]
getPos _ [] _ = []
getPos n (x:xs) c = if(n == x) then c:getPos n xs (c+1) else getPos n xs (c+1)

-- 26. Calcula uma lista com os mesmos elementos da recebida sem repetiçoes

nub':: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = if(x `elem` xs) then nub' xs else x:nub' xs 

-- 27. Elimina a primeira ocorrencia de um elemento numa lista 

delete':: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' a (x:xs) = if(a == x) then xs else x:delete' a xs

-- 28. Retornar a lista resultante de remover as primeiras ocorrencias dos elementos da segunda lista da primeira

delXs:: Eq a => [a] -> [a] -> [a]
delXs l1 [] = l1
delXs [] l2 = [] 
delXs l (x:xs) = delXs(delete' x l) xs -- recorre a funcao anterior

{-
grupo:: Eq a => [a]->[[a]]
grupo [] = []
grupo (h:t) = (h:aux t):aux2 t
        where aux (a:b) = if a == h then a:aux b else []
              aux [] = []
              aux2 (y:z) = if y == h then aux2 z else grupo (y:z)
              aux2 [] = []
-}

-- 29. Dada uma lista unir elementos nao repetidos a outra lista

union':: Eq a => [a] -> [a] -> [a]
union' l1 [] = l1 
union' l (y:ys) = if(y `elem` l) then union' l ys else union' (l++[y]) ys

-- 30. Retornar uma lista resultante de remover da primeira lista os elementos que nao pertencem a segunda

intersect':: Eq a => [a] -> [a] -> [a]
intersect' [] _  = []
intersect' (x:xs) l2 = if (x `elem` l2) then x:intersect' xs l2  else intersect' xs l2

-- 31. Dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista

insert':: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t) = if(h>x) then x:h:t else h:insert' x t

-- 32. Juntar todas as strings de uma lista numa so, sendo separadas por um espaço

stringEspaco:: [String] -> String
stringEspaco [] = ""
stringEspaco (h:t) = if(length t == 0) then h++"" else h++" "++stringEspaco t

-- 33. Juntar todas as strings de uma lista numa so, sendo juntas por um \n

stringNewLine:: [String] -> String
stringNewLine [] = ""
stringNewLine (h:t) = h++"\n"++stringNewLine t

-- 34. Dada uma lista, retorna a posicao do maior elemento da lista

pMaior:: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (h:t) = if(h >= (t !! r)) then 0 else 1+r
    where r = pMaior t

-- 35. Retorna uma lista construida a partir de elementos de uma lista 

lookup':: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' n ((x,y):t) = if(n == x) then (Just y) else lookup' n t 

-- 36. Calcula o maior prefixo crescente de uma lista 

preCrescente:: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (x:y:t) = if(x<y) then preCrescente t else preCrescente t

-- 37. Ordenar uma lista assumindo que existe a funçao insert por usar

iSort:: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert' h (iSort t)

-- 38. Dadas duas strings retornar True se a primeira string for menor que a segunda

menor':: String -> String -> Bool
menor' _ "" = False
menor' "" _ = True
menor' (h:t) (x:xs) | h < x = True
                    | h == x = menor' t xs
                    | otherwise = False

-- 39. Testar se um elemento pertence a um multi-conjunto

elemMSet:: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((x,_):t) = if(a == x) then True else elemMSet a t 

-- 40. Converte um multi-conjunto na lista dos seus elementos

converteMSet:: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,y):t) = (take y (repeat x))++converteMSet t

-- alternativa sem recorrer a outras funcoes

converteMSet':: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((x,1):t) = x:converteMSet' t
converteMSet' ((x,y):t) = x:converteMSet' ((x,y-1):t)

-- 41. Acrescenta um elemento a um multi-conjunto

insereMSet:: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,y):t) = if(a == x) then (x,y+1):t else (x,y):insereMSet a t

-- 42. Remove um elemento a um multi-conjunto

removeMSet:: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet a ((x,y):t) | a == x = if(y > 1) then (x,(y-1)):t else t
                       | otherwise = (x,y):removeMSet a t

-- 43. Dada uma lista ordenada crescente, calcula o multi-conjunto dos seus elementos

constroiMSet:: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet l = auxConstroi (last l) (constroiMSet (init l))

auxConstroi:: Eq a => a -> [(a,Int)] -> [(a,Int)]
auxConstroi a [] = [(a,1)]
auxConstroi a ((x,y):t) = if(a == x) then (x,y+1):t else (x,y):auxConstroi a t

-- MITICO GROUP

groupList:: Eq a => [a] -> [[a]]
groupList [] = []
groupList [x] = [[x]]
groupList (h:t) = if(h == (head t)) then (h:(head a)):tail a else [h]:a
    where a = groupList t

-- 44. Dividir uma lista de Eithers em duas listas

partitionEither:: [Either a b] -> ([a],[b])
partitionEither [] = ([],[])
partitionEither (Left a:t) = ((a:ar),br)
    where (ar,br) = partitionEither t 
partitionEither (Right b:t) = (ar,(b:br))
    where (ar,br) = partitionEither t 

--[Left 1, Right 2]

-- 45. Coleciona os elementos do tipo a de uma lista    

catMaybe:: [Maybe a] -> [a]
catMaybe [] = []
catMaybe (Just h:t) = h:catMaybe t
catMaybe (Nothing:t) = catMaybe t

-- 46. Produzir uma lista de movimentos suficientes para que o robot passe de uma posiçao para outra

data Movimento = Norte | Sul | Este | Oeste
                deriving Show

caminho:: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | x1 < x2 = Este:caminho(x1+1,y1) (x2,y2)
                        | x1 > x2 = Oeste:caminho(x1-1,y1) (x2,y2)
                        | y1 < y2 = Norte:caminho(x1,y1+1) (x2,y2)
                        | y1 > y2 = Sul:caminho(x1,y1-1) (x2,y2)
                        | otherwise = []

-- 47. Verifica se o robot alguma vez volta a passar pela posicao inicial ao longo do percurso correspondente 
{-
hasLoops:: (Int,Int) -> [Movimento] -> Bool
hasLoops (x,y) m = elem (x,y) (auxLoop2 (x,y) m)                       

auxLoop2 :: (Int, Int) -> [Movimento] -> [(Int,Int)]
auxLoop2 x [] = [x]
auxLoop2 (x,y) (h:t) = case h of
                                Norte -> (x,y+1):auxLoop2 (x,y+1)  t
                                Sul   -> (x,y-1):auxLoop2 (x,y-1)  t
                                Oeste -> (x-1,y):auxLoop2 (x-1,y)  t
                                Este -> (x+1,y):auxLoop2 (x+1,y)  t

-}

hasLoops:: (Int,Int) -> [Movimento] -> Bool
hasLoops (x,y) m = auxLoop (x,y) (x,y) m

auxLoop:: (Int,Int) -> (Int,Int) -> [Movimento] -> Bool
auxLoop (x1,y1) (x2,y2) [] = if(x1 == x2 && y1 == y2) then True else False
auxLoop (x1,y1) (x2,y2) (h:t) = case h of 
                                            Norte -> auxLoop (x1,y1+1) (x2,y2) t
                                            Sul   -> auxLoop (x1,y1-1) (x2,y2) t
                                            Oeste -> auxLoop (x1-1,y1) (x2,y2) t
                                            Este  -> auxLoop (x1+1,y1) (x2,y2) t
                                        
-- 48. Dada uma lista com retangulos contar quantos deles sao quadrados

type Ponto = (Float,Float)
data Retangulo = Rect Ponto Ponto

contaQuadrados:: [Retangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (Rect (x1,y1) (x2,y2):t) = if(abs(y2-y1) == abs(x2-x1)) then 1+contaQuadrados t else contaQuadrados t 

-- 49. Dada uma lista de retangulos, determinar a area total

areaTotal:: [Retangulo] -> Float
areaTotal [] = 0
areaTotal (Rect (x1,y1) (x2,y2):t) = abs(x2-x1)*abs(y2-y1) + areaTotal t

-- 50. Recebe uma lista de equipamentos e devolve a quantidade de equipamentos que nao devem ser reparados.

data Equipamento = Bom | Razoavel | Avariado 
    deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t
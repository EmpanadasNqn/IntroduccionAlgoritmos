-- ejercicio1 a)
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Eq, Show, Ord, Bounded)

-- b) 
titulo :: Carrera -> String
titulo Computacion = "Licenciatura en cs de la Computacion"          
titulo Matematica  = "licenciatura en Matematica"
titulo Fisica      = "licenciarura en fisica"
titulo Astronomia  = "licenciarut en Astronomia"

-- c)
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Show, Ord, Bounded)
 
--d) 
cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

-- ejercicio2 a)
--data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Eq, Show, Ord, Bounded)
--data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Show, Ord, Bounded)

-- ejercicio3 a)
minimoElemento :: Ord a => [a] -> a
minimoElemento [a] = a 
minimoElemento (x:xs)= min x (minimoElemento xs) 

-- b)
minimoElemento' :: (Ord a , Bounded a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x:xs) = min x (minimoElemento' xs)

-- c)
--Usar la funcion minimoElemento para determinar la nota más grave de la melodıa:
--minimoElemento [Fa, La, Sol, Re, Fa]
-- = Re

--ejercicio4 a)
-- Ingreso mes un sinonimo de tipo

type Ingreso = Int

--Cargo y Area son tipos enumerados

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq, Show)

data Area = Admininstrativa | Ensenanza | Economica | Postgrado deriving (Eq, Show)

--Persona es un tipo algebraico

data Persona = Decane | Docente Cargo | NoDocente Area | Estudiante Carrera Ingreso deriving (Eq, Show) 

-- b)
-- Docente :: Cargo -> Persona

-- c)
cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc (x:xs) c  
    |x == Docente c = 1 + cuantos_doc xs c
    |otherwise  =  cuantos_doc xs c
    
--d) ¿La funcion anterior usa filter? Si no es ası, reprogramala --para usarla.

cuantos_doc' :: [Persona]-> Cargo -> Int
cuantos_doc' (x:xs) c = length ( filter (== Docente c) xs)

-- Ejercicico 5 a)

data Alteracion = Bemol | Sostenido | Natural deriving(Eq, Show)
data NotaMusical = Nota NotaBasica Alteracion deriving(Eq, Show)

sonido  :: NotaBasica -> Int
sonido Do = 1
sonido Re = 3
sonido Mi = 5
sonido Fa = 6
sonido Sol = 8
sonido La = 10
sonido Si = 12

-- b)

sonidoCromatico :: NotaMusical -> Int
sonidoCromatico ( Nota a b ) | b == Sostenido = sonido a + 1
                             | b == Bemol = sonido a - 1
                             | b == Natural = sonido a

-- c)instance Eq NotaMusical where 
    n1 == n2 = sonidoCromatico n1 == sonidoCromatico n2

-- d)

instance Ord NotaMusical where 
    n1 <= n2 = sonidoCromatico n1 <= sonidoCromatico n2


--ejercicio 6 a)
primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just x

--ejercicio 7)

data Cola = VaciaC | Encolada Persona Cola deriving (Eq,Show)

-- 7 (1)

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada a b) = Just b


-- ejemplos :
--      atender (Encolada Decano ( Encolada (Docente Titular) (Encolada (Docente Adjunto)(VaciaC))))
--      Just (Encolada (Docente Titular) (Encolada (Docente Adjunto) VaciaC))

-- 7  (2)
encolar :: Persona -> Cola -> Cola
encolar p VaciaC =  Encolada p VaciaC
encolar p (Encolada a b) = Encolada a (encolar p b)  

--ejemplo:

--encolar Decano (Encolada (Docente Titular)(Encolada Decano (Encolada (Docente Asociado)(Encolada (Docente Asistente) (VaciaC)))))
--Encolada (Docente Titular) (Encolada Decano (Encolada (Docente Asociado) (Encolada (Docente Asistente) (Encolada Decano VaciaC))))

-- 7 (3)

busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC c = Nothing
busca (Encolada a b) c 
    |a == (Docente c) = Just a
    |a /= (Docente c) = busca b c

--   ejemplo:
--   busca (Encolada (Docente Titular)(Encolada Decano (Encolada (Docente Asociado)(Encolada (Docente Asistente) (VaciaC))))) Asistente
--   Just (Docente Asistente)

-- 7 (4)
-- El tipo Cola se asemeja al tipo Lista


data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving (Show)


--8 (a) 
-- podriamos hacerlo de la siguiente forma:

type GuiaTelefonica = ListaAsoc String Int

-- de esta forma la lista recibiria un nombre (String) 
-- y el numero de documento de la persona (Int)

--8 b (1)

la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b c) = 1 + la_long c 

-- ejemplos :

-- Main> la_long (Nodo "Facundo" 39622848 (Nodo "Juli" 655547( Nodo "Fran" 53539 (Vacia))))
-- 3

-- 8 b (2)
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia l2 = l2
la_concat (Nodo a b l1) l2 = Nodo a b (la_concat l1 l2)

-- 8 b (3)
la_agregar :: ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar Vacia x y = (Nodo x y (Vacia))
la_agregar (Nodo a b l1) x y = Nodo a b (la_agregar l1 x y )

-- 8 b (4)

la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo a b l1) = (a,b) : la_pares l1

-- 8 b (4)

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia c = Nothing
la_busca (Nodo a b l1) c 
    | a == c = Just b
    | a /= c  = la_busca l1 c

-- 8 b (4)

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar c Vacia = Vacia
la_borrar c (Nodo a b l1) 
    |a == c = l1
    |a /= c = la_borrar c l1

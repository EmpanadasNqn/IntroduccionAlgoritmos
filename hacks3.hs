--1)
--a)
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Eq, Show)

--b)
titulo :: Carrera -> String
titulo Matematica = "Lic en Matematica"
titulo Fisica = "Lic en Fisica"
titulo Computacion = "Lic en Computacion"
titulo Astronomia = "Lic en Astronomia"

--c)
--data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si

--d)

cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

--2)
--Para completar la definicion de nota basica es necesario agregar un deriving Eq y Ord para poder comparar las notas
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Show,Eq,Ord)

--3)

--a)-
minimoElemento :: Ord a=> [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)

--b)-
minimoElemento' :: (Bounded a, Ord a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x:xs) = min x (minimoElemento' xs)

--c)-

--minimoElemento [Fa, La, Sol, Re, Fa] = Re

--4)-
type Ingreso = Int

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq, Show)
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Eq, Show)

data Persona = Decane 
            | Docente Cargo 
            | NoDocente Area
            | Estudiante Carrera Ingreso deriving (Eq, Show)

--b) El tipo del constructor Docente es Persona


--c)-

cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc (p:ps) c | (p == Docente c) = (1 + cuantos_doc ps c)
                     | otherwise = cuantos_doc ps c

--d)-
cuantos_docfilter :: [Persona] -> Cargo -> Int
cuantos_docfilter [] c = 0
cuantos_docfilter (p:ps) c = length(filter (tieneCargo c) (p:ps))


tieneCargo :: Cargo -> Persona -> Bool
tieneCargo c (Docente x) = (x == c)


--5)-
--a)-
data Alteracion = Bemol | Sostenido | Natural deriving (Eq, Ord)
data NotaMusical = Nota NotaBasica Alteracion deriving (Eq, Ord)

sonido :: NotaBasica -> Int
sonido Do = 1
sonido Re = 3
sonido Mi = 5
sonido Fa = 6
sonido Sol = 8
sonido La = 10
sonido Si = 12

--b)-
sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota n a) | a==Bemol = (sonido n) - 1
                           | a==Sostenido = (sonido n) +1
                           | a==Natural = sonido n

--6)-
--a)-
primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just x


--7)-
data Cola = VaciaC | Encolada Persona Cola deriving Show

--a)-
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada p c) = Just c

--b)-
encolar :: Persona -> Cola -> Cola
encolar p VaciaC = Encolada p VaciaC
encolar p (Encolada h c) = (Encolada h (Encolada p c))

--c)-
busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC n = Nothing
busca (Encolada (Docente h) c) n | (h==n) = Just (Docente h) 
                                 | otherwise = busca c n
busca (Encolada _ 	c) n = busca c n

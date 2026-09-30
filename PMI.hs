import Control.Arrow (ArrowChoice(left))
------ 1) Producto: Codigo - Descripcion - Precio - Stock
data Producto = Producto Int String Float Int deriving Show --definicion del tipo producto el cual guarda Codigo - Descripcion - Precio - Stock respectivamente

-------- datos de prueba ------------

arbolProductos :: Arbol Producto
arbolProductos = 
    Nodo (Producto 103 "Teclado Mecanico" 15.0 2)
        (Nodo (Producto 102 "Mouse Inalambrico" 25.0 0)
            (Nodo (Producto 101 "Laptop Pro 15" 10.0 8) Vacio Vacio)
            Vacio
        )
        (Nodo (Producto 104 "Monitor 27" 100.0 1)
            Vacio
            (Nodo (Producto 105 "Audifonos Bluetooth" 15.0 0) Vacio Vacio)
        )

listaProductos :: [Producto]
listaProductos = convertirALista arbolProductos


------ 2)
---- a)

data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) deriving Show --definicion del arbol segun el enunciado b)

-- Ya que la firma dice que es [Producto] -> [Producto], es decir recibe una lista y devuelve una lista, hago una funcion para convertir el arbol a lista
convertirALista :: Arbol Producto -> [Producto]
convertirALista Vacio = []
convertirALista (Nodo r izq der) = convertirALista izq ++ [r] ++ convertirALista der


-- Creo la funcion conStock para luego aplicarla sobre los elementos de la lista
conStock :: [Producto] -> [Producto]
conStock = filter (\(Producto _ _ _ stock) -> stock > 0) -- uso filter para poder filtrar los productos que tengan un stock mayor a cero, el resto de datos no interesa asi que los dejo con variables anonimas


---- b)
type Codigo = Int --tipo auxiliar para poder buscar un producto por su codigo y corroborar que coincida con el almacenado

buscarProducto :: Codigo -> Arbol Producto -> Maybe Producto
buscarProducto c Vacio = Nothing 
buscarProducto c (Nodo (Producto cod desc precio stock) izq der)
        | c == cod = Just (Producto cod desc precio stock)
        | c < cod = buscarProducto c izq
        | c > cod = buscarProducto c der
        

---- c)
valorInventario :: [Producto] -> Float
valorInventario = foldr(\(Producto _ _ precio stock) acc -> precio * fromIntegral stock + acc) 0

---- d) 
actualizarStock :: Codigo -> Int -> [Producto] -> Either String [Producto]
actualizarStock _ _ [] = Left "Error: no existe inventario"
actualizarStock codBuscado stockNuevo (Producto cod desc precio stock : xs) 
    | stockNuevo < 0 = Left "Error: numero de stock invalido"
    | codBuscado == cod = Right (Producto cod desc precio stockNuevo : xs)
    | otherwise = case actualizarStock codBuscado stockNuevo xs of
        Left error -> Left error
        Right listaActualizada -> Right (Producto cod desc precio stock : listaActualizada)

---- e) sin terminar
foldArbol :: (b -> a -> b -> b) -> b -> Arbol a -> b
foldArbol _ base Vacio = base
foldArbol f base (Nodo a izq der) = f resIzq a resDer
  where 
    resIzq = foldArbol f base izq
    resDer = foldArbol f base der

cantidadProductos :: Arbol Producto -> Int
cantidadProductos = foldArbol(\izq _ der -> izq + 1 + der) 0

productosCaros :: Float -> Arbol Producto -> [Producto]
productosCaros valor = foldArbol(\izq p@(Producto _ _ precio _) der -> if precio > valor then izq ++ [p] ++ der else izq ++ der) []

---- f)
cantidadStock :: [Producto] -> (Int,Float)
cantidadStock x = aux x (0, 0)
    where 
        aux [] acc = acc
        aux ((Producto _  _ precio stock):xs) (accStock, accPrecio) = aux xs (accStock + stock, accPrecio + precio * fromIntegral stock)
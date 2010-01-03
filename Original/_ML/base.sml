exception NotFound
infixr ++
exception NotImplemented
fun stub _ = raise NotImplemented
datatype 'a susp = $ of 'a
fun force ($x) = x

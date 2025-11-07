module Lens exposing (Lens, compose, over, set)


type alias Lens big small =
    { get : big -> small
    , set : small -> big -> big
    }


compose : Lens a b -> Lens b c -> Lens a c
compose ab bc =
    { get = \a -> bc.get (ab.get a)
    , set = \c a -> ab.set (bc.set c (ab.get a)) a
    }


set : Lens a b -> b -> a -> a
set l v a =
    l.set v a


over : Lens a b -> (b -> b) -> a -> a
over l f a =
    set l (f (l.get a)) a

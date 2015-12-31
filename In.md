Edward Kmett's lens library made lenses talk of the town. This is, however, not a lens tutorial (let's wait for the upcoming Simon Peyton Jones's intro to lenses). I'm going to concentrate on one aspect of lenses that I've found intriguing -- the van Laarhoven representation. 

Quick introduction: A lens is a data structure with a getter and a setter. 
``` haskell
get :: b -> a
set :: b -> a -> b
```
Given object of type `b`, the getter returns the value of type `a` of the substructure on which the lens is focused (say, a field of a record, or an element of a list). The setter takes the object and a new value and returns a modified object, with the focus substructure replaced by the new value.

A lens can also be represented as a single function that returns a `Store` structure. Given the object of type `b` the lens returns a pair of: old value at the focus, and a function to modify it. This pair represents the `Store` comonad (I followed the naming convention in Russell O'Connor's paper, see bibliography):
``` haskell
data Store a b = Store
  { pos  :: a
  , peek :: a -> b
  }
```
The two elements of `Store` are just like getter and setter except that the initial `b ->` was factored out.

Here's the unexpected turn of events: Twan van Laarhoven came up with a totally different representation for a lens. Applied to the `Store` comonad it looks like this:

``` haskell
forall g . Functor g => (a -> g a) -> g b
```

The `Store` with `pos` and `peek` is *equivalent* to this single polymorphic function. Notice that the polymorphism is in terms of the functor `g`, which means that the function may only use the functor-specific `fmap`, nothing else. Recall the signature of `fmap` -- for a given `g`:

``` haskell
fmap :: (a -> b) -> g a -> g b
```

The only way the van Laarhoven function could produce the result `g b` is if it has access to a function `a -> b` and a value of type `a` (which is exactly the content of `Store`). It can apply the function `a -> g a` to this value and `fmap` the function `a -> b` over it. Russell O'Connor showed the isomorphism between the two representations of the `Store` comonad.

This is all hunky-dory, but what's the theory behind this equivalence? When is a data structure equivalent to a polymorphic function? I've seen this pattern in the Yoneda lemma (for a refresher, see my tutorial: [Understanding Yoneda](https://www.fpcomplete.com/user/bartosz/understanding-yoneda)). In Haskell we usually see a slightly narrower application of Yoneda. It says that a certain set of polymorphic functions (natural transformations) is equivalent to a certain set of values in the image of the functor `g`:

``` haskell
forall t . (a -> t) -> g t
   ≈ g a
```
Here, `(a -> t)` is a function, which is mapped to `g t` -- some functor `g` acting on type `t`. This mapping is defined once for all possible types `t` -- it's a natural transformation -- hence `forall t`. All such natural transformations are equivalent to (parameterized by, isomomophic to) a type (set of values) `g a`. I use the symbol `≈` for type isomorphism.

There definitely is a pattern, but we have to look at the application of Yoneda not to Haskell types (the Hask category) but to Haskell functors. 

## Yoneda on Functors

We are in luck because functors form a category. Objects in the *Functor Category* are functors between some underlying categories, and morphisms are natural transformations between those functors.

Here's the Yoneda construction in the Functor Category. Let's fix one functor `f` and consider all natural transformations of `f` to an arbitrary functor `g`. These transformations form a set, which is an object in the Set Category. For each choice of `g` we get a different set. Let's call this mapping from functors to sets a representation, `rep`. This mapping, the canonical Yoneda embedding, is actually a functor. I'll call this a second order functor, since it takes a functor as its argument. Being a functor, its action on morphisms in the Functor Category is also well defined -- morphisms being natural transformations in this case.

![Yoneda on functors](http://www.bartosz.com/images/Yoneda/FunctorYoneda.png)

Now let's consider an arbitrary mapping from functors to sets, let's call it `eta` and let's assume that it's also a 2nd order functor. We have now two such functors, `rep` and `eta`. The Yoneda lemma considers mappings between these 2nd order functors, but not just any mappings -- natural transformations. Since those transformations map 2nd order functors, I'll also call them 2nd order natural transformations (thick yellow arrow in the picture). 

What does it mean for a transformation to be natural? Technically, it means that a certain diagram commutes (see my Yoneda tutorial), but the Haskell intuition is the following: A natural transformation must be polymorphic in its argument. It treats this argument generically, without considering its peculiarities. In our case the 2nd order natural transformation will be polymorphic in its functor argument `g`. 

The Yoneda lemma tells us that all such 2nd order natural transformations from `rep` to `eta` are in one-to-one correspondence with the elements of `eta` acting on `f`. I didn't want to overcrowd the picture, but imagine another red arrow going from `f` to some set. That's the set that parameterizes these natural transformations.

## Back to Haskell

Let's get down to earth. We'll specialize the general Functor Category to the category of endofunctors in Hask and replace the Set Category with Hask (a category of Haskell types, which are treated as sets of values). 

Elements of the Functor Category will be represented in Haskell through the `Functor` class. If `f` and `g` are two functors, a natural transformation between them can be defined pointwise (e.g., acting on actual types) as a polymorphic function. 

``` haskell
forall t . f t -> g t
```

Another way of looking at this formula is that it describes a mapping from an arbitrary functor `g` to the Haskell type `forall t . f t -> g t`, where `f` is some fixed functor. This mapping is the Yoneda embedding we were talking about in the previous section. We'll call it `RepF`:

``` active haskell
{-# LANGUAGE Rank2Types #-}

type RepF f g = (Functor f, Functor g) 
    => forall t . f t -> g t

main = putStrLn "types check"
```

The second ingredient we need is the mapping `Eta` that, just like `Rep` maps functors to types. Since the kind of the functor `g` is `* -> *`, the kind of `Eta` is `(* -> *) -> *`.

``` haskell
type family Eta :: (* -> *) -> *
```
Putting all this together, the Yoneda lemma tells us that the following types are equivalent:

``` active haskell
{-# LANGUAGE Rank2Types, TypeFamilies #-}

type family Eta :: (* -> *) -> *

type NatF f = Functor f  
    => forall g . Functor g 
    => forall t. (f t -> g t) -> Eta g

-- equivalent to 

type NatF' = Functor f => Eta f

main = putStrLn "types check"
```

There are many mappings that we can substitute for `Eta`, but I'd like to concentrate on the simplest nontrivial one, parameterized by some type `b`. The action of this `EtaB` on a functor `g` is defined by the application of `g` to `b`. 

``` active haskell
{-# LANGUAGE Rank2Types #-}

type EtaB b g = Functor g => g b

main = putStrLn "types check"
```

Now let's consider 2nd order natural transformations betwen `RepF` and `EtaB` or, more precisely, between `RepF f` and `EtaB b` for some fixed `f` and `b`. These transformations must be polymorphic in `g`:

``` active haskell
{-# LANGUAGE Rank2Types #-}

type EtaB b g = Functor g => g b
-- show
type NatFB f b = Functor f  
    => forall g . Functor g 
    => forall t. (f t -> g t) -> EtaB b g
-- /show
main = putStrLn "types check"
```

This can be further simplified by applying `EtaB` to its arguments:

``` haskell
type NatFB f b = Functor f  
    => forall g . Functor g 
    => forall t. (f t -> g t) -> {-hi-}g b{-/hi-}
```

The final step is to apply the Yoneda lemma which, in this case, tells us that the above type is equivalent to the type obtained by acting with `EtaB b` on `f`. This type is simply `f b`.

Do you see how close we are to the van Laarhoven equivalence?

``` haskell
forall g :: Functor g => (a -> g a) -> g b
    ≈ (a, a -> b)
```

We just need to find the right `f`. But before we do that, one little exercise to get some familiarity with the Yoneda lemma for functors.

## Undoing fmap

Here's an interesting choice for the functor `f` -- function application. For a given type `a` the application `(->) a` is a functor. Indeed, it's easy to implement `fmap` for it:

``` active haskell
instance Functor ((->) a) where
    -- fmap :: (t -> u) -> (a -> t) -> (a -> u)
    fmap f g = f . g

main = putStrLn "It typechecks!"
```

Let's plug this functor in the place of `f` in our version of Yoneda:

``` haskell
type NatApA a b =   
    forall g . Functor g 
    => forall t. ({-hi-}(a -> t){-/hi-} -> g t) -> g b

NatApA a b ≈ {-hi-}a -> b{-/hi-}
```

Here, `f t` was replaced by `a -> t` and `f b` by `a -> b`. Now let's dig into this part of the formula:

``` haskell
forall t. ((a -> t) -> g t)
```
Isn't this just the left hand side of the regular Yoneda? It's a natural transformation between the Yoneda embedding functor  `(->) a` and some functor `g`. The lemma tells us that this is equivalent to the type `g a`. So let's make the substitution:

``` haskell
type NatApA a b =   
    forall g . Functor g 
    => {-hi-}g a{-/hi-} -> g b

NatApA a b ≈ a -> b
```

On the one hand we have a function `g a -> g b` which maps types lifted by the functor `g`. If this function is polymorphic in the functor `g` than it is equivalent to a function `a -> b`. This equivalence shows that `fmap` can go both ways. In fact it's easy to show the isomorphism of the two types directly. Of course, given a function `a -> b` and any functor `g`, we can construct a function `g a -> g b` by applying `fmap`. Conversely, if we have a function `g a -> g b` that works for any functor `g` then we can use it with the trivial identity functor `Identity` and recover `a -> b`. 

So this is not a big deal, but the surprise for me was that it followed from the Yoneda lemma.

## The Store Comonad and Yoneda

After this warmup exercise, I'm ready to unveil the functor `f` that, when plugged into the Yoneda lemma, will generate the van Laarhoven equivalence. This functor is:

``` haskell
Product (Const a) ((->) a)
```
When acting on any type `b`, it produces:

``` haskell
(Const a b, a -> b)
```

The `Const` functor ignores its second argument and is equivalent to its first argument:
``` haskell
newtype Const a b = Const { getConst :: a }
```

So the right hand side of the Yoneda lemma is equivalent to the `Store` comonad `(a, a -> b)`. 

Let's look at the left hand side of the Yoneda lemma:

``` haskell
type NatFB f b = Functor f  
    => forall g . Functor g 
    => forall t. (f t -> g t) -> g b
```
and do the substitution:

``` haskell
forall g . Functor g 
  => forall t. ({-hi-}(a, a -> t){-/hi-} -> g t) -> g b
```


Here's the curried version of the function in parentheses:

``` haskell
forall t. (a -> (a -> t) -> g t)
```

Since the first argument `a` doesn't depend on `t`, we can move it in front of `forall`:

``` haskell
a -> forall t . (a -> t) -> g t
```


We are now free to apply the 1st order Yoneda
``` haskell
forall t . (a -> t) -> g t ≈ g a
```

Which gives us:

``` haskell
a -> forall t . (a -> t) -> g t ≈ a -> g a
```

Substituting it back to our formula, we get:

``` haskell
forall g . Functor g => (a -> g a) -> g b
  ≈ (a, a -> b)
```
Which is exactly the van Laarhoven equivalence.


## Conclusion

I have shown that the equivalence of the two formulations of the `Store` comonad and, consequently, the `Lens`, follows from the Yoneda lemma applied to the Functor Category. This equivalence is a special case of the more general formula for functor-polymorphic representations:

``` haskell
type family Eta :: (* -> *) -> *

Functor f  
    => forall g . Functor g 
    => forall t. (f t -> g t) -> Eta g
  ≈ Functor f => Eta f
```

This formula is parameterized by two entities: a functor `f` and a 2nd order functor `Eta`. 

In this article I restricted myself to one particular 2nd order functor, `EtaB b`, but it would be interesting to see if more complex `Eta`s lead to more interesting results. 

In the choice of the functor `f` I also restricted myself to just a few simple examples. It would be interesting to try, for instance, functors that generate recursive data structures and try to reproduce some of the biplate and multiplate results of Russell O'Connor's. 

## Appendix: Another Exercise

Just for the fun of it, let's try substituting `Const a` for `f`: 

The naive substitution would give us this:

``` haskell
forall g . Functor g 
    => (forall t . Const a t -> g t) -> g b 
  ≈ Const a b
```

But the natural transformation on the left:

``` haskell
forall t . Const a t -> g t
```
cannot be defined for all `g`s. `Const a t` doesn't depend on `t`, so the co-domain of the natural transformation can only include functors that don't depend on their argument -- constant functors. So we are really only looking for `g`s that are `Const c` for any choice of `c`. All the allowed variation in `g` can be parameterized by type `c`:

``` haskell
forall c . (Const a t -> Const c t) -> Const c b 
  ≈ Const a b
```

If you remove the `Const` noise, the conclusion turns out to be pretty trivial:
``` haskell
forall c . (a -> c) -> c ≈ a
```
It says that a polymorphic function that takes a function `a -> c` and returns a `c` must have some fixed `a` inside. This is pretty obvious, but it's nice to know that it can be derived from the Yoneda lemma.

## Acknowledgment

Special thanks go to the folks on the Lens IRC channel for correcting my Haskell errors and for helpful advice.


## Bibliography
1. Twan van Laarhoven, [CPS based functional references](http://twanvl.nl/blog/haskell/cps-functional-references)
2. Russell O'Connor, [Functor is to Lens as Applicative is to Biplate](http://arxiv.org/pdf/1103.2841v2.pdf)

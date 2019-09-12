#if __GLASGOW_HASKELL__ >= 840
#define MONOID_HEAD Monoid a
#else
#define MONOID_HEAD (Semigroup a, Monoid a)
#endif
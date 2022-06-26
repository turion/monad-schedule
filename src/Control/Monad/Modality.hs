-- I don't want to commit to QuickCheck here
class Modality m where
  allM :: m Property -> Property
  someM :: m Property -> Property

instance Modality m => Modality (WriterT w m) where
  allM = allM . fmap fst . runWriterT
  someM = someM . fmap fst . runWriterT

-- But I need the constraint Arbitrary r here
instance (Arbitrary r, Modality m) => Modality (ReaderT r m) where
  allM p = property $ \r -> allM $ runReaderT p r
  someM = _ -- I need a good "exists" in QuickCheck, see e.g. https://stackoverflow.com/questions/42764847/is-there-a-there-exists-quantifier-in-quickcheck

{-
Instances for
* function
* tuple
* functor compositions
* derived instances for all transformers
* Look for a formal/algebraic IO & corresponding typeclass
-}

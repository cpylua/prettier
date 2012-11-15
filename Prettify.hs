module Prettify
       (
         Doc,
         (<>), empty, text, line, nest,
         char, double,
         (<+>), fsep, fcat, hsep, hcat, punctuate,
         (</>), (</+>),
         pretty
       ) where

infixr 5 :<|>
infixr 6 :<>
infixr 6 <>
infixr 6 <+>
infixr 6 </>
infixr 6 </+>

data Doc = Nil
         | Doc :<> Doc
         | Nest Int Doc
         | Text String
         | Line
         | Doc :<|> Doc
           
data LayoutDoc = LayoutNil
               | String `LayoutText` LayoutDoc
               | Int `LayoutLine` LayoutDoc
                   
(<>) :: Doc -> Doc -> Doc
Nil <> x = x
x <> Nil = x
x <> y   = x :<> y

hcat :: [Doc] -> Doc
hcat = foldr (<>) Nil

(<+>) :: Doc -> Doc -> Doc
Nil <+> x = x
x <+> Nil = x
x <+> y   = x <> text " " <> y

hsep :: [Doc] -> Doc
hsep = foldr (<+>) Nil

(</+>) :: Doc -> Doc -> Doc
x </+> y   = x <> group line <> y

fsep :: [Doc] -> Doc
fsep = foldr (</+>) Nil

(</>) :: Doc -> Doc -> Doc
x </> y   = x <> (Nil :<|> line) <> y

fcat :: [Doc] -> Doc
fcat = foldr (</>) Nil

empty = Nil
nest = Nest
text = Text
line = Line

char :: Char -> Doc
char c = text [c]

double :: Double -> Doc
double = text . show

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (x:xs) = x <> p : punctuate p xs

pretty :: Int -> Doc -> String
pretty w = layout . best w 0    


-- The pretty printer
group :: Doc -> Doc
group x = flatten x :<|> x

flatten :: Doc -> Doc
flatten Nil        = Nil
flatten (x :<> y)  = flatten x :<> flatten y
flatten (Nest i x) = Nest i (flatten x)
flatten d@(Text _) = d
flatten Line       = Text " "
flatten (x :<|> y) = flatten x

layout :: LayoutDoc -> String
layout LayoutNil          = ""
layout (s `LayoutText` x) = s ++ layout x
layout (i `LayoutLine` x) = '\n':replicate i ' ' ++ layout x

best :: Int -> Int -> Doc -> LayoutDoc
best w k x = go w k [(0, x)]
  where
    go w k []                = LayoutNil
    go w k ((_,Nil):z)       = go w k z
    go w k ((i, x :<> y):z)  = go w k $ (i,x):(i,y):z
    go w k ((i, Nest j x):z) = go w k $ (i+j,x):z
    go w k ((_, Text s):z)   = s `LayoutText` go w (k+length s) z
    go w k ((i, Line):z)     = i `LayoutLine` go w i z
    go w k ((i, x :<|> y):z) = better w k (go w k $ (i,x):z) (go w k $ (i,y):z)
      where
        better w k x y = if fits (w-k) x then x else y
        fits w _ | w < 0          = False
        fits _ LayoutNil          = True
        fits w (s `LayoutText` x) = fits (w-length s) x 
        fits _ (_ `LayoutLine` _) = True
                               

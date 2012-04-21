import Text.ParserCombinators.ReadP
import Control.Monad
main=do n<-l;forM_[1..n]$ \t->do putStrLn$"Case #"++show t++":";m<-l;[(t,_)]<-fmap(d.join)$r m g;n<-l;r n$print.t.drop 2.words=<<g
d=readParen True$readP_to_S$do w<-p reads;(do f<-p lex;y<-p d;n<-p d;u(\x->w*(if elem f x then y else n)x))<++u(\_->w::Float)
p=readS_to_P
r=replicateM
g=getLine
l=readLn
u=return
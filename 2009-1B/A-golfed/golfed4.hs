import Text.ParserCombinators.ReadP
main=f(\t->do putStrLn$"Case #"++show t++":";s<-r g;r$do x<-g;let{d=do"("<-e;w<-e;c<-do{f<-e;y<-d;n<-d;u$if elem f(words x)then y else n}<++u 1.0;e;u$c*read w};print.fst.head.readP_to_S d$id=<<s)
f x=do n<-g;mapM x[1..read n]
e=readS_to_P lex
r=f.const
g=getLine
u=return
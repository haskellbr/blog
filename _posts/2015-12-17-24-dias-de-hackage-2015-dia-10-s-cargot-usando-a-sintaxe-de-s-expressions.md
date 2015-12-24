---
layout: post
title: "24 dias de Hackage, 2015 - dia 10 - s-cargot: Usando a sintaxe de s-expressions"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original.](http://conscientiousprogrammer.com/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

# Dia 10
Há momentos em que eu tenho inveja do mundo do Lisp.

Um desses momentos é quando estou definindo alguma linguagem de domínio
específico, porque no mundo do Lisp, a escolha natural é a representar usando
_s-expressions_ como a sintaxe concreta, e não se preocupar com definir ainda
outra sintaxe especial, junto com escrever um parser para essa sintaxe para a
sintaxe abstrata e também um _pretty-printer_ da sintaxe abstrata para a sintaxe
concreta. Talvez no longo termo, os usuários podem querer sintaxe especial que
não é só _s-expressions_, mas para protótipos iniciais, pelo menos, parece
valoroso não se comprometer com nenhuma sintaxe e só usar
_s-expressions_. Apesar de que não há nada de mágico sobre _s-expressions_
(XML, JSON ou qualquer outra representação genérica de uma estrutura de
árvore), elas são particularmente concisas e flexíveis.

<!-- more -->

S-expressions são tão úteis que no meu primeiro emprego como um engenheiro de
software nos anos 90, nós tínhamos uma biblioteca de s-expressions para C++
para "inputar" e "outputar" um formato que era basicamente uma linguagem de
domínio específico que era processada por uma gama de ferramentas (isso era
antes do XML ser inventado e incluía um validador que eu escrevi em Standard
ML).

A biblioteca no Hackage para trabalhar com s-expressions em Haskell é a
[`s-cargot`](http://hackage.haskell.org/package/s-cargot).
Muitas outras já existiram, mas quase todas foram aos poucos deixando de ser
mantidas, enquanto essa é nova e vem com todo tipo de funcionalidade.

Hoje, darei um exemplo de como usar essa biblioteca, no contexto de um domínio
de problemas no qual ter uma sintaxe concreta é importante.

## Instalação
Precisamos adicionar `s-cargot` para o `stack.yaml`:

{% highlight yaml %}
- s-cargot-0.1.0.0
{% endhighlight %}

## A tarefa: matemática simbólica
A tarefa a ser resolvida aqui é, aproximadamente o suficiente, uma tradução de
código Lisp (Scheme, para ser preciso) para um
[diferenciador simbólico de expressões matemáticas](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3.2)
no livro clássico de ciência da computação
["Structure and Interpretation of Computer Programs" (SICP)](https://mitpress.mit.edu/sicp/).
Não vou descontruir a solução aqui, mas só me concentrar em alguns problemas de
sintaxe.

Um exemplo: dada uma expressão matemática como a função linear
`5x + 7`, nós queremos encontrar a derivativa simbólida em respeito a `x`, para
obter `5`.

## A sintaxe mais simples para o tipo da expressão
Como modelamos uma expressão e uma função para computar a derivativa de uma
expressão? Vamos começar com a forma mais crua possível, que é definir um tipo
de dados para a expressão, `Exp`, junto com os construtores alfanuméricos `N`
(para números), `V` (para variáveis), `Plus` (para a soma de sub-expressões) e
`Times` (para o produto de sub-expressões). Um pedaço da suite do
HSpec/QuickCheck que define o que precisamos:

{% highlight haskell %}
module SymbolicDifferentiation.AlphaSyntaxSpec where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times), deriv)

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))

spec :: Spec
spec =
  describe "diferenciação simbólica" $ do
    prop "d/dx (x + n) == 1" $ \x n ->
      deriv (Plus (V x) (N n)) x `shouldBe` N 1
    prop "d/dx (x + y) == x, if x /= y" $ \x y ->
      x /= y ==>
      deriv (Times (V x) (V y)) x `shouldBe` V y
    prop "d/dx (a * x + b) == x" $ \a x b ->
      deriv (Plus (Times (N a) (V x)) (N b)) x `shouldBe` N a
    it "d/dx (x * y * (x + 3)) == (x * y) + y * (x + 3)" $ do
      deriv (Times (Times (V "x") (V "y"))
                   (Plus (V "x") (N 3))) "x" `shouldBe`
        (Plus (Times (V "x") (V "y"))
              (Times (V "y") (Plus (V "x") (N 3))))
{% endhighlight %}

A sintaxe parece OK para expressões simples, mas horrível quando há muitas
sub-expressões dentro de outras, com parênteses para o agrupamento, como no
último exemplo artificial.

Aqui está o código para um ainda ingênuo diferenciador simbólico, tendo apenas
algumas heurísticas embutidas para um pouco de simplificação por meio de
reescrita (por exemplo, adicionar `0` a uma expressão resulta na própria
expressão ao invés de construir uma sub-expressão `N 0` superflua):

{% highlight haskell %}
module SymbolicDifferentiation.AlphaSyntax where

-- | Variável em uma expressão
type Var = String

-- | Uma Expressão.
data Exp
  = N Int          -- ^ números
  | V Var          -- ^ variáveis
  | Plus Exp Exp   -- ^ soma
  | Times Exp Exp  -- ^ produto
  deriving (Show, Eq)

-- | Derivada de uma expressão em respeito a uma variável
deriv :: Exp -> Var -> Exp
deriv (N _)         _ = N 0
deriv (V v')        v = N (if v' == v then 1 else 0)
deriv (Plus e1 e2)  v = plus (deriv e1 v) (deriv e2 v)
deriv (Times e1 e2) v = plus (times e1 (deriv e2 v))
                             (times (deriv e1 v) e2)

-- | Construtor inteligente que simplifica ao combinar sub-expressões
plus :: Exp -> Exp -> Exp
plus (N 0)  e      = e
plus e      (N 0)  = e
plus (N n1) (N n2) = N (n1 + n2)
plus e1     e2     = Plus e1 e2

-- | Construtor inteligente que simplifica ao combinar sub-expressões
times :: Exp -> Exp -> Exp
times (N 0)  _      = N 0
times _      (N 0)  = N 0
times (N 1)  e      = e
times e      (N 1)  = e
times (N n1) (N n2) = N (n1 * n2)
times e1     e2     = Times e1 e2
{% endhighlight %}

A sintaxe do código, usando _pattern matching_, parece razoavelmente boa para
mim. É a construção de um `Exp` que é um pouco feia, mas não terrível. Então
estamos em uma situação na qual o implementador dessa linguagem está feliz, mas
o usuário não. Na verdade, nós nem expusemos uma forma para um usuário fora do
sistema criar expressões: até agora, temos uma API mas nenhum parser de string
para expressão.

## Parsing a partir de qual formato?
Uma forma de prosseguirmos seria escrever um parser para uma sintaxe
customizada, por exemplo, poderíamos escrever uma função `fromString :: String
Exp` tal que:

{% highlight haskell %}
fromString "5x + 7y + 20" ==
  Plus (Plus (Times (N 5) (V "x"))
             (Times (N 7) (V "y")))
       (N 20)
{% endhighlight %}

Nós também teríamos que escrever um _pretty-printer_ `toString :: Exp ->
String` inteligente o suficiente para voltar para a forma original:

{% highlight haskell %}
toString (Plus (Plus (Times (N 5) (V "x"))
                     (Times (N 7) (V "y")))
               (N 20)) == "5x + 7y + 20"
{% endhighlight %}

É uma tarefa direta escrever um parser desses usando o
[`parsec`](http://hackage.haskell.org/package/parsec)
ou algo do gênero, mas você pode imaginar que as vezes é um pouco irritante
desenhar ou fazer os usuários aprenderem a sintaxe de uma linguagem muito mais
complexa (incluindo operadores infixos, precedência, escopo, tipos de blocos
diferentes, etc.).

Então vamos assumir para o propósito desse artigo que nós temos uma razão para
preferir _s-expressions_ com a "notação polonesa reversa", somente composta de
prefixos, da mesma forma que a comunidade de Lisp faz para evitar todas as
desvantagens de uma sintaxe especial.

### (Update 14/12/2015) Usando `Earley` para expor uma sintaxe customizada

No
[dia 14](/2015/12/21/24-dias-de-hackage-2015-dia-14-earley-uma-biblioteca-de-parsers-promisora-para-haskell.html),
criei uma sintaxe customizada e um parser usando a biblioteca de parsing `Earley`.

### Testes do QuickCheck
Aqui estão alguns testes de exemplo do QuickCheck para mostrar o que queremos
ser capazes de fazer. (Note que por conveniencia, estamos usando interpolação de strings com o pacote
[`here`](http://hackage.haskell.org/package/here) como introduzido ontem,
[dia 9](/2015/12/16/24-dias-de-hackage-2015-dia-9-pontos-interessantes-do-template-haskell.html).)

{% highlight haskell %}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module SymbolicDifferentiation.SExpSpec where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))
import qualified SymbolicDifferentiation.SExp as SExp

import Test.Hspec (Spec, hspec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)

import Data.String.Here (i)

-- | Necessário para a descoberta automática
spec :: Spec
spec =
  describe "Sintaxe s-expression para uma expressão" $ do
    prop "(+ x a)" $ \a ->
      SExp.parse [i|(+ x ${a})|] `shouldBe`
        Right (Plus (V "x") (N a))

    prop "(* (+ x a) (+ y b))" $ \a b ->
      SExp.parse [i|
                     (* (+ x ${a})
                        (+ y ${b}))
                   |] `shouldBe`
        Right (Times (Plus (V "x") (N a))
                     (Plus (V "y") (N b)))

    it "(!? x y)" $
      SExp.parse "(!? x y)" `shouldBe`
        Left "\"!?\" is not a valid operator"
{% endhighlight %}

Incluí um pequeno teste para indicar que nós também queremos alguma forma de
capturar e lidar com erros. Nada é mais irritante para um usuário do que
mensagens de erros de parse ruins. Não podemos oferecer mensagens ótimas aqui,
mas pelo menos vamos dar uma ideia de como poderíamos o fazer.

## O parser de _s-expressions_

`s-cargot` expõe formas muito flexíveis de construir parsers de
_s-expressions_, baseadas em que tipo de sintaxe você quer permitir (Scheme
completo ou não, por exemplo) e permite _hooks_ em muitos níveis para suportar
"leitores" customizados e também especificar o parser de átomos desejado.

Antes de mais nada, alguns imports:

{% highlight haskell %}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module SymbolicDifferentiation.SExp where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))

import qualified Data.SCargot as S
import Data.SCargot.Language.Basic (basicParser)
import Data.SCargot.Repr.WellFormed
       (WellFormedSExpr(WFSList, WFSAtom), fromWellFormed)

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Read (signed, decimal)

import Data.String.Here (i)

-- | Um erro de parse
type Error = String
{% endhighlight %}


A parte principal é uma pipeline que chama um parser `s-cargot` e então chama o
nosso próprio parser de uma estrutura representando uma _s-expression_ para
nosso tipo `Exp`:

{% highlight haskell %}
-- | Para simplificar, usamod o 'basicParser' que só trata todo átomo
-- como um 'Text', que nós podemos parsear mais tarde ao invés de no primeiro
-- "pass".
parse :: Text -> Either Error Exp
parse text = parseOneSexp text >>= toExp

parseOneSexp :: Text -> Either Error (WellFormedSExpr Text)
parseOneSexp = S.decodeOne (S.asWellFormed basicParser)
{% endhighlight %}

Uma vez tendo uma _s-expression_ bem formada, podemos a desestruturar,
encontrando os possíveis erros.

{% highlight haskell %}
toExp :: WellFormedSExpr Text -> Either Error Exp
toExp (WFSAtom text) = fromAtom text
toExp (WFSList [WFSAtom operatorText, sexp1, sexp2]) = do
  operator <- fromOperator operatorText
  e1 <- toExp sexp1
  e2 <- toExp sexp2
  return (operator e1 e2)
toExp list@(WFSList _) = Left [i|${list} should have exactly 3 elements|]

fromOperator :: Text -> Either Error (Exp -> Exp -> Exp)
fromOperator "+" = return Plus
fromOperator "*" = return Times
fromOperator text = Left [i|${text} is not a valid operator|]

-- | Um átomo é um inteiro ou uma variável
fromAtom :: Text -> Either Error Exp
fromAtom text =
  case signed decimal text of
    Right (n, "") ->
      return (N n)
    Right (_, _) ->
      Left [i|extra garbage after numeric in ${text}|]
    Left _ ->
      return (V (Text.unpack text))
{% endhighlight %}

## Pretty-printing
E nós ganhamos _pretty-printing_ de graça do `s-cargot` se nós transformarmos
nosso `Exp` em uma _s-expression_. Não vou mostrar detalhes, mas você pode usar
o _printer_ básico de _s-expressions_ ou o customizar com muitas opções
incluíndo a estratégia de indentação. Vamos usar o básico.

Um exemplo de um teste para nosso `SExp.prettyPrint`:

{% highlight haskell %}
    prop "pretty-printing" $ \a b ->
      SExp.prettyPrint (Times (Plus (V "x") (N a))
                              (Plus (V "y") (N b))) `shouldBe`
       [i|(* (+ x ${a}) (+ y ${b}))|]
{% endhighlight %}

O código:

{% highlight haskell %}
fromExp :: Exp -> WellFormedSExpr Text
fromExp (N n) = WFSAtom (Text.pack (show n))
fromExp (V x) = WFSAtom (Text.pack x)
fromExp (Plus e1 e2) = WFSList [WFSAtom "+", fromExp e1, fromExp e2]
fromExp (Times e1 e2) = WFSList [WFSAtom "*", fromExp e1, fromExp e2]

prettyPrint :: Exp -> Text
prettyPrint =
  S.encodeOne (S.setFromCarrier fromWellFormed (S.basicPrint id))
  . fromExp
{% endhighlight %}

## Resumo de parsing e pretty-printing de _s-expressions_
Aí está: com um pouco de boilerplat, você pode ter uma experiência similar à de
trabalhar com Lisp. Note que com um pouco de trabalho de Template Haskell
usando "quasiquotes", você poderia ir mais longe dos templates de texto que
criamos e tabém criar templates de "patterns"

Não tivemos que escrever um parser tradicional e fomos capazes de separar o que
era be formado do que precisava ser processado. Isso é muito útil em muitos
contextos: na minha experiência, a checagem de erros em vários níveis faz
mensagens de erros boas mais fáceis de serem implementadas. Além disso, algo
não discutido aqui é como _s-expressions_ podem ajudar com predicção e
_auto-completion_.

## Mais notas opcionais sobre a sintaxe para DSLs
Eu queria apontar que para muitos domínios de problemas, como esse que acontece
de ser matemático, as vezes é popular fazer as coisas parecerem matemáticas. Eu
apresentei tudo sem essa intenção primeiro, mas agora vou mostrar algumas
variações de sintaxe.

### Identificadores alfanuméricos como operadores
Primeiro, podemos usar a sintaxe de operadores com _backticks_ e níveis de
precedência:

{% highlight haskell %}
module SymbolicDifferentiation.AlphaOperatorSyntax where

-- | Variável em uma expressão
type Var = String

-- | Precedências para os construtores de expressão
infixl 6 `Plus`
infixl 7 `Times`

-- | Uma Expressão.
data Exp
  = N Int          -- ^ number
  | V Var          -- ^ variable
  | Plus Exp Exp   -- ^ sum
  | Times Exp Exp  -- ^ product
  deriving (Show, Eq)

-- | Derivada de uma expressão em respeito a uma variável.
deriv :: Exp -> Var -> Exp
deriv (N _)         _ = N 0
deriv (V v')        v = N (if v' == v then 1 else 0)
deriv (Plus e1 e2)  v = deriv e1 v `plus` deriv e2 v
deriv (Times e1 e2) v = e1 `times` deriv e2 v
                        `plus`
                        deriv e1 v `times` e2

-- | Precedências para nossos construtores de expressão inteligentes.
infixl 6 `plus`
infixl 7 `times`

-- | Construtor inteligente que simplifica ao combinar sub-expressões
plus :: Exp -> Exp -> Exp
N 0  `plus` e    = e
e    `plus` N 0  = e
N n1 `plus` N n2 = N (n1 + n2)
e1   `plus` e2   = e1 `Plus` e2

-- | Construtor inteligente que simplifica ao combinar sub-expressões
times :: Exp -> Exp -> Exp
N 0  `times` _    = N 0
_    `times` N 0  = N 0
N 1  `times` e    = e
e    `times` N 1  = e
N n1 `times` N n2 = N (n1 * n2)
e1   `times` e2   = e1 `Times` e2
{% endhighlight %}

Perceba que nada mudou de fato, exceto o uso e definição de funções infixas e o
uso de precedência para remover parenteses, como na grande expressão para a
derivativa de um produto de sub-expressões. Mas a clareza está começando a ser
perdida, para aqueles não familiares com o domínio do problema e suas
convenções.

### Identificadores simbólicos como operadores
Pode-se ir mais longe e usar identificadores simbólicos ao invés dos operadores
alfanuméricos com backticks. Isso é onde muitos de nós começamos a nos perder
no que está acontecendo:

{% highlight haskell %}
module SymbolicDifferentiation.OperatorSyntax where

-- | Variável em uma expressão
type Var = String

-- | Precedências para os construtores de expressão
infixl 6 :+:
infixl 7 :*:

-- | Uma Expressão.
data Exp
  = N Int        -- ^ number
  | V Var        -- ^ variable
  | Exp :+: Exp  -- ^ sum
  | Exp :*: Exp  -- ^ product
  deriving (Show, Eq)

-- | Derivada de uma expressão em respeito a uma variável.
deriv :: Exp -> Var -> Exp
deriv (N _)       _ = N 0
deriv (V x )      y = N (if x == y then 1 else 0)
deriv (e1 :+: e2) v = deriv e1 v .+. deriv e2 v
deriv (e1 :*: e2) v = e1 .*. deriv e2 v
                      .+.
                      deriv e1 v .*. e2

-- | Precedências para nossos construtores de expressão inteligentes.
infixl 6 .+.
infixl 7 .*.

-- | Construtor inteligente que simplifica ao combinar sub-expressões
(.+.) :: Exp -> Exp -> Exp
N 0  .+. e    = e
e    .+. N 0  = e
N n1 .+. N n2 = N (n1 + n2)
e1   .+. e2   = e1 :+: e2

-- | Construtor inteligente que simplifica ao combinar sub-expressões
(.*.) :: Exp -> Exp -> Exp
N 0  .*.  _   = N 0
_    .*. N 0  = N 0
N 1  .*. e    = e
e    .*. N 1  = e
N n1 .*. N n2 = N (n1 * n2)
e1   .*. e2   = e1 :*: e2
{% endhighlight %}

Dependendo do seu gosto pessoal, pode achar que essa sintaxe engraçadinha faz
os testes ficarem mais bonitos:

{% highlight haskell %}
spec :: Spec
spec =
  describe "diferenciação simbólica" $ do
    prop "d/dx (x + n) == 1" $ \x n ->
      deriv (V x :+: N n) x `shouldBe` N 1
    prop "d/dx (x + y) == x, if x /= y" $ \x y ->
      x /= y ==>
      deriv (V x :*: V y) x `shouldBe` V y
    prop "d/dx (a * x + b) == x" $ \a x b ->
      deriv (N a :*: V x :+: N b) x `shouldBe` N a
    it "d/dx (x * y * (x + 3)) == (x * y) + y * (x + 3)" $ do
      deriv (V "x" :*: V "y" :*: (V "x" :+: N 3)) "x" `shouldBe`
        (V "x" :*: V "y") :+: (V "y" :*: (V "x" :+: N 3))
{% endhighlight %}

Tudo isso é um pouco fofo se você está acostumado, mas eu acho que pode ser
muito estranho caso contrário; e eu sei que muitos não-Haskellers vendo isso
antes de ver a versão crua tem a ideia errada de que você precisa escrever as
coisas dessa forma em Haskell e viram as costas em desgosto e confusão. Isso é
porque eu mostrei a forma crua antes, e estou mostrando essa só para ilustrar
que há bibliotecas que vão tentar ser bem sugestivas no uso de operadores, e
também que não há nada de especial acontecendo: é só uma sintaxe diferente para
expressar exatamente a mesma coisa que na primeira versão, com a segunda versão
(operadores alfanuméricos com _backticks_) sendo um passo de transição em
direção a essa terceira versão. É bom saber as três variações, independente de
qual você prefere ler ou escrever.

Mas e a ideia de usar _s-expressions_, que era desacoplar o lado do usuário
(representado por testes) da sintaxe abstrata e do Haskell? Minha intuição é
que há benefícios em ter uma sintaxe alternativa usando _s-expressions_ para
qualquer outra sintaxe concreta disponível.

## Conclusão
_S-expressions_ são uma forma testada pelo tempo de representar dados. A
biblioteca `s-cargot` vem com muitas formas de criar parsers de _s-expressions_
customizados e pretty-printers e também vem com padrões úteis.

## Todo o código
Todo o código para a série estará [nesse repositório do GitHub](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).

- - -

### Nota do tradutor
Se você quer ajudar com esse tipo de coisa, agora é a hora. Entre no
[Slack](http://haskellbr.com/slack/) ou no
[IRC](http://irc.lc/freenode/haskell-br) da [HaskellBR](http://haskellbr.com/) e
contribua. Esse blog e outros projetos associados estão na
[organização `haskellbr` no GitHub](https://github.com/haskellbr) e em
[haskellbr.com/git](http://haskellbr.com/git).

[Há um milestone no GitHub com tarefas esperando por você.](https://github.com/haskellbr/blog/milestones/24%20dias%20de%20Hackage%202015).

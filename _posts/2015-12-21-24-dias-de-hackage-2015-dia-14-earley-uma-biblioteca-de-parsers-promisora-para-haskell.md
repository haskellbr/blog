---
layout: post
title: "24 dias de Hackage, 2015 - dia 14 - Earley: Uma biblioteca de parsers promisora para Haskell"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original.](http://conscientiousprogrammer.com/blog/2015/12/14/24-days-of-hackage-2015-day-14-earley-a-promising-newer-parser-library-for-haskell/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

## Encontro HaskellBR São Paulo
[Vamos nos encontrar em São Paulo em 25 de Janeiro de 2016. Marque sua presença
e comente se não puder vir.](http://www.meetup.com/haskellbr-sp/events/227526368/)

# Dia 14

No
[dia 10](/2015/12/17/24-dias-de-hackage-2015-dia-10-s-cargot-usando-a-sintaxe-de-s-expressions.html),
mostrei como usar _s-expressions_ para evitar ter de escrever um parser
customizado. Mas escrever parsers não é nada mal em Haskell; ou será que é?
A popular biblioteca `parsec`
[tem muitos problemas](http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/),
porque ela requere _backtracking_ hackeado à mão que causa mensagens de erro
estranhas e dificuldade de pensar sobre a sua gramática. Há um fork melhorado
do `parsec` chamado
[`megaparsec`](https://hackage.haskell.org/package/megaparsec),
mas ainda é o mesmo tipo de tecnologia. Que tal algo completamente diferente?

<!-- more -->

O recente pacote [`Earley`](https://hackage.haskell.org/package/Earley) é
intrigante e eu comecei a o usar para novos projetos onde eu não preciso do
poder monádico de algo como o `parsec` mas me contento com uma API de
_applicatives_ e não preciso da performance de algo como o `attoparsec`. Além
de boas mensagens de erro, ele lida bem com parsing _inline_ e ambiguidades.

Hoje vou dar dois exemplos pequenos de usar o `Earley`.

## Instalação
Já que o LTS Stackage está um pouco atrasado agora, e o `Earley` não para de
mudar, decidi usar a última versão do `Earley` modificando nosso `stack.yaml`:

{% highlight yaml %}
- Earley-0.10.1.0
{% endhighlight %}

## Parsing para a AST do dia 10
Vamos voltar para o problema de diferenciação simbólica do dia 10 e criar uma
sintaxe infixa de tipo matemático para _"parsear"_.

### Testes
Aqui estão alguns testes do HSpec/QuickCheck para ilustrar o que nós queremos
quando _"parseamos"_ uma `Exp` de uma string.

#### Imports
`Text.Earley` é o módulo principal do pacote `Earley`; `Report` é usado para
retornar um relátorio do progresso do _"parse"_.

{% highlight haskell %}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module SymbolicDifferentiation.EarleySpec where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))
import qualified SymbolicDifferentiation.Earley as Earley
import Text.Earley (Report(..))

import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative(..))

import Data.String.Here (i)
{% endhighlight %}

#### Testes do QuickCheck
Alguns testes do QuickCheck que mostram que expressões como
`x*a + y*b * (z+c)` _"parseiam"_ para a AST esperada:

{% highlight haskell %}
spec :: Spec
spec =
  describe "Custom syntax for expression parsed by Earley" $ do
    -- For simplicity, don't support negative numeric literals now.
    prop "x + a" $ \(NonNegative (a :: Int)) ->
      fst (Earley.parses [i|x + ${a}|]) `shouldBe`
        [Plus (V "x") (N a)]

    prop "x*a + y*b * (z+c)" $
      \(NonNegative (a :: Int))
       (NonNegative (b :: Int))
       (NonNegative (c :: Int)) ->
      fst (Earley.parses [i|x*${a} + y*${b} * (z+${c})|]) `shouldBe`
        [Plus (Times (V "x") (N a))
              (Times (Times (V "y") (N b))
                     (Plus (V "z") (N c)))]
{% endhighlight %}

#### Erros de parse esperados
Finalmente, um exemplo de como checar por erros de parse esperados. Os tokens
de erro são definidos pelo o usuário e adicionados à construção de gramáticas,
como veremos.

{% highlight haskell %}
    it "x + y * + 5" $
      Earley.parses "x + y * + 5" `shouldSatisfy`
        \case
          ([], Report { position = 8
                      , expected = ["number", "identifier", "("]
                      , unconsumed = "+ 5"
                      }) -> True
          _ -> False
{% endhighlight %}

### Implementação
A implementação envolve idiomas de `Applicative` que vão ser familiares se você
já usou o `parsec`.

#### Imports

{% highlight haskell %}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module SymbolicDifferentiation.Earley where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))

import qualified Text.Earley as E
import Text.Earley ((<?>))
import Control.Applicative (many, some, (<|>))
import qualified Data.Char as Char
import Control.Monad.ST (ST)
import Data.ListLike (ListLike)

-- | What to report for something expected.
type Expected = String
{% endhighlight %}

O operador `<?>` é usado para adicionar uma expectativa (que nós decidimos
especificar como uma string, com o sinônimo de tipo `Expected`) para um
_production_.

#### Drivers
O que queremos para nosso problema em particular é um parser que recebe uma
string como input e espera que possa fazer _parse_ completo dela. Construimos
ele a partir de um parser mais genérico que vem de processar nossa gramática.

{% highlight haskell %}
-- | Return a list of all possible `Exp` parses, and also a status report
-- regardless of how many successes.
parses :: String -> ([Exp], E.Report Expected String)
parses = E.fullParses expParser

-- | Parser created from the grammar.
expParser :: ListLike input Char =>
             ST state (input -> ST state (E.Result state Expected input Exp))
expParser = E.parser grammar
{% endhighlight %}

#### Gramática
Nossa gramática é bem direta. `Earley` usa uma mônada para manter seu estado
interno e nós usamos a extensão do GHC `RecursiveDo` (coberta em um
[dia de extensões do GHC de 2014](https://ocharles.org.uk/blog/posts/2014-12-09-recursive-do.html))
para conseguirmos nos referir para uma regra da gramática recursivamente. Note
que a recursão da esquerda para a direita não tem problemas no `Earley`.

`Prod` é o
[construtor de tipo para uma _production_](https://hackage.haskell.org/package/Earley-0.10.1.0/docs/Text-Earley.html#t:Prod)
e você constrói _productions_ usando combinadores como `satisfy` e `symbol`.

{% highlight haskell %}
-- | Basically taken from <https://github.com/ollef/Earley/blob/master/examples/Expr2.hs Earley example expression parser>
grammar :: forall r. E.Grammar r (E.Prod r Expected Char Exp)
grammar = mdo
  whitespace <- E.rule $
    many $ E.satisfy Char.isSpace

  let token :: E.Prod r Expected Char a -> E.Prod r Expected Char a
      token p = whitespace *> p

      sym x   = token $ E.symbol x <?> [x]

      ident   = token $ (:) <$> E.satisfy Char.isAlpha
                            <*> many (E.satisfy Char.isAlphaNum)
                            <?> "identifier"
      num     = token $ some (E.satisfy Char.isDigit) <?> "number"
      -- For now, just handle unsigned numeric literals.

  atom <- E.rule $
    (N . read) <$> num
    <|> V <$> ident
    <|> sym '(' *> term <* sym ')'

  factor <- E.rule $
    Times <$> factor <* sym '*' <*> atom
    <|> atom

  term <- E.rule $
    Plus <$> term <* sym '+' <*> factor
    <|> factor

  return $ term <* whitespace
{% endhighlight %}

Para mais exemplos de gramáticas veja o
[diretório de exemplos no repositório do GitHub do `Earley`](https://github.com/ollef/Earley/tree/master/examples).

### (Update of 2015-12-17)

O inverso de parsing é pretty-printing, coberto no [dia 17](/blog/2015/12/17/24-days-of-hackage-2015-day-17-ansi-wl-pprint-avoiding-string-hacking/).

_N.T. Não traduzido; link vai para o original_

## Só por diversão: resolvendo o problema "number words"

A habilidade de lidar com ambiguidades e retornar todos os parses possível é
algo útil em muitas situações. Aqui, mostro uma solução para o problema
["number word"](http://programmingpraxis.com/2014/07/25/number-words/).
No passado, eu lidei com ambiguidades usando o
[suporte GLR do Happy](https://www.haskell.org/happy/doc/html/sec-glr.html),
mas não gosto de escrever parsers com ele.

O problema "number word":

> Dado um inteiro positivo, retorne todas as formas que um inteiro pode ser
> representado por letras usando o mapa `1 -> A, 2 -> B, ..., 26 -> Z`. Por
> exemplo, o número 1234 pode ser representado pelas palavras `ABCD`, `AWD` e
> `LCD`.

Essa é uma versão de brincadeira de um problema sério, a
[segmentação em linguagens naturais](https://en.wikipedia.org/wiki/Text_segmentation).

### O teste

O teste reflete o enunciado do problema:

{% highlight haskell %}
module EarleyExampleSpec where

import EarleyExample (grammar, NumberWord, Expected)
import qualified Text.Earley as E
import qualified Data.List.NonEmpty as NonEmpty

import Test.Hspec (Spec, hspec, describe, it, shouldMatchList)

spec :: Spec
spec =
  describe "EarleyExample" $ do
    it "returns all possible parses of number words" $ do
      let (result, _) = parseNumberWord 1234
      map NonEmpty.toList result `shouldMatchList`
        ["ABCD", "AWD", "LCD"]

parseNumberWord :: Integer -> ([NumberWord], E.Report Expected String)
parseNumberWord = E.fullParses (E.parser grammar) . show
{% endhighlight %}

Note que estou usando uma lista `NonEmpty` de `Char`s porque uma string vazia
não é uma solução válida para o problema. (Cobri `NonEmpty` no
[dia 7](/2015/12/15/24-dias-de-hackage-2015-dia-7-semigroups-lista-nonempty-e-um-caso-de-estudo-de-tipos-e-testes.html)).

### Solução
A solução é só escrever uma gramática que tente escolher dígitos consecutivos
para formar uma letra. Nós criamos uma _production_ para cada letra que nos
importa, usando o `numberLetterFor`, combinamos todas elas por meio de
alternação usando `asum` e conseguimos uma _production_ composta, que é a
gramática.

{% highlight haskell %}
{-# LANGUAGE RecursiveDo #-}

module EarleyExample where

import qualified Text.Earley as E
import Text.Earley ((<?>))
import Control.Applicative ((<|>))
import qualified Data.Foldable as Foldable
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))

-- | Result wanted.
type NumberWord = NonEmpty NumberLetter

-- | 'A' to 'Z'.
type NumberLetter = Char

-- | What to report for something expected.
type Expected = String

grammar :: E.Grammar r (E.Prod r Expected Char NumberWord)
grammar = mdo
  numberWord <- E.rule $
    NonEmpty.cons <$> numberLetter <*> numberWord
    <|> (:| []) <$> numberLetter
  return numberWord

numberLetter :: E.Prod r Expected Char NumberLetter
numberLetter = (Foldable.asum . map numberLetterFor) ['A'..'Z'] <?> "number"

-- | Return a production for a given letter.
--
-- 1 is 'A', 2 is 'B', .. 26 is 'Z'.
numberLetterFor :: NumberLetter -> E.Prod r Expected Char NumberLetter
numberLetterFor c = c <$ E.word (show (toNumber c)) <?> [c]

-- | 'A' is 1, ... 'Z' is 26
toNumber :: NumberLetter -> Int
toNumber c = (Char.ord c - Char.ord 'A') + 1
{% endhighlight %}

## Conclusão
Só recentemente descobri a biblioteca de parsers `Earley` e comecei a
usá-la. Eu estou animado com o quão amigável é.

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

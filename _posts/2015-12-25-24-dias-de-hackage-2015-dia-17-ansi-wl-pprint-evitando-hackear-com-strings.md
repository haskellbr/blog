---
layout: post
title: "24 dias de Hackage, 2015 - dia 17 - ansi-wl-pprint: Evitando hackear com strings"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original.](http://conscientiousprogrammer.com/blog/2015/12/17/24-days-of-hackage-2015-day-17-ansi-wl-pprint-avoiding-string-hacking/)_

## Encontro HaskellBR São Paulo
[Vamos nos encontrar em São Paulo em 25 de Janeiro de 2016. Marque sua presença
e comente se não puder vir.](http://www.meetup.com/haskellbr-sp/events/227526368/)

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

## Dia 17
Hoje, fazemos o inverso do que fizemos no
[day 14](/2015/12/21/24-dias-de-hackage-2015-dia-14-earley-uma-biblioteca-de-parsers-promisora-para-haskell.html),
que foi _parsear_ de texto para uma árvore de sintaxe abstrata. Hoje nós
fazemos o _pretty-printing_ de uma árvore abstrata para texto.

<!-- more -->

Eu acho que no mundo mais amplo da programação, é muito comum ver soluções
inflexíveis e ad-hoc para esse problema, usando _string hacking_ e talvez, no
melhor dos casos, templates (baseados em _strings_). Tipicamente, é difícil
customizar estilos de indentação rápidamente, amarrar expressões e outras
funcionalidades como uma solução ad-hoc.

Na comunidade de Haskell, uma solução melhor é mais comum, por causa do número
de bibliotecas de qualidade para ajudar com _pretty-printing_.

[`ansi-wl-pprint`](https://hackage.haskell.org/package/ansi-wl-pprint)
é uma dessas bibliotecas, que incluí não só muitos combinadores úteis, mas
também expõe suporte para _output_ em cores, se você precisa disso (você não
precisa).

### Update
Um comentário no Reddit notou que há outra versão da biblioteca de
_pretty-printing_,
[`annotated-wl-pprint`](https://hackage.haskell.org/package/annotated-wl-pprint).
Isso parece ser muito legal, e há um
[video por David Christiansen sobre seu uso com Idris](https://www.youtube.com/watch?v=m7BBCcIDXSg).

## De volta a nosso tipo de expressão de exemplo
Lembre que nosso tipo de expressão é:

{% highlight haskell %}
-- | Variable in an expression.
type Var = String

-- | Expression.
data Exp
  = N Int          -- ^ number
  | V Var          -- ^ variable
  | Plus Exp Exp   -- ^ sum
  | Times Exp Exp  -- ^ product
{% endhighlight %}

## Alguns exemplos de output _pretty-printed_
Vamos fazer _pretty-print_ em uma expressão de exemplo.
(Na vida real, nós criaríamos um corpo de expressões para _pretty-print_ e as
verificaríamos, e também escreveríamos testes do QuickCheck para confirmar
várias propriedades.)

{% highlight haskell %}
{-# LANGUAGE QuasiQuotes #-}

module SymbolicDifferentiation.AnsiWlPprintSpec where

import qualified SymbolicDifferentiation.Earley as Earley
import qualified SymbolicDifferentiation.AnsiWlPprint as AnsiWlPprint

import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldSatisfy)

import Data.String.Here (hereLit)

spec :: Spec
spec =
  describe "anti-wl-pprint for symbolic differentiation expression" $ do
    let eString = [hereLit|(x*1) + 2*y + ((3+z) * (4*b)) * (5+d+w)|]
    let ([e], _) = Earley.parses eString
    it eString $ do
      show (AnsiWlPprint.prettyPrint e) `shouldBe`
        [hereLit|x*1 + 2*y + (3 + z)*4*b*(5 + d
+ w)|]
{% endhighlight %}

Para evitar o problema de construir um valor `Exp` na mão, estamos reusando
nosso parser.

Um exercício para o leitor: escreva um gerador do QuickCheck de expressões
randômicas, e escreva um teste que verifica que quando você passa uma expressão
aleatória `Exp` pelo _pretty-printer_ e de volta pelo parser para uma `Exp`,
você recebe o mesmo resultado!

O exemplo aqui formada somas separadas por espaços e produtos concatenados
juntos, com parênteses mínimos e quebras de linha opcionais.

## O código
O código é curto. A
[documentação Haddock para o `ansi-wl-pprint`](https://hackage.haskell.org/package/ansi-wl-pprint-0.6.7.3/docs/Text-PrettyPrint-ANSI-Leijen.html)
é excelente, então você pode procurar nela para uma explicação dos combinadores
usados.

O conceito fundamental por trás da biblioteca é que há um tipo de dados `Doc`
que representa um "documento", uma peça de informação _pretty-printed_, e que
há uma algebra combinando os documentos para os organizar de várias formas,
então você pode usar uma variedade de interpretadores sobre o documento para
realizar a conversão para string no final.

{% highlight haskell %}
module SymbolicDifferentiation.AnsiWlPprint where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))
import Text.PrettyPrint.ANSI.Leijen
       ( Doc
       , int, text, char
       , (</>), (<//>), (<+>), (<>)
       , parens
       )

-- | Very primitive, for illustration only!
--
-- Spaces for sums, but no spaces for products.
-- Soft breaks before operators.
prettyPrint :: Exp -> Doc
prettyPrint e = p 10 e

-- | Pretty-print inside a precedence context to avoid parentheses.
-- Consider + to be 6, * to be 7.
p :: Int -> Exp -> Doc
p _ (N n) = int n
p _ (V v) = text v
p prec (Plus e1 e2) = maybeParens (prec < 7)
  (p 7 e1 </> char '+' <+> p 7 e2)
p prec (Times e1 e2) = maybeParens (prec < 6)
  (p 6 e1 <//> char '*' <> p 6 e2)

maybeParens :: Bool -> Doc -> Doc
maybeParens True = parens
maybeParens False = id
{% endhighlight %}

Exercício para o leitor: Há muitas formas de melhorar o _pretty-printer_, por
exemplo alinhar os termos para ficar como:

{% highlight text %}
a + b*c
  + d*e
  + f*g
{% endhighlight %}

Eu provavelmente primeiro transformaria uma `Exp` em uma `SugaredExp` que
representa a sintaxe abstrata intermediária dos termos e fatores, e então
escrever um _pretty-printer_ sobre isso.

## Duplicação de trabalho?
Você deve se perguntar sobre a duplicação de trabalho em escrever o parser e o
_pretty-printer_. Não podemos escrever uma única especificação high-level para
fazer ambos simultâneamente? Sim, há ferramentas para isso, mas estão fora do
escopo desse artigo.

## Conclusão
_Pretty-printing_ é uma parte importante de uma interface baseada em texto,
seja para inspecionar código gerado ou criar boas mensagens de
erro. `ansi-wl-pprint` é uma boa biblioteca para usar para _pretty-printing_>

## Todo o código
Todo o código para a série estará [nesse repositório do GitHub](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).

- - -

### Nota do tradutor
Se você quer ajudar com esse tipo de coisa, agora é a hora. Entre no
[Slack](http://slack.haskellbr.com/) ou no
[IRC](http://irc.lc/freenode/haskell-br) da [HaskellBR](http://haskellbr.com/) e
contribua. Esse blog e outros projetos associados estão na
[organização `haskellbr` no GitHub](https://github.com/haskellbr) e em
[haskellbr.com/git](http://haskellbr.com/git).

[Há um milestone no GitHub com tarefas esperando por você.](https://github.com/haskellbr/blog/milestones/24%20dias%20de%20Hackage%202015).

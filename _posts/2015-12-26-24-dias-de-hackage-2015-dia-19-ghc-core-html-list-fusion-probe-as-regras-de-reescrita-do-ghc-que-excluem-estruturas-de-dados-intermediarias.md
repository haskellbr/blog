---
layout: post
title: "24 dias de Hackage, 2015 - dia 19 - ghc-core-html, list-fusion-probe: As regras de reescrita do GHC que excluem estruturas de dados intermediárias"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original.](http://conscientiousprogrammer.com/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](http://conscientiousprogrammer.com/blog/2015/12/19/24-days-of-hackage-2015-day-19-ghc-core-html-list-fusion-probe-checking-ghcs-fusion-rewrite-rules-for-erasing-intermediate-data-from-existence/).

## Encontro HaskellBR São Paulo
[Vamos nos encontrar em São Paulo em Janeiro de 2016. Marque sua presença e comente se não puder vir.](http://www.meetup.com/haskellbr-sp/events/227526368/)

[Vote se o encontro terá workshops, um dojo ou palestras.](https://plus.google.com/+DanielYokomizo/posts/bM1C5HRLcR4)

## Dia 19

([Discussão no reddit](https://www.reddit.com/r/haskell/comments/3xie9b/24_days_of_hackage_2015_day_19_ghccorehtml/))

A *funcionalidade mais legal* de usar Haskell, para mim, é o
[_fusion_](https://wiki.haskell.org/GHC_optimisations#Fusion). O compilador GHC
realiza essa otimização memorável, que pode apagar estruturas intermediárias
inteiras inteiras da existência.

O que isso quer dizer, e como podemos saber que aconteceu? Hoje vou mostrar
como o [`ghc-core-html`](http://hackage.haskell.org/package/ghc-core-html) e o
[`list-fusion-probe`](https://hackage.haskell.org/package/list-fusion-probe)
podem ajudar a determinar o que o compilador de fato fez com nossas estruturas
de dados intermediárias.

<!-- more -->

Vou dar exemplos usando listas como os dados intermediários, mas também
mencionar vetores porque ontem,
[no dia 18](/2015/12/25/24-dias-de-hackage-2015-dia-18-vector-vector-algorithms-libere-seu-alter-programador-c.html),
brevemente mencionei fusão no contexto de vetores.

## Imports para o código
Primeiro, vamos tirar os imports do caminho antes de mostrar alguns testes
HSpec e QuickCheck:

{% highlight haskell %}
{-# LANGUAGE ScopedTypeVariables #-}

module ListFusionProbeSpec where

import Data.List.Fusion.Probe (fuseThis)

import Test.Hspec ( Spec, hspec, describe, it
                  , shouldBe, shouldSatisfy
                  , shouldThrow, errorCall
                  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Function (Fun(..), apply)
import Control.Exception (evaluate)
import Control.Arrow ((>>>))
import Data.Function ((&))
{% endhighlight %}

## O que são dados intemediários em uma pipeline?
Quando estamos programando de forma composicional, frequentemente criamos
pipelines de fluxo de dados, onde dados em um estágio são transformados em
dados para o próximo e assim por diante até o resultado final. O problema é que
uma implementação ingênua de um pipeline vai resultar na construção de alguma
estrutura de dados intermediária que existe somente para ser consumida pelo o
próximo estágio.

Aqui está um teste HSpec ilustrando uma pipeline onde nós *nomeamos cada lista
intermediária*:

{% highlight haskell %}
spec :: Spec
spec =
  describe "list-fusion-probe" $ do
    it "runs a chain of maps, filters" $
      let list1 = ["Hello", "my", "world!"]

          -- ["Hello", "world!"]
          list2 = filter ((> 2) . length) list1

          -- [5, 6]
          list3 = map length list2

          list4 = map (*3) list3
      in list4 `shouldBe` [15, 18]
{% endhighlight %}

Em uma implementação típica de uma linguagem de programação típica, código que
parece com esse vai resultar em *alocar* e *criar* quatro listas (ou arrays, ou
qualquer que seja o tipo para coleções idiomático e desejado), um depois do
outro, e percorrendo três listas usando um _filter_ e dois _maps_. Para fazer
algo mais inteligente e evitar criar estruturas de dados intermediárias,
podemos usar um tipo de dados _"stream"_, ou mais radicalmente,
[_"transducers"_](http://clojure.org/transducers). Essas técnicas nos permitem
comprimir a computação da estrutura de dados resultante em uma única
percorrida, sem alocações ou dados intermediários.

O ecossistema do Haskell contém muitas funções para realizar esses tipos de
otimizações, como o
[`foldl`](http://hackage.haskell.org/package/foldl),
mas elas estão fora do escopo desse artigo. Ao invés disso, para exemplos
simples como o acima, o GHC já *automaticamente* aplica fusão, por meio de
[regras de reescrita](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/rewrite-rules.html)
especiais, inclusas nas bibliotecas padrão.

### Uma nota na sintaxe para pipelines e composição
Podemos escrever pipeslines de muitas formas diferentes. Aqui está uma forma
estilo _OO_ usando `&` (aplicação reversa de funções) e `>>>` (composição da
esquerda para a direita):

{% highlight haskell %}
    it "runs a chain of maps, filters (OO-style)" $
      let list4 = ["Hello", "my", "world!"] &
                     (filter (length >>> (> 2))
                      >>> map length
                      >>> map (*3)
                     )
      in list4 `shouldBe` [15, 18]
{% endhighlight %}

Aqui está a mesma coisa mas com a composição estraída. Essa é minha forma
preferida de escrever pipelines como valores de primeira classe:

{% highlight haskell %}
    it "runs a chain of maps, filters, written compositionally with >>>" $
      let pipeline = filter (length >>> (> 2))
                     >>> map length
                     >>> map (*3)
      in pipeline ["Hello", "my", "world!"] `shouldBe` [15, 18]
{% endhighlight %}

Usando a composição tradicional `(.)`:

{% highlight haskell %}
    it "runs a chain of maps, filters, written compositionally with ." $
      let pipeline = map (*3)
                     . map length
                     . filter ((> 2) . length)
      in pipeline ["Hello", "my", "world!"] `shouldBe` [15, 18]
{% endhighlight %}

## O que quer dizer "reescrita"?

A coisa que reescritas fazem que outras abordagens não: a reescrita é um passe
de preprocessamento do compilador que *reescreve seu código* para o
otimizar. Nós não vamos entrar exatamente em como isso funciona. Há regras de
reescrita que encontrar alguns construtos e os substituem com construtos
semanticamente equivalentes, e basicamente a fusão acontece quando, durante o
processo de reescrever algo repetidamente, alguns construtos se cancelam, e
*"poof"* seus dados intermediários se foram.

Se você olhar o código fonte para o
[`map`](https://hackage.haskell.org/package/base-4.8.1.0/docs/src/GHC.Base.html#map)
você vai ver alguns comentários intimidadores e diretivas para o compilador que
se parecem com:

{% highlight haskell %}
map :: (a -> b) -> [a] -> [b]
{-# NOINLINE [1] map #-}    -- We want the RULE to fire first.
                            -- It's recursive, so won't inline anyway,
                            -- but saying so is more explicit
map _ []     = []
map f (x:xs) = f x : map f xs

-- Note eta expanded
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
{-# INLINE [0] mapFB #-}
mapFB c f = \x ys -> c (f x) ys

-- The rules for map work like this.
--
-- Up to (but not including) phase 1, we use the "map" rule to
-- rewrite all saturated applications of map with its build/fold
-- form, hoping for fusion to happen.
-- In phase 1 and 0, we switch off that rule, inline build, and
-- switch on the "mapList" rule, which rewrites the foldr/mapFB
-- thing back into plain map.
--
-- It's important that these two rules aren't both active at once
-- (along with build's unfolding) else we'd get an infinite loop
-- in the rules.  Hence the activation control below.
--
-- The "mapFB" rule optimises compositions of map.
--
-- This same pattern is followed by many other functions:
-- e.g. append, filter, iterate, repeat, etc.

{-# RULES
"map"       [~1] forall f xs.   map f xs                = build (\c n -> foldr (mapFB c f) n xs)
"mapList"   [1]  forall f.      foldr (mapFB (:) f) []  = map f
"mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g)
  #-}
{% endhighlight %}

### Alguns recursos sobre fusão

Aqui estão alguns artigos ilustrando o funsionamento de baixo-nível da fusão:

- [por Chris Done](http://chrisdone.com/posts/stream-composability)
- [por Jan Stolarek](http://lambda.jstolarek.com/2013/04/haskell-as-fast-as-c-a-case-study/)

## E como saber se a fusão funcionou?
Algo que sempre me incomodou é que não há uma forma fácil de saber se a fusão
funcionou de fato ou dizer ao compilador, "eu quero que a fusão ocorra sobre o
resultado dessa expressão, então imprima um erro se você não puder fazer algo,
porque eu preciso de eficiência máxima aqui". Por exemplo, há um
[ticket aberto](https://ghc.haskell.org/trac/ghc/ticket/8763) sobre algo que
alguém esperava que fosse _"fundido"_.

Quando estou escrevendo código de alto nível e esperando que o compilador faça
otimizações para mim, eu considero importante que seja notificado quando algo
não funciona como esperado.

Então foi interessante quando eu encontrei a biblioteca `list-fusion-probe` que
aplica seus próprios macetes de reescrita para determinar se uma lista foi
_fundida_ ou não. A forma de a usar com uma expressão com tipo de lista é só a
amarrar em uma chamada para `fuseThis` e a biblioteca vai reescrever a chamada
de forma que se a fusão acontecer, só a lista é retornada, mas senão, ela gera
código que vai atirar uma exceção durante runtime. Isso é extremamente
primitivo (eu nunca gostaria de botar código que atira uma exceção "cannot
fuse" em produção), mas é uma prova de conceito útil para testes.

Então aqui estão alguns testes ilustrando como a inserção de `fuseThis` em
pipelines verifica que a fusão *de fato* acontece (porque não há nenhuma
exceção em runtime):

{% highlight haskell %}
    it "fuses a chain of maps, filters" $
      let list1 = ["Hello", "my", "world!"]

          -- ["Hello", "world!"]
          list2 = fuseThis $ filter ((> 2) . length) list1

          -- [5, 6]
          list3 = fuseThis $ map length list2

          list4 = map (*3) list3
      in list4 `shouldBe` [15, 18]
    it "fuses a chain of maps, filters" $
      let list4 = ["Hello", "my", "world!"]
          pipeline = filter (length >>> (> 2)) >>> fuseThis
                     >>> map length >>> fuseThis
                     >>> map (*3)
      in pipeline list4 `shouldBe` [15, 18]
    it "Prelude foldl fuses" $
      let list = fuseThis [0..1001] :: [Int]
      in foldl (+) 0 list `shouldBe` 501501
{% endhighlight %}

Nós verificamos que a `list2` e a `list3` nunca existiram como dados
"materializados".

Note, por exemplo, que a fusão de exemplo do `foldl` resulta na lista
`[0..1001]` nunca ser criada; ao invés disso, o código de máquina gerado é
basicamente um loop com índice de `0` a `1001` adicionando um acumulador.

Aqui está uma falha de fusão:

{% highlight haskell %}
    it "handwritten myFoldl fails to fuse" $
      let list = fuseThis [0..1001] :: [Int]
      in evaluate (myFoldl (+) 0 list) `shouldThrow`
           errorCall "fuseThis: List did not fuse"
{% endhighlight %}

Aqui estamos passando `myFoldl` escrito de tal forma que ele não bate com as
regras de reescrita:

{% highlight haskell %}
-- | This example taken straight from `list-fusion-probe` tests directory.
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f = go
  where go a [] = a
        go a (x:xs) = go (f a x) xs
{% endhighlight %}

Finalmente, por diversão, vamos escrever um teste QuickCheck que testa
predicados arbitrários e funções de transformação em um pipeline `filter` e
`map`:

{% highlight haskell %}
    prop "Prelude foldl fuses the result of a filter, map pipeline" $
      \(list :: [Int]) (predicate :: Fun Int Bool) (f :: Fun Int Int) ->
      let pipeline = fuseThis . filter (apply predicate)
                     . fuseThis . map (apply f)
      in
         -- Just to force evaluation.
        foldl (+) 0 (pipeline list) `shouldSatisfy` (<= maxBound)
{% endhighlight %}

## Porque não relatar que uma fusão falhou em tempo de compilação?
Há um problema de interface nas otimizações como a fusão. Idealmente, eu
gostaria de declarar a fusão desejada e ver uma falha de compilação.

No entanto, a situação é difícil, porque o código pode ficar frágil no meio de
configurações otimização diferentes. Por exemplo, se você roda os testes acima
no GHCi, você vai ver que o GHCi nunca relata que uma fusão falhou. Isso é
porque ele simplesmente não está fazendo nenhuma fusão: só ignora as regras de
reescrita, tanto na biblioteca padrão e em `list-fusion-probe`.

Talvez a solução é ter diretivas condicionais. Eu acho que há uma opção de
tratar um bug conhecido de performance como um erro de compilação. Não é
prático viver com medo e de forma defensiva quando estamos escrevendo código
que deve ter boa performance e decidindo se, na falta de feeback do compilador,
devemos escrever código de baixo nível feio, porque não confiamos que o código
elegante vai funcionar propriamente.

O que você acha? Que tipo de feedback você gostaria de receber do compilador
sobre otimizações que você espera que sejam realizadas?

## Checando código gerado no exemplo de `vector` de ontem
Ontem, mencionei que a fusão com vetores acontece, mas eu não estava
completamente confiante em quanto dela de fato acontece. Eu encontrei uma
ferramenta útil `ghc-core-html` que imprime uma versão bonitinha do código de
GHC Core que o GHC gera.

Para o usar, o instale globalmente:

{% highlight console %}
$ stack install ghc-core-html
{% endhighlight %}

Eu o rodei em um arquivo com o código fonte modificado para receber o resultado
em HTML:

{% highlight console %}
$ stack exec ghc-core-html src/VectorFusionExample.hs > VectorFusionExample.html
{% endhighlight %}

O arquivo de exemplo:

{% highlight haskell %}
-- | Extracted from VectorExample for minimal GHC Core output file.
module VectorFusionExample (makeCountsPurely) where

import qualified Data.Word as Word
import qualified Data.Vector.Unboxed as V

-- | Assume 8-bit unsigned integer.
type Value = Word.Word8

-- | Number of occurrences of an 8-bit unsigned value.
-- We assume no overflow beyond 'Int' range.
type CountOfValue = Int

-- | NOT the real thing!! With fake constants 123, 789
-- to help in reading the GHC Core.
makeCountsPurely :: V.Vector Value -> V.Vector CountOfValue
makeCountsPurely =
  V.unsafeAccumulate (+) (V.replicate numPossibleValues 123)
  . V.map (\v -> (fromIntegral v, 789))

-- | 256, in our case.
numPossibleValues :: Int
numPossibleValues = fromIntegral (maxBound :: Value) + 1
{% endhighlight %}

Não vou mostrar a saída, mas só comentar que quando estava olhando para ela, eu
vi duas chamadas para `newByteArray#`. Já que um dos arrays criados é o array
final, mas há uma outra alocação, nós não conseguimos chegar no ponto de criar
só um array sem alocar nenhum outro, diferente da nossa implementação de ontem
usando um `MVector` interno.

Isso é bom saber, porque eu não deveria ter que gerar código de baixo-nível e o
inspecionar para ter que ter informação sobre onde a fusão acontece e com o quê.

[[_N.T. Veja o output aqui_]](/content/2015-12-26-24-dias-de-hackage-2015-dia-19-ghc-core-html-example.html)

## Para mais informação

Um tutorial sobre o processo de compilação do GHC está
[aqui](http://blog.ezyang.com/2011/04/tracing-the-compilation-of-hello-factorial/).
Aqui está [outro](http://www.haskellforall.com/2012/10/hello-core.html).

Um
[vídeo por Simon Peyton Jones sobre Core](https://skillsmatter.com/skillscasts/6495-keynote-from-simon-peyton-jones).

Leia qualquer e toda coisa por
[Johan Tibell](http://blog.johantibell.com/) sobre performance com Haskell.

## Conclusão
Hoje eu mencionei a importância de fusão e minha frustração de não ser capaz de
conseguir o tipo de informação detalhada que eu gostaria sobre se o GHC
realizou fusões onde eu gostaria. No entanto, `list-fusion-probe` ajuda em
escrever testes para detectar quando a fusão de listas falha e `ghc-core-html`
é uma útil ferramenta de propósito geral para examinar para examinar o código
gerado.

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

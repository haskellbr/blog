---
layout: post
title: "24 dias de Hackage, 2015 - dia 18 - vector, vector-algorithms: Libere seu alter-programador C"
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

## Dia 18

([Discussão no Reddit](https://www.reddit.com/r/haskell/comments/3xdqg8/24_days_of_hackage_2015_day_18_vector/))

É comum em programação, que a estrutura de dados a ser usada é um array estilo
C, seja ele tratado como mutável ou imutável.

Idealmente, eu não teria que escrever esse artigo, mas eu tenho a impressão de
que muitos recem chegados ao Haskell não sabem sobre o
[`vector`](http://hackage.haskell.org/package/vector), a biblioteca para
arrays estilo C, e usam listas onde arrays se encaixariam melhor no
problema. Arrays são eficientes para _indexar_ e também são eficientes quanto
ao cache da CPU e a memória, sendo alocados em um bloco de memória, ao invés de
espalhados por meio de um monte de ponteiros.

<!-- more -->

A terminologia usada para arrays no ecossistema do Haskell é confusa porque
Haskell, nos anos 90, originalmente veio com uma estrutura de dados chamada
`Array`, e há até um pacote
[`array`](https://hackage.haskell.org/package/array),
mas na prática eu nunco o uso porque ele é mais genérico e estranho que a
estrutura de dados simples exposta mais tarde chamada "vetores" _[N.T. "vectors"]_
(pela falta de um nome melhor).

Há bons tutoriais sobre o `vector` que eu recomendo:

- [Wiki](https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial)
- [Grupo Commercial Haskell](https://github.com/commercialhaskell/haskelldocumentation/blob/master/content/vector.md)

Achei que seria útil dar um exemplo completo de usar vetores imutáveis e
mutáveis. Em particular, vamos usar vetores *unboxed* que são especializados e,
portanto, não contém nenhuma indireção e são basicamente equivalentes aos
arrays estilo C. Vamos fazer um pouco de programação de baixo nível estilo C.

## O problema a ser resolvido: encontrar a mediana de um array de inteiros de 8-bits

Aqui está um problema
[encontrado no _Programming Praxis_](http://programmingpraxis.com/2015/09/11/finding-the-median/):

> A mediana de um array é o valor no meio se o array estivesse ordenado:
> se o array tiver um número impar de items `n`, a mediana é o
> `(n+1)/2`nth maior item no array (que também é o `(n+1)/2`nth menor item no
> array), e se o array tiver um número par de items `n`, a mediana é a média
> aritmética do `n/2`th menor item e do `n/2`th maior item no array. Por
> exemplo, a mediana do array `[2,4,5,7,3,6,1]` é `4` e a mediana do array
> `[5,2,1,6,3,4]` é `3.5`.
>
> Sua tarefa é escrever um programa que recebe um array de inteiros de 8-bits
> (possivelmente mas não necessariamente ordenados) e encontra o valor da
> mediana do array; você deve encontrar um algoritmo que tome *tempo linear* e
> *espaço constante*.

## Instalação do `vector`
O Stackage LTS ainda está no `vector-0.10`, mas eu queria usar a
[versão 0.11](https://hackage.haskell.org/package/vector-0.11.0.0) porque a API
mudou um pouco, então eu adicionei uma restrição de versão apropriada ao
arquivo Cabal.

## A importância de ir *unboxed*
Já que temos que escrever código específico a um array de inteiros de 8-bits,
vamos usar vetores
[*unboxed*](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/primitives.html)
ao invés de *boxed* (genéricos): vetores *boxed* contém elementos que são
ponteiros para valores, enquanto os *unboxed* tem elementos que são os próprios
valores. Isso é importante para a eficiência, se você sabe de cara qual é o
valor dos seus elementos.

## Alguns testes
Vamos escrever alguns testes antes de mais nada, assumindo que vamos escrever
um módulo `VectorExample`.

Primeiro, alguns imports. Nós vamos usar o operador de _indexing_ em tempo
constante para vetores `!`.

{% highlight haskell %}
module VectorExampleSpec where

import VectorExample
       ( Value, MedianValue, averageValue
       , spaceInefficientMedian, constantSpaceMedian
       )

import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Radix as Radix
{% endhighlight %}

Vamos usar HSpec e QuickCheck, e escrever nossos próprios geradores de vetores
aleatórios:

{% highlight haskell %}
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(arbitrary, shrink), choose, sized, shrinkList)
{% endhighlight %}

Vamos implementar tanto uma versão simples de referência e a inteligente
desejada, então vamos estar ambas:

{% highlight haskell %}
spec :: Spec
spec = do
  describe "compute median of vector of 8-bit unsigned integers" $ do
    medianSpec "inefficientSpaceMedian" inefficientSpaceMedian
    medianSpec "constantSpaceMedian" constantSpaceMedian
{% endhighlight %}

Testamos os exemplos fornecidos, assim como propriedades genéricas. Para
ordenamento, nós convenientemente usamos a ordenação _radix_ do pacote
[`vector-algorithms`](http://hackage.haskell.org/package/vector-algorithms):

{% highlight haskell %}
medianSpec :: String
            -> (V.Vector Value -> Maybe MedianValue)
            -> Spec
medianSpec description findMedian =
  describe description $ do
    describe "some examples" $ do
      it "handles odd number of elements" $ do
        findMedian (V.fromList [2, 4, 5, 7, 3, 6, 1]) `shouldBe` Just 4.0
      it "handles nonzero even number of elements" $ do
        findMedian (V.fromList [5, 2, 1, 6, 3, 4]) `shouldBe` Just 3.5
    describe "properties" $ do
      it "handles no elements" $ do
        findMedian V.empty `shouldBe` Nothing
      prop "handles one element" $ \v ->
        findMedian (V.singleton v) == Just (fromIntegral v)
      prop "handles odd number of elements" $
        \(VectorWithOdd values) ->
          let len = V.length values
              midIndex = pred (succ len `div` 2)
              sorted = V.modify Radix.sort values
          in findMedian values == Just (fromIntegral (sorted ! midIndex))
      prop "handles positive even number of elements" $
        \(VectorWithPositiveEven values) ->
          let len = V.length values
              midIndex = pred (succ len `div` 2)
              sorted = V.modify Radix.sort values
          in findMedian values ==
            Just (averageValue (fromIntegral (sorted ! midIndex))
                               (sorted ! succ midIndex))
{% endhighlight %}

Os testes de propriedades especificam o que a mediana deve ser, para todos os
casos possíveis.

### Uma nota sobre mutação
A ordenação _radix_ opera sobre vetores mutáveis, mas estamos usando um vetor
[função `modify`](http://hackage.haskell.org/package/vector-0.11.0.0/docs/Data-Vector-Unboxed.html#v:modify)
que tem um tipo pomposo:

{% highlight haskell %}
modify :: Unbox a => (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
{% endhighlight %}

Ela recebe um callback que recebe um vetor mutável e transforma um vetor
imutável em outro vetor imutável (possivelmente o mesmo vetor mutado
"secretamente" por baixo dos panos). Usa tipos de alta ordem e o
[monad transformer de estado `ST`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Monad-ST.html),
mas tudo o que você tem de saber é que
[`Radix.sort`](http://hackage.haskell.org/package/vector-algorithms-0.7.0.1/docs/Data-Vector-Algorithms-Radix.html#v:sort)
tem um tipo compatível de callback e pode portanto ser composto com `modify`:

{% highlight haskell %}
sort :: forall e m v. (PrimMonad m, MVector v e, Radix e) => v (PrimState m) e -> m ()
{% endhighlight %}

### Geradores customizados do QuickCheck
Aqui estão alguns geradores rápidos de vetores do tipo que queremos para os
testes. Se estivéssemos fazendo isso de verdade, provavelmente gostariamos de
escrever _shrinkers_ customizados, ao invés de percorrer listas.

{% highlight haskell %}
-- | Gene um vetor com um número impar de elementos.
newtype VectorWithOdd a =
  VectorWithOdd { getVectorWithOdd :: V.Vector a }
  deriving (Show)

instance (Arbitrary a, V.Unbox a) => Arbitrary (VectorWithOdd a) where
  arbitrary = sized $ \n -> do
    k <- choose (0, n `div` 2)
    VectorWithOdd <$> V.replicateM (2*k+1) arbitrary
  shrink = map (VectorWithOdd . V.fromList)
           . shrinkList shrink
           . V.toList
           . getVectorWithOdd

-- | Gere um vetor não vazio com um número par de elementos.
newtype VectorWithPositiveEven a =
  VectorWithPositiveEven { getVectorWithPositiveEven :: V.Vector a }
  deriving (Show)

instance (Arbitrary a, V.Unbox a) => Arbitrary (VectorWithPositiveEven a) where
  arbitrary = sized $ \n -> do
    k <- choose (1, 1 `max` (n `div` 2))
    VectorWithPositiveEven <$> V.replicateM (2*k) arbitrary
  shrink = map (VectorWithPositiveEven . V.fromList)
           . shrinkList shrink
           . V.toList
           . getVectorWithPositiveEven
{% endhighlight %}

## Uma versão ineficiente que viola o requisito de espaço
Para referência (nós podíamos quase ler a partir dos testes do QuickCheck):

{% highlight haskell %}
{-# LANGUAGE BangPatterns #-}

module VectorExample where

import qualified Data.Word as Word
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Algorithms.Radix as Radix

-- | Assuma um inteiro unsigned de 8-bits
type Value = Word.Word8

-- | O que retornar como a mediana, se existir
type MedianValue = Double

-- | A implementação óbvia; basicamente a especificação. O uso da ordenação
-- radix toma tempo linear, mas uma cópia do array original é necessária.
inefficientSpaceMedian :: V.Vector Value -> Maybe MedianValue
inefficientSpaceMedian values
  | V.null values = Nothing
  | odd len = Just (fromIntegral (atSorted midIndex))
  | otherwise = Just (averageValue (atSorted midIndex)
                                   (atSorted (succ midIndex)))
  where
    len = V.length values
    midIndex = pred (succ len `div` 2)

    -- Faz uma cópia local mutável para ordenar
    atSorted = V.unsafeIndex (V.modify Radix.sort values)

-- | A média de dois valores
averageValue :: Value -> Value -> MedianValue
averageValue a b = (fromIntegral a + fromIntegral b) / 2
{% endhighlight %}

### Uma nota sobre operações _unsafe_
Eu prometi que iríamos fazer programação estilo C. Poderíamos usar _indexing_
com um _bounds-check_ (que atira uma exceção durante runtime quando acessamos
elementos que não estão em um vetor), mas vamos nos "divertir" um pouco, e usar
o `unsafeIndex`. Note que isso é inseguro mesmo: vamos pegar o centésimo
elemento de um vetor com 1 `Int`:

{% highlight haskell %}
Data.Vector.Unboxed> unsafeIndex (singleton (5 :: Int)) 100
4931475852
{% endhighlight %}

Porque usar operações _unsafe_? Para ilustração e também porque acontece de que
podemos provar (fora do _type system_) que nós nunca vamos tentar acessar um
elemento fora do vetor. Na vida real, eu hesitaria em usar operações _unsafe_ a
não ser que ter a performance máxima fosse crítico! Mas *nós podemos nos
candidatar ao mundo de desastres em potencial do C se quisermos*.

**Não quero promover usar operações _unsafe_!!** Substitua todas as funções
de hoje chamadas `unsafeXyz` por `xyz` para ficar tranquilo.

## Uma solução inteligente
A função inteligente aloca uma tabela de todas as contegens possíveis (porque
sabemos que os valores são inteiros _unsigned_ de 8-bits) e a preenche, então
itera pela tabela e descobre onde a mediana deve estar, e quando bate no lugar
certo, retorna o valor do meio (se tiver um número impar de valores) ou o meio
de dois valores (se tiver um número par de valores).

{% highlight haskell %}
-- | O número de ocorrências de um inteiro de 8-bits unsigned.
-- Assumimos que não overflow além dos limites de um 'Int'.
type CountOfValue = Int

-- | Create a table of counts for each possible value, since we know
-- the number of values is small and finite.
-- | Crie uma tabela de contagens para cada valor possível, já que sabemos que
-- o número de valores é pequeno e finito.
constantSpaceMedian :: V.Vector Value -> Maybe MedianValue
constantSpaceMedian values
  | V.null values = Nothing
  | odd len = Just (findMid 0 (numToCount - countOf 0))
  | otherwise = Just (findMid2 0 (numToCount - countOf 0))
  where
    len = V.length values

    -- Quantos elementos ordenados que precisamos contar, para chegar ao
    -- primeiro valor mediano.
    numToCount = succ len `div` 2

    -- Crie a tabela de contagens para cada valor possível.
    countOf = V.unsafeIndex (makeCountsMutably values)

    findMid !i !numRemaining
      | numRemaining <= 0 = fromIntegral i
      | otherwise = findMid (succ i) (numRemaining - countOf (succ i))

    findMid2 !i !numRemaining =
      case numRemaining `compare` 0 of
        LT -> fromIntegral i -- median is duplicated, don't need average
        GT -> findMid2 (succ i) (numRemaining - countOf (succ i))
        EQ -> midAverage (succ i) (countOf (succ i))
          where
            midAverage j 0 = midAverage (succ j) (countOf (succ j))
            midAverage j _ = averageValue (fromIntegral i) (fromIntegral j)
{% endhighlight %}

Note o uso da extensão do GHC `BangPatterns`, coberta em um
[dia de extensões do GHC de 2014](https://ocharles.org.uk/blog/posts/2014-12-05-bang-patterns.html),
para nos assegurarmos da avaliação estrita em loops recursivos "pela cauda"
(_tail-recursive loops_).

### Formas mutáveis e imutáveis de criar a tabela de contagens
O que é `makeCountsMutably`?

#### Mutável
A versão da função que usa mutação usa a
[função `create`](http://hackage.haskell.org/package/vector-0.11.0.0/docs/Data-Vector-Unboxed.html#v:create):

{% highlight haskell %}
-- | Use local mutation for efficiency in creating a table of counts,
-- looping through to update it, and freezing the result to return.
-- | Use mutabilidade local para eficiência em criar uma tabela de contagens,
-- fazendo um laço para a atualizar e congelando o valor retornado.
makeCountsMutably
  :: V.Vector Value        -- ^ values seen
  -> V.Vector CountOfValue -- ^ value => count
makeCountsMutably values = V.create $ do
  counts <- M.replicate numPossibleValues 0
  V.forM_ values $
     M.unsafeModify counts succ . fromIntegral
  return counts

-- | 256, no nosso caso.
numPossibleValues :: Int
numPossibleValues = fromIntegral (maxBound :: Value) + 1
{% endhighlight %}

`create` tem tipo:

{% highlight haskell %}
create :: Unbox a => (forall s. ST s (MVector s a)) -> Vector a
{% endhighlight %}

E inteligentemente "congela" o vetor retornado no lugar, dentro do contexto do
monad `ST`, em um vetor imutável, o que evita qualquer cópia, porque a mutação
acontece dentro do callback e não é visível fora dele.

#### Imutável
Mas espere, também há uma forma de criar a tabela usando somente `Vector`s
imutáveis, sem nenhum vetor `MVector` intermediário!

{% highlight haskell %}
-- | Crie a tabela de contagens sem usar 'MVector'.
makeCountsPurely
  :: V.Vector Value
  -> V.Vector CountOfValue
makeCountsPurely =
  V.unsafeAccumulate (+) (V.replicate numPossibleValues 0)
  . V.map (\v -> (fromIntegral v, 1))
{% endhighlight %}

Isso é elegante de direto, mas *parece criar um vetor intermediário* usando
`map`, que não é o que queremos já que todo o ponto é ter eficiência de espaço.

##### Fusion

Acontece que o `vector` vem com sofisticado
[suporte para fusion](https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial#A_note_on_fusion).

Um dia de Hackage posterior (agora disponível no
[dia 19](http://conscientiousprogrammer.com/blog/2015/12/19/24-days-of-hackage-2015-day-19-ghc-core-html-list-fusion-probe-checking-ghcs-fusion-rewrite-rules-for-erasing-intermediate-data-from-existence/))
vou esplicar o que acontece nos bastidores. Efetivamente, o GHC reescreve as
chamadas para `unsafeAccumulate` e `map` para evitar o vetor intermediário!

## Conclusão
Arrays de tamanho fixo são uma peça importante para a construção muitas
computações. Uso o `vector` quando preciso fazer operações (sequenciais) sobre
arrays. Espero que esse pequeno exemplo mostre como escrever código imperativo
baseado em vetores em Haskell, escondendo a maior quantidade de estado
possível, por meio de APIs orientadas a tipos que operam entre os dois mundos
da mutabilidade e imutabilidade. É até possível escrever código que dependa no
_fusion_ e não crie vetores mutáveis intermediários.

(Esses são arrays sequenciais. Para arrays paralelos, veja um
[dia de Hackage de 2013](https://ocharles.org.uk/blog/posts/2013-12-16-24-days-of-hackage-repa.html),
que cobriu o [`repa`](http://hackage.haskell.org/package/repa), uma biblioteca
para "arrays paralelos de alta performance, regulares, multi-dimensionais e
polimórficos".)

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

---
layout: post
title: "24 dias de Hackage, 2015 - dia 8 - multiset; gostaria que isso estivesse no pacote padrão"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Hanneli T.
translator_url: "https://github.com/hannelita"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original](http://conscientiousprogrammer.com/blog/2015/12/08/24-days-of-hackage-2015-day-8-multiset-i-wish-this-were-in-the-standard-containers-package/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

# Dia 8

Não me lembro quando foi, mas um dia fiquei cansado de reimplementar, em Haskell, a mesma lógica clichê para rastrear múltiplos objetos sob a mesma chave. Essa estrutura de dados é conhecida por [multiset](https://en.wikipedia.org/wiki/Multiset), ou *bag* (N.T. alguns livros de estrutura de dados em Português podem traduzir para _balde_), em contraste com um *set* regular, que rastreia apenas um único objeto por chave.

<!-- more -->

Fiquei surpreso com a ausência de um módulo para multiset nos [`containers`](https://hackage.haskell.org/package/containers) do pacote padrão, embora seja possível implementar um multiset com base em um set, então é razoável que essa estrutura seja "supérflua". Entretanto, estava acostumado a ter a estrutura de multiset pronta, sem nenhum trabalho (mesmo que trivial), devido ao constante uso da [estrutura no C++](http://www.cplusplus.com/reference/set/multiset/), onde voltava aos anos 1990 para checar a implementação original da [Standard Template Library](https://en.wikipedia.org/wiki/Standard_Template_Library), e também a biblioteca de collections do Python possui o [`Counter`](https://docs.python.org/2/library/collections.html#collections.Counter), classe que serve para propósito similar.

Hoje brevemente mostrarei um código que utiliza o `multiset` e falarei sobre por que uma estrutura assim talvez devesse estar na biblioteca padrão.

## O exemplo clássico de uso do multiset

Multisets geralmente são usados para contar coisas. Contadores de palavras são exemplos consagrados de uso, então vamos implementar um simples contador de palavras que quebra um texto em palavras ("words") separadas por espaços, e, tratando elementos diferentes de letras como espaços, conta as palavras e finalmente apresenta uma saída que consiste numa linha para cada palavra e sua contagem, em ordem descendente de número mas em ordem ascendente de palavra.

Para um pouco mais de realismo e eficiência mesmo para um projeto demonstrativo, não utilizemos o input como `String`, mas como sendo do tipo `Text`, a partir do módulo `Data.Text.Lazy` do pacote [`text`](https://hackage.haskell.org/package/text). Vamos construir também um relatório final como `Text`, igualmente eficiente. 

### Uma amostra de teste com HSpec

Temos aqui um teste (por diversão talvez você queira escrever alguns geradores com QuickCheck e uma suite de testes para esse problema). Peço desculpas pela quase ilegível String de múltiplas linhas utilizada; gostaria que o Haskell possuísse uma estrutura para strings de múltiplas linhas, interpolação e todos esses outros manipuladores úteis que outras linguagens possuem.

#### (Tangenciando o dia 9)

Algumas pessoas comentaram que fiz um comentário errôneo anteriormente sobre o
suporte a strings em Haskell. É fato que a sintaxe do core é limitada, mas é
fácil contornar isso com o Template Haskell, portanto dê uma olhada no
[Dia 9](/2015/12/16/24-dias-de-hackage-2015-dia-9-pontos-interessantes-do-template-haskell.html)
para ver como utilizá-lo. Recomendo fortemente o use de alguma das bibliotecas
mencionadas.

Brevemente, a horrenda string poderia ser escrita como:

{% highlight haskell %}
      let expected = [hereLit|words 3
I 2
have 2
so 2
for 1
like 1
many 1
to 1
|]
{% endhighlight %}

#### O código

{% highlight haskell %}
{-# LANGUAGE OverloadedStrings #-}

module MultisetExampleSpec where

import MultisetExample (wordCount)

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "multiset" $ do
    it "wordCount" $ do
      let input = "I have so   many words; words I so like to have words for!?"
      let expected = "words 3\n\
                     \I 2\n\
                     \have 2\n\
                     \so 2\n\
                     \for 1\n\
                     \like 1\n\
                     \many 1\n\
                     \to 1\n"
      wordCount input `shouldBe` expected
{% endhighlight %}

### A solução

O código praticamente se descreve como uma linguagem natural para o problema.

{% highlight haskell %}
{-# LANGUAGE OverloadedStrings #-}

module MultisetExample where

import qualified Data.MultiSet as MultiSet
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyBuilder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Ord (Down(..))
import qualified Data.Char as Char
import qualified Data.List as List
import Control.Arrow ((>>>))
import Data.Monoid ((<>))

wordCount :: LazyText.Text -> LazyText.Text
wordCount =
  LazyText.map replaceNonLetterWithSpace
  >>> LazyText.words
  >>> MultiSet.fromList
  >>> MultiSet.toOccurList
  >>> List.sortOn (snd >>> Down)
  >>> map summarizeWordCount
  >>> mconcat
  >>> LazyBuilder.toLazyText

replaceNonLetterWithSpace :: Char -> Char
replaceNonLetterWithSpace c
  | Char.isLetter c = c
  | otherwise = ' '

summarizeWordCount :: (LazyText.Text, MultiSet.Occur) -> LazyBuilder.Builder
summarizeWordCount (word, count) =
  LazyBuilder.fromLazyText word <> " " <> decimal count <> "\n"
{% endhighlight %}

Vamos analisar passo a passo. Note que estamos usando meu operador de composição favorito no estilo left-to-right (esquerda para a direita), o `>>>`, em oposição ao operador right-to-left `.` (direita para a esquerda), pois o `>>>` apresenta-se de forma muito mais natural para pipelines, e também porque minha descrição é feita do começo ao fim, combinando com a sintaxe.

#### (Atualização na sintaxe)

Aqui está o trechod e código com a sintaxe tradicional right-to-left (direita para esquerda). Acho que é importante estar apto a ler e escrever em ambos os estilos:

{% highlight haskell %}
-- | Mesma coisa, utilizando o estilo tradicional de composição right-to-left .
wordCountTraditional :: LazyText.Text -> LazyText.Text
wordCountTraditional =
  LazyBuilder.toLazyText
  . mconcat
  . map summarizeWordCount
  . List.sortOn (Down . snd)
  . MultiSet.toOccurList
  . MultiSet.fromList
  . LazyText.words
  . LazyText.map replaceNonLetterWithSpace
{% endhighlight %}

### Coletando as palavras do texto

Primeiramente, substitua o que for diferente de letras por espaços em branco, atendendo ao propósito desse problema, e então utilize o `words` do `Data.Text.Lazy` (o qual importamos como `LazyText`), para assim podermos separar as palavras (uma tokenização real teria regras mais sofisticadas). A propósito, isso é feito de forma bem eficiente oirque o GHC performa o [fusion](https://hackage.haskell.org/package/text-1.2.1.3/docs/Data-Text-Lazy.html#g:1) a fim de substituir e separar as palavras em uma única etapa, em vez de duas tal qual escrevemos no código. Fusion é uma otimização muito, muito legal para a qual confiamos nosso código Haskell se tornar super eficiente:

{% highlight haskell %}
wordCount =
  LazyText.map replaceNonLetterWithSpace
  >>> LazyText.words
{% endhighlight %}

### Colocando as palavras em um multiset

Na sequência, colocamos a lista de palavras em um multiset. `MultiSet` convenientemente possui um construtor para isso, [`fromList :: Ord a => [a] -> MultiSet a`](https://hackage.haskell.org/package/multiset-0.3.0/docs/Data-MultiSet.html#v:fromList), e ele é executado em tempo `O(n log n)` devido à comparação através de `Ord` para construir uma árvore (exercite isso caso queira: use alguma das bibliotecas de tabela Hash no Hackage para criar um `MultiHashSet`).

{% highlight haskell %}
  >>> MultiSet.fromList
{% endhighlight %}

#### Contando as palavras

Coletamos a lista de pares de palavras/quantidade (em ordem ascendente de palavras) utilizando [`toOccurList :: MultiSet a -> [(a, Occur)]`](https://hackage.haskell.org/package/multiset-0.3.0/docs/Data-MultiSet.html#v:toOccurList), onde [`type Occur = Int`](https://hackage.haskell.org/package/multiset-0.3.0/docs/Data-MultiSet.html#t:Occur):

{% highlight haskell %}
  >>> MultiSet.toOccurList
{% endhighlight %}

#### Ordenando a lista de palavras/quantidade

Nós ordenamos utilizando a ["Transformada de Schwartzian"](https://en.wikipedia.org/wiki/Schwartzian_transform), eficientemente usando [`Data.List.sortOn`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html#v:sortOn) do tipo `sortOn :: Ord b => (a -> b) -> [a] -> [a]`; recorde-se que queremos que a quantidade (o segundo elemento da tupla que obtemos do multiset) seja descendente.

A documentação do [`Data.Ord.Down`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Ord.html#t:Down) explica como esse "newtype hack" é usado para mudar a comparação padrão durante a ordenação.

{% highlight haskell %}
  >>> List.sortOn (snd >>> Down)
{% endhighlight %}

#### Gerando um relatório para cada palavra/quantidade

{% highlight haskell %}
  >>> map summarizeWordCount
  >>> mconcat
{% endhighlight %}

onde nossa função resumo, para eficiência em evitar alocações desnecessárias, não constrói um `Text` mas sim um [builder](https://en.wikipedia.org/wiki/Builder_pattern), dado que não precisamos dessa informação para cada palavra como `Text`, mas planejamos combinar os relatórios para todas as linahs como um único `Text` no final.

{% highlight haskell %}
summarizeWordCount :: (LazyText.Text, MultiSet.Occur) -> LazyBuilder.Builder
summarizeWordCount (word, count) =
  LazyBuilder.fromLazyText word <> " " <> decimal count <> "\n"
{% endhighlight %}

(Se você está curioso(a) sobre como o builder funciona, dê uma olhada no [código fonte](https://hackage.haskell.org/package/text-1.2.1.3/docs/src/Data-Text-Internal-Builder.html). Utilizam-se tipos higher-rank, operações unsafe e anotações strict).

#### Gerando o relatório final

Apenas materialize o `Text` a partir do builder:

{% highlight haskell %}
  >>> LazyBuilder.toLazyText
{% endhighlight %}

## O que deveria entrar na biblioteca padrão?

Mencionei que acho que isso deveria ir para a biblioteca padrão por conveniência. O argumento para **não** colocar algo assim como padrão é sempre o de todo mundo acha que seu código super útil deveria ir para lá também, mas não há meios para isso acontecer e escolhas devem ser feitas. Dado isso, não posso discutir baseado na minha experiência e percepção próprias de que a comunidade como um todo teria benefícios se meus feitos fossem padronizados. Gostaria de pensar que temos tecnologia para coletar dados reais que determinam as necessidades concretas das pessoas e mudam o modo descobrimos e utilizamos bibliotecas. Mesmo algo como as perguntas do Stack Overflow poderiam trazer estatísticas interessantes sobre o que as pessoas necessitam. Tal coleta de dados não substituiria um especialista, claro, mas certamente seria útil.

Há um outro argumento para não ter algo como o multiset incorporado na biblioteca padrão: é fácil implementá-lo uma vez que você tenha um set regular. Se você olhar o [código fonte](https://hackage.haskell.org/package/multiset-0.3.0/docs/src/Data-MultiSet.html#MultiSet), notará que basicamente um multiset é apenas um mapa de uma chave para uma contagem ou quantidade.

{% highlight haskell %}
newtype MultiSet a = MS { unMS :: Map a Occur }

type Occur = Int
{% endhighlight %}

Todo resto é apenas um wrapper bem direto.

Mas isso é como dizer que a linguagem não deveria ter nenhum facilitador sintático ("syntactic sugar"). Acho que o termo "library sugar" é tão valioso quanto "syntactic sugar".

## Conclusão

Multisets são úteis. Você pode implementá-los ou utilizar o pacote `multiset` que alguma outra pessoa já escreveu para nós.

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

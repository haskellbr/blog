---
layout: post
title: "24 dias de Hackage, 2015 - dia 21 - hood, GHood, Hoed: Debugging orientado à observação no Haskell"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original.](http://conscientiousprogrammer.com/blog/2015/12/21/24-days-of-hackage-2015-day-21-hood-ghood-hoed-observation-oriented-debugging-in-haskell/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](http://conscientiousprogrammer.com/blog/2015/12/19/24-days-of-hackage-2015-day-19-ghc-core-html-list-fusion-probe-checking-ghcs-fusion-rewrite-rules-for-erasing-intermediate-data-from-existence/).

## Encontro HaskellBR São Paulo
[Vamos nos encontrar em São Paulo em Janeiro de 2016. Marque sua presença e comente se não puder vir.](http://www.meetup.com/haskellbr-sp/events/227526368/)

[Vote se o encontro terá workshops, um dojo ou palestras.](https://plus.google.com/+DanielYokomizo/posts/bM1C5HRLcR4)

# Dia 21

([Discussão no Reddit](https://www.reddit.com/r/haskell/comments/3xqaak/24_days_of_hackage_2015_day_21_hood_ghood_hoed/))

Como você _debugga_ seu código Haskell?

Eu tenho que confessar de cara que não tenho uma boa resposta para a questão de
como _debuggar_ Haskell.

Não sou a pessoa certa para falar sobre _debuggers_, porque a última vez que
usei uma ferramenta de debugging oficial foi quando estava desenvolvendo C e
C++ e usava ferramentas como o `gdb` e interfaces de mais alto-nível para ele,
e desde então, meu processo de _debugging_ para muitas linguagens tem envolvido
olhar a _stack traces_  e logs, inserir expressões "print", escrever testes
melhores e refatorar o código para encontrar a causa do problema. Eu não uso
mais aplicações de _debugging_ oficiais tanto assim (com _breakpoints_,
_stepping_, etc.). Mas será que eu deveria?

<!-- more -->

A questão fica ainda mais complicada quando estamos trabalhando em Haskell,
porque eu acho que é honesto dizer que Haskell
[não tem uma boa história com _debugging_](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#debugging).

Eu dei uma olhada em uma família de ferramentas de _debugging_ para o haskell,
incluindo [`hood`](http://ku-fpg.github.io/software/hood/)
[`GHood`](http://community.haskell.org/~claus/GHood/), e
[`Hoed`](https://wiki.haskell.org/Hoed), todos baseados no mesmo conceito:
anotações manuais no código fonte, de forma que _traces_ mais úteis possam ser
gerados e analisados de formas interessantes.

## Hood

Vamos dar uma olhada no [`hood`](http://ku-fpg.github.io/software/hood/) antes,
que significa "Haskell Object Observation Debugger". É tudo sobre observação
por meio da _type-class_ `Observable a` e uma função de instrumentação
[`observe`](https://hackage.haskell.org/package/hood-0.3/docs/Debug-Hood-Observe.html#v:observe):

{% highlight haskell %}
observe :: Observable a => String -> a -> a
{% endhighlight %}

### Efeitos colaterais secretos!

**Atenção!** Apesar da sua assinatura de tipo, essa função realiza efeitos por
usando o `unsafePerformIO`, então você deve tomar cuidado quanto a como
escrever código que usa o `observe`, para conseguir os traces que quer.

### Exemplo de instrumentação de uma pipeline
Vamos instrumentar a pipeline de contagens de palavras do
[dia 8 ](/2015/12/16/24-dias-de-hackage-2015-dia-8-multiset-queria-que-estivesse-na-biblioteca-padrao.html).
Nós importamos `Debug.Hood.Observe` e copiamos e colamos o código original com
modificações para inserir chamadas para o `observe`.

{% highlight haskell %}
{-# LANGUAGE OverloadedStrings #-}

module HoodExample where

-- | hood
import Debug.Hood.Observe (Observable(..), observe, observeBase, printO)

import qualified Data.MultiSet as MultiSet
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyBuilder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Ord (Down(..))
import qualified Data.Char as Char
import qualified Data.List as List
import Control.Arrow ((>>>))
import Data.Monoid ((<>))

-- | Break up text into "words" separated by spaces, while treating
-- non-letters as spaces, count them, output a report line for each
-- word and its count in descending order of count but ascending order
-- of word.
wordCount :: LazyText.Text -> LazyText.Text
wordCount = observe "(1) wordCount"
  >>> LazyText.map replaceNonLetterWithSpace >>> observe "(2) map replaceNonLetterWithSpace"
  >>> LazyText.words             >>> observe "(3) LazyText.words"
  >>> MultiSet.fromList          >>> observe "(4) MultiSet.fromList"
  >>> MultiSet.toOccurList       >>> observe "(5) MultiSet.toOccurList"
  >>> List.sortOn (snd >>> Down) >>> observe "(6) List.sortOn (snd >>> Down)"
  >>> map summarizeWordCount     >>> observe "(7) map summarizeWordCount"
  >>> mconcat                    >>> observe "(8) mconcat"
  >>> LazyBuilder.toLazyText     >>> observe "(9) LazyBuilder.toLazyText"

replaceNonLetterWithSpace :: Char -> Char
replaceNonLetterWithSpace c
  | Char.isLetter c = c
  | otherwise = ' '

summarizeWordCount :: (LazyText.Text, MultiSet.Occur) -> LazyBuilder.Builder
summarizeWordCount (word, count) =
  LazyBuilder.fromLazyText word <> " " <> decimal count <> "\n"
{% endhighlight %}

(Peço desculpas pelo boilerplate sem sentido na descrição tipo `String` de cada
ponto de observação: poderíamos escrever um wrapper usando Template Haskell, eu
imagino, se desejado)

Aqui eu escolhi só instrumentar os "estágios" naturais dos dados na
pipeline. Se quisesse, também podería instrumentar qualquer outro nível, e.g. o
*resultado de chamar* `summarizeWordCount` (com `summarizeWordCount >> observe
"summarizeWordCount"`), ou muitos outros níveis poderosos, tais como
instrumentar uma função, não só o resultado de uma chamada a uma função, e.g.
`observe "summarizeWordCountfunction" summarizeWordCount`, que resulta na
coleção de todas as chamadas para a função.

### Implementando uma type-class `Observable`
Irritantemente, porque tudo é baseado na _type-class_ `Observable`, ignoramos
avisos do GHC e criamos
[instâncias órfãs](https://wiki.haskell.org/Orphan_instance)
para várias tipos (a alternativa, adicionando um wrapper `newtype` em todos os
lugares é um pouco trabalhosa nessa situação):

{% highlight haskell %}
-- | Some orphan instances of 'Observable'.
instance Observable LazyText.Text where
  observer = observeBase
instance (Observable a, Show a) => Observable (MultiSet.MultiSet a) where
  observer = observeBase
instance Observable LazyBuilder.Builder where
  observer = observeBase
{% endhighlight %}

### Saída de exemplo
Para simplicidade, vamos usar `printO :: Show a => a -> IO ()` para executar
de forma que um _trace_ seja impresso. Mais genéricamente, há um `runO` que
executa uma ação `IO` arbitrária.

{% highlight haskell %}
exampleRun :: IO ()
exampleRun = printO $
  wordCount "I have all-too-many words; words I don't like much!"
{% endhighlight %}

A saída (eu só rodei isso no GHCi):

{% highlight console %}
-- (1) wordCount
  "I have all-too-many words; words I don't like much!"
-- (2) map replaceNonLetterWithSpace
  "I have all too many words  words I don t like much "
-- (3) LazyText.words
   "I" :  "have" :  "all" :  "too" :  "many" :  "words" :  "words" :  "I" :
  "don" :  "t" :  "like" :  "much" : []
-- (4) MultiSet.fromList
  fromOccurList [("I",2),("all",1),("don",1),("have",1),("like",1),("many",1),("much",1),("t",1),("too",1),("words",2)]
-- (5) MultiSet.toOccurList
   ("I", 2) :  ("all", 1) :  ("don", 1) :  ("have", 1) :  ("like", 1) :
  ("many", 1) :  ("much", 1) :  ("t", 1) :  ("too", 1) :  ("words", 2) : []
-- (6) List.sortOn (snd >>> Down)
   ("I", 2) :  ("words", 2) :  ("all", 1) :  ("don", 1) :  ("have", 1) :
  ("like", 1) :  ("many", 1) :  ("much", 1) :  ("t", 1) :  ("too", 1) : []
-- (7) map summarizeWordCount
   "I 2\n" :  "words 2\n" :  "all 1\n" :  "don 1\n" :  "have 1\n" :
  "like 1\n" :  "many 1\n" :  "much 1\n" :  "t 1\n" :  "too 1\n" : []
-- (8) mconcat
  "I 2\nwords 2\nall 1\ndon 1\nhave 1\nlike 1\nmany 1\nmuch 1\nt 1\ntoo 1\n"
-- (9) LazyBuilder.toLazyText
  "I 2\nwords 2\nall 1\ndon 1\nhave 1\nlike 1\nmany 1\nmuch 1\nt 1\ntoo 1\n"
{% endhighlight %}

Isso definitivamente pode ser uma forma útil de _debuggar_ pipelines, ou só
gerar _traces_ para fins educacionais. Por exemplo, aqui nós podemos ver
imediatamente que não recebemos a resposta que queríamos (classificar "don't"
como uma palavra) porque no terceiro estágio, nós recebemos "don", o que
significa que algo deu errado nos estágios anteriores. No caso só havia um
outro, a remoção de caracteres que *não fizessem parte de palavras* - como o
`'`.

Há muito mais que podemos fazer com o `hood`, mas isso dá um gostinho.

## GHood
Agora, nós mudamos o olhar brevemente para o
[`GHood`](http://community.haskell.org/~claus/GHood/), que tem a mesma
interface com `Observable` do `hood`, exceto que nós importamos `Debug.Observe`
ao invés de `Debug.Hood.Observe`. Não vou mostrar código de exemplo porque ele
é literalmente só copiar/colar o código antigo e mudar o import. Problemas com
cópia e cola como essa (que também apareceram no
[dia 15 sobre `IOSpec`](/2015/12/23/24-dias-de-hackage-2015-dia-15-iospec-testando-io-e-algumas-dicas-para-o-quickcheck.html))
de fato me fazem desejar que o Haskell tivesse módulos parametrizados como o
ML. (E o problema com a instância orfã de `Observable` também, onde módulos
estilo ML são a forma natural de _plugar_ formas diferentes de _observar_ o
mesmo tipo de dados como for desejado).

`GHood` é um _backend_ gráfico escrito em Java para o `hood`. Quando você
instala o `GHood`, ele vem com um arquivo Java JAR. O Java lê um arquivo de
logs `ObserveEvents.log` que é gerado a partir do código instrumentalizado pelo
`hood`. Você pode animar, dar pulos para trás e para frente e os _traces_ são
mostrados em uma estrutura de árvore, incluindo mostrar a avaliação dos
thunks. É uma prova-de-conceito interessante de 2001, mas é um pouco
primitiva. Eu decidi não tentar incluir um _screenshot_ de uma execução de
exemplo, porque a saída do app Java Swing vai bem para a direita da janela e
não é muito customizável.

## Hoed
[`Hoed`](https://wiki.haskell.org/Hoed) tem uma API baseada no `hood` mas
é um projeto muito mais moderno, sofisticado e ativo. Ele permite
"debugging algorítmo" fornecendo uma aplicação web interativa, onde usa uma
estrutura de árvore para perguntar repetidamente ao usuário se os resultados
estão corretos, para isolar e identificar a fonte do erro baseado no seu
feedback.

Além disso, ele vem incluso com suporte para QuickCheck e _debugging_ baseado
propriedades. Confira a
[documentação e screenshots](https://wiki.haskell.org/Hoed).

`Hoed` começa um servidor web local que você pode acessar com um browser
[na porta 10000](http://localhost:10000/). O repositório no GitHub contém
um monte de
[exemplos](https://github.com/MaartenFaddegon/Hoed/tree/master/examples).

Infelizmente, meu tempo acabou antes de que eu conseguisse o fazer funcionar da
forma que eu queria para mostrar como um sistema de _debugging_ interativo
aqui, então, o que eu posso dizer é que o `Hoed` é muito interessante e que
planejo ir mais fundo nisso quando tiver o tempo. Vou atualizar esse post mais
tarde.

## Conclusão
Eu não usei muitas ferramentas de _debugging_ nos útilmos anos, mas eu gostaria
de mudar isso na medida em que encontrar formas de usar novas ferramentas que
sejam fáceis de usar. Sistemas baseados no `hood` parecem úteis como formas de
coletar informação durante a execução sem ter que reestruturar o código
radicalmente.

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

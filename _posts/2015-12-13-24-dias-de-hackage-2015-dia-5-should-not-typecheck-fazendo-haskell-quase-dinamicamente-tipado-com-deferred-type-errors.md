---
layout: post
title: "24 dias de Hackage, 2015 - dia 5 - should-not-typecheck: Fazendo Haskell quase dinamicamente tipado com deferred-type-errors"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---

_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original.](http://conscientiousprogrammer.com//blog/2015/12/05/24-days-of-hackage-2015-day-5-should-not-typecheck-making-haskell-sort-of-dynamically-typed-with-deferred-type-errors/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

# Dia 5

Você já se frustrou usando uma linguagem staticamente tipada, porque havia um
erro em algum lugar da sua code base, mas você queria poder rodar o programa de
qualquer jeito, ou porque não se importava com um erro que não tinha nada a ver
com o que estava fazendo, ou porque queria poder andar pelo código e debugar o
que o erro era de fato? Eu certamente já.

<!-- more -->

Já quis escrever um teste unitário para verificar que a code base não permitia
código que você não queria permitir, mas foi frustrado porque como você escreve
código em uma linguagem tipada que testa se algo vai dar _typecheck_, se a
linguagem não nos deixa rodar código que não vai dar _typecheck_?

Bem-vindo a terra dos
["deferred type errors"](https://ghc.haskell.org/trac/ghc/wiki/DeferErrorsToRuntime)
do GHC, uma função que faz parte do GHC [desde a versão 7.6.1](https://downloads.haskell.org/~ghc/7.6.1/docs/html/users_guide/defer-type-errors.html)
publicada em 2013. Como isso não foi coberto na
[série "24 Dias de Extensões do GHC" 2014](https://ocharles.org.uk/blog/pages/2014-12-01-24-days-of-ghc-extensions.html)
do Ollie, eu decidi tratar disso aqui, no contexto do pacote
[`should-not-typecheck`](https://hackage.haskell.org/package/should-not-typecheck)
que se integra com o HSpec para fazer asserções de que algo não vai dar
_typecheck_.

- - -

## Instalação

Como o LTS não incluí esse pacote obscuro, o Stack ons diz o que adicionar ao
nosso `stack.yaml` para o instalar:

{% highlight yaml %}
extra-deps:
- should-not-typecheck-2.0.1
{% endhighlight %}

## Vamos escrever alguns testes

A documentação completa do `should-not-typecheck` está
[na sua página do Hackage](https://hackage.haskell.org/package/should-not-typecheck).

Primeiro, nós precisamos ativar a opção do GHC `-fdefer-type-errors` no módulo
de testes com uma diretiva:

{% highlight haskell %}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{% endhighlight %}

### Nosso primeiro teste

{% highlight haskell %}
import ShouldNotTypecheckExample

import Test.Hspec ( Spec, hspec, describe, it
                  , shouldBe
                  , shouldThrow, anyException
                  )
import Test.ShouldNotTypecheck (shouldNotTypecheck)

spec :: Spec
spec =
  describe "should-not-typecheck" $ do
    it "should not allow mapping negation over a list of strings" $ do
      shouldNotTypecheck (map not ["hello", "world"])
{% endhighlight %}

Isso é alto explicativo. Nós não podemos executar a negação booleana (`not`)
sobre uma lista de strings. Haskell não é uma linguagem baseada em
"truthy"-ness, mas em "truth".

## Um pouco de código criptico

Vamos olhar para módulo `ShouldNotTypecheckExample`:

{% highlight haskell %}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module ShouldNotTypecheckExample (thisWorks, thisFails) where

thisWorks :: String
thisWorks =
  fst ("hello", ["world" / True, "!"])

thisFails :: String
thisFails =
  snd ("hello", ["world" / True, "!"])
{% endhighlight %}

Pare por um momento e pense no que vai acontecer quando `thisWorks` e
`thisFails` forem usados, como e porque. Nos dois casos, nós temos uma tupla e
retornamos um de seus elementos. O segundo elemento claramente não tem tipos
corretos, porque contem algo que não faz sentido (divisão de uma string por um
boolean).

## O papel da avaliação preguiçosa

Para entender o que acontece nos testes a seguir, você tem de entender como a
avaliação preguiçosa funciona em Haskell. A palavra "preguiçosa" é usada para
muitas ideias e construções diferentes em outras linguagens de programação, mas
a "avaliação preguiçosa" do Haskell é única. Uma discussão completa está fora
do escopo desse artigo, mas eu achei que mostrar o que acontece com os
"deferred type errors" _[N.T. erros de tipos atrasados]_ pode ser a porta de
entrada para entender o modelo de execução do Haskell.

### Nunca alcançado

{% highlight haskell %}
    it "you can run code even if it contains ill-typed parts" $ do
      thisWorks `shouldBe` "hello"
{% endhighlight %}

Isso funciona porque tuplas em Haskell são preguiçosas e então pegar o primeiro
elemento de uma tupla funciona independente do tipo do secondo elemento. A
diferença quando estamos operando com "deferred type errors", a tupla nem
precisa ser corretamente tipada e o segundo elemento pode ser um lixo completo,
como ele é aqui. Então esse exemplo é claro se você considerar que o que o GHC
faz com essa opção é isolar os erros para um contexto relativamente restrito
para que as coisas fora do contexto de um erro ainda funcionem normalmente.

### "Laziness all the way down"

O que acontece se nós tentamos calcular o tamanho do segundo elemento da tupla
(que é um "lixo completo")?

{% highlight haskell %}
    it "deferred type errors are only lazily reached" $ do
      length thisFails `shouldBe` 2
{% endhighlight %}

Tudo continuará funcionando normalmente, porque a lista dentro da tupla
preguiçosa é uma lista preguiçosa (porque listas são preguiçosas em Haskell) e
`length` nunca olha nos elementos da lista, só conta que eles existem e quantos
são. Então ele passa pela parte que não faz sentido (`"world" / True`) sem
precisar que ela seja avaliada.

### Forçando a avaliação

Para forçar explicitamente que uma estrutura de dados seja completamente
avaliada, precisamos usar o pacote
[`deepseq`](https://hackage.haskell.org/package/deepseq). Seu trabalho é
avaliar algo profundamente e completamente em Haskell! Usamos
a função
[`force`](https://hackage.haskell.org/package/deepseq-1.4.1.2/docs/Control-DeepSeq.html#v:force)
do pacote.

Para capturar, no HSpec, a exceção que nós esperamos receber, também precisamos do
[`evaluate`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Exception.html#v:evaluate)
de `Control.Exception`, parte do
[`base`](https://hackage.haskell.org/package/base), o pacote principal do
ecossistema (discutido
em um [post Dia de Hackage, 2012](https://ocharles.org.uk/blog/posts/2012-12-23-24-days-of-hackage-base.html)).

{% highlight haskell %}
import Control.Exception (evaluate)
import Control.DeepSeq (force)
{% endhighlight %}

Nosso teste fica (por simplicidade captura qualquer exceção, não só o problema
específico tipagem):

{% highlight haskell %}
    it "deferred type errors cause an exception only when reached" $ do
      evaluate (force thisFails) `shouldThrow` anyException
{% endhighlight %}

A avaliação vai percorrer toda a estrutura até a expressão sem sentido na lista
dentro da tupla do exemplo e um erro vai ser atirado em run time, como
esperado.

Suponha que só estamos avaliando `thisFails` no código, por exemplo no
GHCi. Obtemos:

{% highlight console %}
*Main> import ShouldNotTypecheckExample
*Main ShouldNotTypecheckExample> thisFails
"*** Exception: /Users/chen/Sync/haskell/twenty-four-days2015-of-hackage/src/ShouldNotTypecheckExample.hs:14:26:
    No instance for (Fractional Char) arising from a use of ‘/’
    In the expression: "world" / True
    In the expression: ["world" / True, "!"]
    In the first argument of ‘snd’, namely
      ‘("hello", ["world" / True, "!"])’
(deferred type error)
{% endhighlight %}

## Haskell não está sendo dinâmico

Haskell é dinâmicamente tipado quando rodamos com essa flag? Não
exatamente. Ele está fingindo. O que estamos fazendo é que o _typechecker_
encontra o erro em tempo de compilação e substitui o código sem sentido com uma
chamada para atirar o erro. Os detalhes técnicos estão
[nesse paper](http://dreixel.net/research/pdf/epdtecp.pdf).

Isso é completamente diferente de tipagem dinâmica onde nada é checado a tempo
de compilação e o erro aparece durante o run time. Aqui o erro é descoberto de
cara, mas escondido debaixo do tapete até/se for preciso o atirar.

## Mais sobre avaliação preguiçosa e forçosa

O livro grátis do Simon Marlow
["Parallel and Concurrent Programming in Haskell"](http://chimera.labs.oreilly.com/books/1230000000929)
tem capítulos sobre estratégias de avaliação, começando pelo [capítulo 2](http://chimera.labs.oreilly.com/books/1230000000929/ch02.html#sec_par-eval-sudoku2). Essas coisas são sutis.

_[N.T. Esse livro é muito bom, recomendo fortemente após uma introdução à linguagem]_

## Conclusão

Para o dia 5, introduzi o pacote `should-not-typecheck` e discuti brevemente a avaliação preguiçosa e como interage com os "deferred type errors" do GHC. Outro dia de Hackage vai entrar no mundo de fazer tipagem dinâmica "de fato" em Haskell.

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

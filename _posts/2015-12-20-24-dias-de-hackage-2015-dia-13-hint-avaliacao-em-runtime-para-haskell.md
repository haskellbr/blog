---
layout: post
title: "24 dias de Hackage 2015 - dia 13 - hint: Avaliação em runtime para Haskell"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original](http://conscientiousprogrammer.com/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o
[dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

## Dia 13
Um característica marcante de uma uma "linguagem dinâmica" como Lisp e
JavaScript é a habilidade de avaliar código em runtime, dentro de um processo
existente. Já que o runtime de carregar classes é uma funcionalidade
fundamental do Java, Java também seria uma "linguagem dinâmica"? Acho que os
termos "linguagem estática" e "linguagem dinâmica" não são muito úteis, e uma
comparação de linguagens, compiladores e ambientes de desenvolvimento deveria
se concentrar em funções especificas da experiência de usuário e onde as
barreiras estão na semântica. Uma coisa complicada é que muito do que é
interessante na verdade depende da implementação.

<!-- more -->

Por exemplo, o padrão do Haskell não diz nada sobre `eval` em runtime, então há
algum sentido em que Haskell considerado como uma "linguagem" estritamente
definida não tem suporte nenhum para isso. Mas se considerarmos a implementação
e ecossistema do GHC, que é dominante hoje apesar da existência de outras
implementações do Haskell, existe muitas ferramentas que são "dinâmicas", no
sentido de serem capazes de acessar APIs do GHC de uma ou muitas formas
diferentes.

Edward Yang recentemente escreveu um _blog post_ interessante
["The convergence of compilers, build systems and package managers" _[N.T. A convergência de compiladores, sistemas de compilação e gerenciadores de pacotes]_](http://blog.ezyang.com/2015/12/the-convergence-of-compilers-build-systems-and-package-managers/)
sobre um subset do problema genérico de o que pode ter acesso ao quê no
contexto das ferramentas. Ele não tocou em avaliação em runtime, que é um
tópico inteiro por si só.

Para hoje, decidi mencionar que você já pode fazer avaliação em runtime do
código Haskell no GHC usando o pacote
[`hint`](http://hackage.haskell.org/package/hint)
e oferecer a ideia de que talvez pudesse ser útil ter algo menos _ad-hoc_ do
que pacotes de terceiros como esse.

## Por que carregar e avaliar Haskell dinamicamente?
Para mim, sempre se resume a ficar com inveja do mundo do Lisp.

Há momentos em que quis poder fazer avaliação e carregamento dinâmico de
Haskell e desejei que estivesse trabalhando com Lisp. O maior exemplo é tentar
suportar "plugins" escritos pelo usuário que podem ser carregados de um arquivo
ou digitados em um REPL customizado. A forma mais limpa de fazer isso é
escrever uma linguagem de domínio específico limitada e um parser, _type
checker_ (se a DSL tiver tipos) e compilador/interpretador para ela. Mas por
que fazer tudo isso, se podemos só permitir que todo o poder do Haskell seja
usado?

Felizmente, encontrei bibliotecas como o `hint` que me permitiram fazer o que
queria. Vou mostrar um exemplo do tipo de coisas que tenho feito.

## A tarefa
Imagine um programa que faz _sorting_ (ordena coisas), e permite que o usuário,
durante runtime, ofereça uma função customizada para fazer isso ao invés da
padrão. Por exemplo, o usuário poderia ter especificado o path de um arquivo
Haskell `OurSorter.hs` como um argumento da linha de comando ou o programa
poderia ter uma janela de preferências permitindo que o usuário digitasse o
texto de uma função de ordenamento.

Para fazer as coisas ainda mais interessantes, vamos dizer que a função de
ordenação que vai ser especificada deve ser polimórfica, restrita apenas o
suficiente para requerir a operação de comparação:

{% highlight haskell %}
userDefinedSort :: Ord a => [a] -> [a]
{% endhighlight %}

Como nós carregamos esse tipo de função em runtime?

## Um wrapper de tipos necessário
A primeira coisa a tirar do caminha é que nós não podemos carregar uma função
com tipo `Ord a => [a] -> [a]` diretamente, por causa da falta de suporte no GHC para
[_impredicate types_](http://jozefg.bitbucket.org/posts/2014-12-23-impredicative.html).
Mesmo assim, há uma forma de contornar isso, que é amarrar o tipo dentro de um
`newtype`, junto com o uso da funcionalidade de tipos de alta ordem
(_higher-rank types_). Eu aprendi esse macete lendo esse
[artigo sobre _impredicative types_](http://jozefg.bitbucket.org/posts/2014-12-23-impredicative.html).

{% highlight haskell %}
{-# LANGUAGE RankNTypes #-}

module SortWrapper (Sort(..)) where

newtype Sort =
  Sort { getSort :: forall a. Ord a => [a] -> [a] }
{% endhighlight %}

Agora nós podemos tentar carregar valores do tipo `Sort` ao invés do tipo
`Ord a => [a] -> [a]`.

## Como carregar?
Vamos criar uma API chamada `loadSort` que nos permite carregar um `Sort`
pesquisando por módulo e nome. Aqui está um teste do HSpec que ilustra que
queremos conseguir carregar um `Sort` e o usar em tipos de listas
diferentes. Nós estamos usando `Language.Haskell.Interpreter` para fazer o
trabalho:

{% highlight haskell %}
module HintExampleSpec where

import SortWrapper (Sort(Sort))
import HintExample (loadSort)

import qualified Language.Haskell.Interpreter as I

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec =
  describe "hint" $ do
    it "dynamically loads a correct polymorphic sort function" $ do
      Right (Sort ourSort) <-
        I.runInterpreter (loadSort "OurSorter" "ourSort")
      ourSort "ebcad" `shouldBe` "abcde"
      ourSort [1 :: Int, 5, 4, 3, 7] `shouldBe` [1, 3, 4, 5, 7]
    it "dynamically loads a wrong (only head) sort function" $ do
      Right (Sort onlyHead) <-
        I.runInterpreter (loadSort "OurSorter" "onlyHead")
      onlyHead "ebcad" `shouldBe` "e"
      onlyHead [True, False] `shouldBe` [True]

{% endhighlight %}

## Um "plugin" de exemplo
Nós criamos um "plugin" de exemplo em um diretório cujo código **não** é
compilado junto com o programa principal. Imagine que o usuário tem um
diretório separado de plugins.

## O carregador

{% highlight haskell %}
module HintExample where

import SortWrapper (Sort)
import qualified Language.Haskell.Interpreter as I
import Language.Haskell.Interpreter (OptionVal((:=)))

-- | Dynamically load a 'Sort' implementation from a file.
-- src is needed to pick up our SortWrapper.
-- sort-plugins is a sample user plugins directory
loadSort :: I.MonadInterpreter m =>
            String  -- ^ module name
         -> String  -- ^ function name
         -> m Sort
loadSort moduleName functionName = do
  I.set [I.searchPath := ["src", "sort-plugins"]]
  I.loadModules [moduleName]
  I.setImports [moduleName, "SortWrapper"]
  I.interpret (moduleName ++ "." ++ functionName) (I.as :: Sort)
{% endhighlight %}

A parte mais interessante é o uso da função `interpret` que tem tipo

{% highlight haskell %}
interpret :: (MonadInterpreter m, Typeable a) => String -> a -> m a
{% endhighlight %}

recebendo uma string e uma "testemunha" para um tipo monomórfico para que o
`interpret` saiba qual dicionário em runtime usar para o `Typeable` (a forma
padrão moderna para uma biblioteca fazer isso seria ter usado uma
[`Proxy`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Proxy.html)).

Então aqui está: avaliação em runtime no Haskell do GHC. O que o `hint` nos dá
é razoavelmente primitivo, mas eu achei útil.

## Conclusão
Eu gostaria de ver mais suporte oficial para o dinamismo em ambientes de
linguagens como Haskell. Isso requere acesso às entranhas do compilador ou APIs
oficiais, mas acho que é o caminho a ser seguido. Separações de fase com
princípios são importantes, como é a integração entre elas. Eu gosto que o
`hint` existe para permitir que eu carregue código Haskell dinamicamente.

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

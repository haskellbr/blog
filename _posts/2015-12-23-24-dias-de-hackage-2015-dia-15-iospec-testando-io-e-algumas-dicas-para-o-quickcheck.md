---
layout: post
title: "24 dias de Hackage, 2015 - dia 15 - IOSpec: Testando IO e algumas dicas para o QuickCheck"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original.](http://conscientiousprogrammer.com/blog/2015/12/15/24-days-of-hackage-2015-day-15-iospec-testing-io-and-some-quickcheck-tricks/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

## Encontro HaskellBR São Paulo
[Vamos nos encontrar em São Paulo em 25 de Janeiro de 2016. Marque sua presença
e comente se não puder vir.](http://www.meetup.com/haskellbr-sp/events/227526368/)

## Dia 15

No
[dia 11](/2015/12/18/24-dias-de-hackage-2015-dia-11-monad-loops-refatorando-para-evitar-escrever-funcoes-recursivas.html)
mencionei tangencialmente me sentir envergonhado de que ao contrário dos meus
outros exemplos, porque estava usando `IO`, eu não comecei escrevendo testes de
cara. Eu não queria desviar do tópico em questão, que não tinha nada a ver com
`IO` mas com combinadores monádicos genéricos.

<!-- more -->

Agora estou de volta para preencher esse vazio, mostrando uma forma de "mockar"
o `IO`, a biblioteca
[`IOSpec`](https://hackage.haskell.org/package/IOSpec). Não é uma solução
perfeita e gostaria que houvesse uma forma padrão de testar `IO` no ecossistema
do Haskell, ao invés dos muitos métodos diferentes disponíveis por aí
(comumente inventados de novo). Mas pelo menos ela dá um gostinho de uma forma
de atacar o problema.

Ironicamente, eu encontrei e
[consertei um bug](https://github.com/FranklinChen/twenty-four-days2015-of-hackage/commit/3ed2b4963a226b0fa0a8193a0ef2a4086723f1a7)
no dia 11 no processo de escrever um teste para ele! As vezes, eu acho que sou
muito asarado, porque quando faço edições de último minúto no código sem testes
(nesse caso transformar uma expressão `print` em duas), eu inevitavelmente
introduzo bugs. Isso é precisamente porque eu gosto de escrever testes. Eu não
confio em mim mesmo sem testes.

## O código do dia 11 para testar
Abaixo está uma cópia da descrição introdutória da tarefa no dia 11, com a
primeira versão do código em Haskell. Nós vamos testar essa primeira versão.

Vamos escrever uma função para simular um login, no qual:

- Pedimos a senha do usuário
- Entramos em um loop onde
    - Uma tentativa é lida
    - Se não for correta, pedimos outra senha e começamos de novo
    - Se for correta saímos do loop
 - Imprimimos "Congratulations!"

{% highlight haskell %}
logIn :: IO ()
logIn = do
  putStrLn "% Enter password:"
  go
  putStrLn "$ Congratulations!"

  where
    -- Use recursion for loop
    go = do
      guess <- getLine
      if guess /= "secret"
        then do
          putStrLn "% Wrong password!"
          putStrLn "% Try again:"
          go
        else
          return ()
{% endhighlight %}

## Mudanças necessárias para usar o `IOSpec`
Copiei todo o módulo do dia 11 para um novo módulo `IOSpecExample.hs`, com
algumas modificações:

### Modando o nome do módulo

{% highlight haskell %}
-- | Copied with modification from MonadLoopsExample
module IOSpecExample where
{% endhighlight %}

### Escondendo e adicionando imports relacionados a IO

{% highlight haskell %}
import Prelude hiding (getLine, putStrLn)
import Test.IOSpec (IOSpec, Teletype, getLine, putStrLn)
{% endhighlight %}

### Mudando assinaturas de tipo
Nós mudamos `IO a` para `IOSpec Teletype a` em todos os lugares, para
configurar o uso do simulador "teletype" para ler e imprimir caracteres, já que
acontece que nosso `logIn` só usa essas operações. Se usássemos outras
operações, há outros simuladores disponíveis e o `IOSpec`
[usa um mecânismo de tipo de "tipos de dados a la carte" para encontrar e misturar soluções](https://hackage.haskell.org/package/IOSpec-0.3/docs/Test-IOSpec-Types.html).

## Como `IOSpec` funciona
O `IOSpec` funciona fazendo tudo contruir uma estrutura de dados que é então
rodada com um `Scheduler` para produzir um `Effect`. Nós usamos `evalIOSpec`:

{% highlight haskell %}
evalIOSpec :: Executable f => IOSpec f a -> Scheduler -> Effect a
{% endhighlight %}

Nós (os testadores) podemos então interpretar o `Effect` da forma que
quisermos. Nós vamos só usar um "scheduler" básico _single-threaded_ para esse
exemplo.

## O teste
Para o nosso teste, nós vamos usar o QuickCheck para gerar input randômico do
usuário. Como estamos simulando um teletype, nós assumimos que o usuário entra
com uma stream (que representamos como uma lista) de strings que não contém
quebras de linha. Nós queremos verificar que uma delas é a palavra `"secret"`,
então o output total deve ser só uma prompt introdutória, o número de prompts
mostradas se o input estiver incorreto e o "parabéns" no final.

Aqui está o spec, com código auxiliar discutido abaixo:

{% highlight haskell %}
{-# LANGUAGE ScopedTypeVariables #-}

module IOSpecExampleSpec where

import IOSpecExample (logIn)
import qualified Test.IOSpec as IO

import Test.Hspec (Spec, hspec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.Coerce (coerce)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "IOSpec" $ do
    prop "logIn outputs prompts until secret is guessed" $
      \(notSecretLines :: [NotSecretString]) (anyLines :: [NotNewlineString]) ->
      let allLines = coerce notSecretLines
                     ++ ["secret"]
                     ++ coerce anyLines
          outputLines = ["% Enter password:"]
                        ++ concatMap
                           (const [ "% Wrong password!"
                                  , "% Try again:"])
                           notSecretLines
                        ++ ["$ Congratulations!"]
      in takeOutput (withInput (unlines allLines)
                               (IO.evalIOSpec logIn IO.singleThreaded))
         == unlines outputLines
{% endhighlight %}

## `newtype`s para controlar o QuickCheck
Nós estamos usando um macete padrão do QuickCheck para gerar geradores
randômicos de um tipo existênte para um domínio específico.

Um dos nossos tipos é `NotSecretString`, representando uma linha digitada pelo
o usuário que não é igual a `"secret"`; ela também não deve conter uma quebra
de linha. A instância de `Arbitrary` do QuickCheck é só boilerplate para manter
as invariantes desejadas.

{% highlight haskell %}
-- | User input without a newline, and not equal to "secret".
newtype NotSecretString =
  NotSecretString { getNotSecretString :: NotNewlineString }
  deriving (Show)

instance Arbitrary NotSecretString where
  arbitrary = NotSecretString <$>
              arbitrary `suchThat` ((/= "secret") . coerce)
  shrink = map NotSecretString
              . filter ((/= "secret") . coerce)
              . shrink
              . getNotSecretString
{% endhighlight %}

Nós refinamos ainda mais até o nível do `Char`, para gerar `NotNewlineChar`:

{% highlight haskell %}
type NotNewlineString = [NotNewlineChar]

newtype NotNewlineChar =
  NotNewlineChar { getNotNewlineChar :: Char }
  deriving (Show)

-- | Quick hack. Ideally should write specific generator rather than
-- filtering off the default 'Char' generator.
instance Arbitrary NotNewlineChar where
  arbitrary = NotNewlineChar <$>
              arbitrary `suchThat` (/= '\n')
  shrink = map NotNewlineChar
              . filter (/= '\n')
              . shrink
              . getNotNewlineChar
{% endhighlight %}

### Uma nota sobre strings randômicas
Nós usamos um hack rápido para conseguir uma string "arbitrária". Não é tão
arbitrária, como você pode ver a partir do
[código-fonte do QuickCheck](https://github.com/nick8325/quickcheck/blob/master/Test/QuickCheck/Arbitrary.hs#L473).
Para gerar strings Unicode de qualidade, use o
[`quickcheck-unicode`](http://hackage.haskell.org/package/quickcheck-unicode).

## Uma nota sobre `coerce`: é só um hack de otimização
Se ainda não viu `coerce` de
[`Data.Coerce`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Coerce.html),
você pode estar se perguntando do que se trata. É só um hack de eficiência para
tomar vantagem da garantia do Haskell que uma representação `newtype` é
idêntica ao tipo que contém. `coerce` resolve o problema de garantir que algo
que contém algo que contém (algo que contém...) alguma coisa ainda é
reconhecido como tendo a mesma representação. Para que não haja nenhum custo
durante runtime de tratar o valor como sendo de um tipo diferente. É um hack
feio mas conveniente. Se algo não pode ser forçado para essa forma, temos um
erro de tipagem, então isso é *seguro*.

Se você não gosta de usar `coerce`, terá que usar muito `map` e _unwrapping_ e
_rewrapping_ para converter entre todos os tipos diferentes, como:

{% highlight haskell %}
notSecretStringsAsStrings :: [NotSecretString] -> [String]
notSecretStringsAsStrings = map notSecretStringAsString

notSecretStringAsString :: NotSecretString -> String
notSecretStringAsString = map getNotNewlineChar . getNotSecretString
{% endhighlight %}

Seria mais fácil ainda que menos embasado para mim só pensar "eles são todos só
`Char` e `[Char]` por baixo dos panos". Se alguém tem guias sobre quando usar o
`coerce` e quando não, ficaria feliz em as linkar aqui (e mudar meu código como
apropriado).

## Interpretando um `Effect`
Nós interpretamos um `Effect` da mesma forma de um
[exemplo vindo da biblioteca](https://hackage.haskell.org/package/IOSpec-0.3/src/examples/Echo.hs).
Nós convertemos `Print` e `ReadChar` da forma que você esperaria se estivesse
gerando uma stream de caracteres ou lendo uma.

Receber output é direto, interpretando `Done` e `Print`:

{% highlight haskell %}
takeOutput :: IO.Effect () -> String
takeOutput (IO.Done _) = ""
takeOutput (IO.Print c xs) = c : takeOutput xs
takeOutput _ = error "takeOutput: expects only Done, Print"
{% endhighlight %}

Receber input é mais difícil, porque o que está acontecendo de fato é que uma
stream de caracteres de input é usada para converter um `Effect` em outro. O
construtor `ReadChar` tem tipo `Char -> Effect a` e então quando chamado em um
`Char`, retorna "a próxima coisa que acontece".

{% highlight haskell %}
withInput :: [Char] -> IO.Effect a -> IO.Effect a
withInput _ (IO.Done x) = IO.Done x
withInput stdin (IO.Print c e) = IO.Print c (withInput stdin e)
withInput (char:stdin) (IO.ReadChar f) = withInput stdin (f char)
withInput _ _ = error "withInput: expects only Done, Print, ReadChar"
{% endhighlight %}

Com esse teste, encontrei um bug no meu código, no qual o output não era o que
eu esperava, por causa de um sinal de porcentagem que desapareceu depois de uma
edição.

## Uma nota final: há um padrão popular chamado "monad livre"
Se você já sabe sobre monads livres, você provavelmente queria me dizer "e
quanto ao monad livre?" desde o começo do artigo, mas eu não queria entrar nisso
ainda. Talvez depois.

## Conclusão
Hoje eu apresentei uma forma de adicionar uma camada de abstração a operações
`IO` de forma que possamos interpretar elas de outra forma além de apenas as
realizar; para escrever e rodar testes.

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

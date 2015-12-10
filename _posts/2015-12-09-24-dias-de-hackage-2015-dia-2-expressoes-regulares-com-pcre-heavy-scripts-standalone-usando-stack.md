---
layout: post
title: "24 dias de Hackage, 2015 - dia 2 - Expressões regulares com pcre-heavy; scripts standalone usando Stack"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original](http://conscientiousprogrammer.com/blog/2015/12/02/24-days-of-hackage-2015-day-2-regexes-with-pcre-heavy-standalone-haskell-scripts-using-stack/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introdução-e-stack.html).

- - -

# Dia 2
Não ria, mas muito tempo atrás, minha linguagem de programação principal era
Perl (entre meados de 1999 e 2010). Havia uma série de razões para isso; uma
delas era que Perl fazia processamento de texto usando expressões regulares ser
muito fácil.

Se você é um Haskeller experiente, você pode estar pensando "Por que não usar um
parser de verdade?", algo usando o venerável
[parsec](https://hackage.haskell.org/package/parsec), coberto em um
[dia de Hackage de 2012](https://ocharles.org.uk/blog/posts/2012-12-10-24-days-of-hackage-parsec.html).

(Hoje, várias outras bibliotecas alternativas para parsing poderiam ser
consideradas. Outro post dirá mais sobre isso!)

Afinal, como Jamie Zawinski escreveu:

> Algumas pessoas, quando confrontadas com um problema, pensam: "Já sei, vou
> usar expressões regulares". Agora elas tem dois problemas.

Eu até dei uma palestra no [Pittsburgh Tech Fest](http://pghtechfest.com/) em
2013, ["Stop overusing regular
expressions!"](http://www.slideshare.net/FranklinChen/handout-22302440) _["Pare
de abusar de expressões regulares!"]_, na qual eu promovi escrevermos parsers ao
invés de regexes.

Ainda assim, às vezes eu quero usar uma expressão regular. Nesse caso, eu tenho
usado um pacote obscuro porém útil: [`pcre-heavy`](https://hackage.haskell.org/package/pcre-heavy).

Hoje vou mostrar como usar o `pcre-heavy`, ao mesmo tempo que mostro como
publicar *scripts Haskell standalone de um arquivo* que só requerem o Stack.

- - -

## Por que usar regexes?

Antes de começar com o `pcre-heavy`, eu acho que deveria explicar quando uso
expressões regulares.

Na época em que trabalhava com muita extração de texto, limpeza (incluindo
*correção*) e reestruturação de dados bagunçados, expressões regulares pareciam
a única escolha real. Eu tinha que não perder nenhuma informação mesmo se ela
estivesse escondida atrás de barulho, erros gramaticais ou coisas do tipo. Eu
não podia usar alguma técnica estatística aproximada; tinha que iterativamente
fazer um monte de trabalho exploratório com alguma prompt interativa para
gradualmente limpar os dados. _Constructs_ super-poderosos de regexes em Perl
pareciam perfeitos para isso.

Mesmo fora desse tipo de uso, não há como negar que regexes podem ser muito
convenientes para tarefas simples _[N.T.: até pouco tempo o compilador de
CoffeeScript era escrito com RegExps...]_. Além disso, porque regexes são tão
usadas no nosso mundo da programação em geral, se estivermos migrando algumas
regexes prontas de código escrito em outras linguagens para Haskell, é
conveniente não ter que as reescrever.

## Qual biblioteca de regexes usar com Haskell?!

Um recem-chagado a Haskell pode ficar frustrado com a falta de uma única
biblioteca padrão e sintaxe para regexes. Quero dizer... Veja essa [página da
wiki](https://wiki.haskell.org/Regular_expressions).

Hoje, estou apresentando
[`pcre-heavy`](https://hackage.haskell.org/package/pcre-heavy), uma biblioteca
de expressões regulares que eu tenho usado quando eu quero expressões regulares
(tento não as querer). Ela é bem nova e nem está mencionada nessa página da
wiki.

Alguns dos meus critérios para escolher uma biblioteca de regex:

- Eu quero regexes estilo-Perl. Isso é o que _eu_ estou acostumado com e elas
  são meio que um padrão no suporte a regexes em muitas linguagens.
- Boa sintaxe é um _plus_. Uma das vantagens de usar regexes é o quão conciso é
  escrever patterns, extrair matches etc. Sem isso, eu já pensaria "Por que não
  só escrever um parser de verdade? De qualquer forma, só leva algumas linhas em
  Haskell."
- Alta-performance

Dados esses critérios, usar uma biblioteca [PCRE](http://www.pcre.org/) parece
ser a escolha certa. Ok, a wiki lista um monte de bibliotecas baseadas em PCRE.

_[N.T.: PCRE significa "Perl Compatible Regular Expressions" ou "expressões
regulares 'estilo' perl"]_

[`pcre-light`](https://hackage.haskell.org/package/pcre-light) é um bom começo.

**Esse pacote depende da instalação da biblioteca em C para PCRE.**

Eu sou principalmente um usuário de Mac OS X, então eu tenho o suporte a PCRE
instalado com `$ brew install pcre`. Também tenho o pacote funcionando no Linux.
Infelizmente, eu não uso Windows, então seria ótimo se alguém pudesse verificar se o
`pcre-light` instala no Windows. Eu ia ficar um pouco triste se eu tiver
escolhido uma biblioteca problemática para usuários Windows. _[N.T.: eu não]_

Recentemente, o [`pcre-heavy`](https://hackage.haskell.org/package/pcre-heavy)
foi publicado, um wrapper em volta do `pcre-light` que usa [Template Haskell](https://wiki.haskell.org/Template_Haskell),
a extensão do GHC que se resume a "macros para Haskell", permitindo
meta-programação em tempo de compilação (veja o [Dia de Hackage de 2014 sobre Template Haskell](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html)).

_[N.T.: Template Haskell tem a vantagem sobre um "sistema" de macros porque é
seguro; se o código estiver errado, não compila. Além disso, é relativamente
simples gerar expressões complicadas graças ao suporte a: 1. Gerar a estrutura
de dados que representa um pedaço de código automaticamente baseada nele (isso é
útil porque se opera com a AST), 2. ADTs (Abstract Data Types) para cada
estrutura de código (uma função, um `where`, um `let` etc.). Vale lembrar que
não é a única forma de metaprogramação em Haskell, podemos listar todos os
campos de um tipo por exemplo e os acessar em runtime usando outras técnicas.]_

Eu gostei dele, então o uso.

## Um programa de exemplo usando o `pcre-heavy`

`pcre-heavy` tem uma documentação decente na [sua página do
Hackage](https://hackage.haskell.org/package/pcre-heavy), então eu recomendo ler
ela para todos os detalhes em como o usar. Eu só vou dar um exemplo simples no
contexto de um programa que faz algo.

### Especificação e alguns testes

Digamos que temos um arquivo de texto com um formato separado por vírgulas
contendo:

- Um header fixo
- Um campo "áudio" ou "vídeo" indicando o tipo da mídia associada
- O path de uma transcrição
- Um comentário opcional dizendo se a mídia não existe ou ainda não está linkada
  na transcrição

(Eu inventei esse exemplo baseado na especificação de texto estruturado chamada
CHAT que inclui uma única linha com esse formato, e.g.
[essa transcrição do Supremo para "Citizens United v. Federal Election Commission"](http://talkbank.org/data-orig/Meeting/SCOTUS/2008/08-205.cha).)


Exemplos que devem dar _match_:
{% highlight console %}
@Media:	has-audio,   audio
@Media:	has-video,video
@Media:	has-audio-but-missing, audio, missing
@Media:	has-video-but-unlinked  , video,      unlinked
{% endhighlight %}

Exemplos que não devem dar _match_:
{% highlight console %}
@Media:	no-audio-or-video
@Media:	missing-media-field, unlinked
{% endhighlight %}

### Escrevendo a expressão regular

Aqui está uma expressão regular `pcre-heavy`, usando o
[quasiquoter](https://wiki.haskell.org/Template_Haskell#QuasiQuoters)
de Template Haskell
[`re`](https://hackage.haskell.org/package/pcre-heavy-1.0.0.1/docs/Text-Regex-PCRE-Heavy.html#v:re)
que constrói uma
[`Regex`](https://hackage.haskell.org/package/pcre-heavy-1.0.0.1/docs/Text-Regex-PCRE-Heavy.html#t:Regex):
PCRE compilada.

{% highlight haskell %}
mediaRegex :: Regex
mediaRegex = [re|^@Media:\t([^ ,]+)\ *,\ *(audio|video)(\ *,\ *(?:missing|unlinked))?|]
{% endhighlight %}

## Expressão Regular validada no tempo de compilação do Haskell
Uma vantagem do `pcre-heavy` para mim é que porque ele usa
Template Haskell, uma regex errada resulta em um erro de compilação, não um erro
de runtime.

Um exemplo desses erros:

{% highlight haskell %}
-- Esse código não compila!
mediaRegex :: Regex
mediaRegex = [re|^@Media:\t([^ ,]+)\ *,\ *(audio|video)(\ *,\ *(?:missing|unlinked)?|]
{% endhighlight %}

Carregar isso no GHCi imprime:

{% highlight console %}
    Exception when trying to run compile-time code:
      Text.Regex.PCRE.Light: Error in regex: missing )
    Code: template-haskell-2.10.0.0:Language.Haskell.TH.Quote.quoteExp
            re
            "^@Media:\\t([^ ,]+)\\ *,\\ *(audio|video)(\\ *,\\ *(?:missing|unlinked)?"
{% endhighlight %}

## Usando a expressão regular

Vamos usar
[`scan`](https://hackage.haskell.org/package/pcre-heavy-1.0.0.1/docs/Text-Regex-PCRE-Heavy.html#v:scan)
para extrair os _matches_ (se existirem) da nossa regex contra uma string.

`scan` retorna uma lista preguiçosa de todos os _matches_ possíveis:
{% highlight haskell %}
-- Assinatura simplificada para nossos propósitos
scan :: Regex -> String -> [(String, [String])]
{% endhighlight %}

Cada _match_ é um par `(String, [String])`, onde o primeiro membro é toda a
string que deu _match_ e o segundo é uma lista de todos os grupos na expressão
regular. Na nossa regex, tínhamos três grupos, então um _match_ só pode resultar
em uma lista com três elementos:

{% highlight haskell %}
*Main> scan mediaRegex "@Media:\tfoo, audio, unlinked"
[("@Media:\tfoo, audio, unlinked",["foo","audio",", unlinked"])]
{% endhighlight %}

Como só queremos o primeiro _match_ (se existir), nós podemos compor nossa
função com
[`listToMaybe` de `Data.Maybe`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Maybe.html#v:listToMaybe),
que tem tipo:

{% highlight haskell %}
listToMaybe :: [a] -> Maybe a
{% endhighlight %}

então `listToMaybe . scan mediaRegex` tem tipo `String -> Maybe (String, [String])`.

_[N.T.: `listToMaybe` é um `head` type-safe, retorna `Just primeiroElemento` ou
`Nothing`.]_

{% highlight haskell %}
*Main> (listToMaybe . scan mediaRegex) "@Media:\tfoo, audio, unlinked"
Just ("@Media:\tfoo, audio, unlinked",["foo","audio",", unlinked"])
{% endhighlight %}

## Extraindo os dados úteis

Finalmente, o que nós queríamos fazer de verdade depois de dar _match_ era
aplicar um pouco de lógica e botar as coisas em um tipo assim que possível, ao
invés de entrar no ramo de programação orientada a strings e a listas cujo
tamanho depende do contexto.

_[N.T.: Ele quer dizer que nós não queremos operar com uma estrutura tipo
`resultado !! 0 -- é a mídia`, `resultado !! 1 -- é o tipo` etc. Coisas que não
queremos fazer mesmo em linguagens sem tipos ;)]_

Digamos que para a nossa tarefa, só as linhas que não estão _missing_ ou
_unlinked_ importam. Podemos definir um tipo de dados e usar _pattern matching_
para sair do mundo dinâmico e entrar no mundo tipado do nosso modelo de dados.

{% highlight haskell %}
data Info =
    Skip
  | Audio FilePath
  | Video FilePath
    deriving (Eq, Show)

-- | Extrai informação sobre um arquivo de mídia se ele existir
extractIfPresent :: (String, [String]) -> Info
extractIfPresent (_, [name, "audio"]) = Audio name
extractIfPresent (_, [name, "video"]) = Video name
extractIfPresent (_, _) = Skip
{% endhighlight %}

## Apresentação como um relatório

Agora que acabamos com o mundo das expressões regulares e temos um modelo de
dados estruturado, tudo que falta é completar um programa CLI simples.

Temos toda a informação necessária para imprimir um relatório para cada linha.

{% highlight haskell %}
-- | Impríme um relatório
reportOnInfo :: Maybe Info -> IO ()
reportOnInfo Nothing = putStrLn "não deu match"
reportOnInfo (Just Skip) = putStrLn "deu match, mas é 'missing' ou 'unlinked'"
reportOnInfo (Just (Audio path)) = printf "audio at %s\n" path
reportOnInfo (Just (Video path)) = printf "video at %s\n" path
{% endhighlight %}

Para finalizar, jogamos todo o `stdin` na nossa lógica:
{% highlight haskell %}
main :: IO ()
main = do
  s <- getContents
  mapM_ (reportOnInfo
        . fmap extractIfPresent
        . listToMaybe
        . scan mediaRegex
       ) (lines s)
{% endhighlight %}

- - -

### N.T. Extendida: Desconstruindo esse exemplo

Todo o programa de Haskell precisa declarar um `main`, como no `C`. Essa é a
primeira linha:
{% highlight haskell %}
main :: IO ()
main = undefined
{% endhighlight %}

O `main` tem tipo `IO ()` - uma ação que roda no contexto de Haskell que pode
fazer _input_ e _output_, e que retorna o tipo vazio `()` (a.k.a. `void`).

Seguimos para pegar _todo_ o `stdin` como uma lista preguiçosa usando `s <-
getContents`. O tipo de `getContents` é:

{% highlight haskell %}
getContents :: IO String
{% endhighlight %}

Se você conhece Node.js, isso é algo equivalente à `Stream` `process.stdin`,
exceto que você pode a tratar como uma `String` normal e ela vai de pouco em
pouco sendo populada. Armazenamos na variável `s`, mas nesse ponto o `stdin` não
foi consumido. Há uma nota do autor original sobre isso no final do post.

{% highlight haskell %}
s <- getContents
{% endhighlight %}

Em seguida chamamos:
{% highlight haskell %}
mapM_ todaNossaLogica (lines s)
{% endhighlight %}

O que essa expressão faz é chamar `lines s` para gerar outra lista preguiçosa de
cada uma das linhas do `stdin` e chamar `mapM_` com toda a nossa lógica sobre as
linhas.

`mapM_` é um helper que executa uma função sobre uma collection no contexto de
um `Monad` e omite o resultado. Podemos simplificar e dizer que é:
{% highlight haskell %}
mapM_ fn lista = case lista of
    (primeiroElemento:restoDaLista) -> do
        fn x
        mapM_ fn restoDaLista
    [] -> return ()
{% endhighlight %}

Inclusive, se você escrever isso o compilador vai inferir quase o tipo certo pra
você, exceto um detalhe: algo que foi introduzido no GHC 7.10 esse ano, que
deixa a função ser mais genérica e funcionar em outras estruturas além de
listas; isso tem a ver com a proposta FTP (Foldable Traversable in Prelude),
sobre a qual você pode ler [aqui](https://wiki.haskell.org/Foldable_Traversable_In_Prelude).

_(avise se você gostaria de ler sobre isso)_

Resta `todaNossaLogica` que é:
{% highlight haskell %}
todaNossaLogica = reportOnInfo
                . fmap extractIfPresent
                . listToMaybe
                . scan mediaRegex
{% endhighlight %}

Isso quer dizer para cada `linha` em `lines s`:

* Chama `scan mediaRegex linha` (retorna a lista de matches `[(string, grupo)]`)
* Passa isso pro `listToMaybe` (retorna um `Maybe` para o primeiro elemento)
* Passa isso para `fmap extractIfPresent` (veja o parágrafo abaixo)
* Passa o resultado final (um tipo estruturado - o `Info`) para `reportOnInfo`

`fmap` é uma função da type-class `Functor` no Haskell. Muito resumidamente, um
`Functor` é: uma estrutura de dados que contem um valor no qual podemos aplicar
uma função. Para isso temos o `fmap`. Ele:

* "entra" dentro da estrutura
* aplica a função
* retorna outra estrutura de dados contendo o valor transformado

Então para um `Maybe a`, `fmap` recebe uma função de `a -> a`, um `Maybe a` e
retorna outro `Maybe a`. Em outras palavras:

{% highlight haskell %}
fmap :: Functor f => (a -> a) -> f a -> f a
fmapDoMaybe :: (a -> a) -> Maybe a -> Maybe a
fmapDoMaybe fn (Just x) = Just (fn x)
fmapDoMaybe _ Nothing = Nothing
{% endhighlight %}

- - -

## Usando Stack para publicar scripts standalone

Nós podemos testar nosso programa no REPL GHCi digitando `main` ou `:main` no
prompt do REPL e digitando linhas de entrada. Nós também podemos rodar
`stack build` para compilar um binário nativo.

Uma outra opção é publicar o código-fonte como um script _standalone_ de um
arquivo. Isso pode ser muito conveniente em algumas circunstancias; você pode só
confiar que o usuário tenha o Stack instalado.

Aqui está como podemos converter nosso programa em um script _standalone_: só
adicione essas duas linhas e faça o arquivo ser executável (`chmod +x arquivo`):

{% highlight haskell %}
#!/usr/bin/env stack
-- stack --resolver lts-3.17 --install-ghc runghc --package pcre-heavy
{% endhighlight %}

O Stack vai ler o comando no comentário para:
- Instalar o GHC se necessário
- Instalar os pacotes listados
- Interpretar o código

Nós especificamos uma distribuição LTS dos pacotes do Stackage para garantir que
tudo quais versões de tudo serão usadas.
(Nota: nesse caso, por causa da FFI com uma biblioteca escrita em C, ela deve
ser instalada antes de rodar o script)

_[N.T. Algo parecido é feito no meu projeto
[`stack-run-auto`](https://github.com/yamadapc/stack-run-auto) com a adição de
não termos de especificar os pacotes ou a distribuição - eles são detectados
automáticamente]_

Se você tem programas curtos que não precisam ser organizados em pacotes do
Cabal completos, você pode tratar Haskell como uma "linguagem de scripting" e
ainda ter acesso a todas as bibliotecas no Hackage!

{% highlight console %}
$ app/PCREHeavyExampleMain.hs < input.txt > output.txt
{% endhighlight %}

### Um aviso

Apesar dessa função do Stack como um interpretador de Haskell ser muito legal,
eu prefiro escrever código modular, em bibliotecas separadas e testáveis,
deixando o `main` ser só a lógica que amarra as pontas de várias bibliotecas em
um módulo `Main` do programa. Além disso, eu prefiro usar bibliotecas e
executáveis compilados porque eles tem um _startup_ muito mais rápido. `runghc`
é um interpretador de Haskell, não um compilador nativo com otimizações. Claro
que a beleza do mundo do GHC é que você pode usar um ou o outro, e pular de
intepretado para compilado sem problemas.

### O programa completo

{% highlight haskell %}
#!/usr/bin/env stack
-- stack --resolver lts-3.17 --install-ghc runghc --package pcre-heavy

{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.Regex.PCRE.Heavy (Regex, re, scan)
import Data.Maybe (listToMaybe)
import Text.Printf (printf)

-- | Dê match em um nome de mídia, audio/video, e opcionalmente missing/unlinked.
mediaRegex :: Regex
mediaRegex = [re|^@Media:\t([^ ,]+)\ *,\ *(audio|video)(\ *,\ *(?:missing|unlinked))?|]

data Info =
    Skip
  | Audio FilePath
  | Video FilePath
    deriving (Eq, Show)

-- | Extraí a informação do match se ela estiver lá
extractIfPresent :: (String, [String]) -> Info
extractIfPresent (_, [name, "audio"]) = Audio name
extractIfPresent (_, [name, "video"]) = Video name
extractIfPresent (_, _) = Skip

-- | Imprime um relatório
reportOnInfo :: Maybe Info -> IO ()
reportOnInfo Nothing = putStrLn "no match"
reportOnInfo (Just Skip) = putStrLn "match, but missing or unlinked"
reportOnInfo (Just (Audio path)) = printf "audio at %s\n" path
reportOnInfo (Just (Video path)) = printf "video at %s\n" path

-- | Amarra as pontas
main :: IO ()
main = do
  s <- getContents
  mapM_ (reportOnInfo
        . fmap extractIfPresent
        . listToMaybe
        . scan mediaRegex
       ) (lines s)
{% endhighlight %}

## Algumas notas adicionais
Uma limitação de um artigo expositório com código de exemplo é que nós não
queremos desperdiçar espaço e atenção, e por isso tendemos a apresentar código
rápido-e-sujo, ao invés de código com nível de produção (que é rápido, se
recupera no caso de erros, tem boa documentação etc.). Eu tenho pensado no
dilema de *como não dar uma má impressão e ser um mau exemplo ao mostrar código
simplista*. Não há uma resposta fácil , mas eu senti que poderia ser útil prover
notas opcionais "avançadas" as vezes, sobre como escrever Haskell no mundo real.

`pcre-heavy` permite executar regexes contra `String`s, `ByteString`s e `Text`s.
Na prática, para eficiencia, nós queremos usar
[`bytestring`](http://hackage.haskell.org/package/bytestring) e
[`text`](http://hackage.haskell.org/package/text) o máximo possível,
no lugar do tipo `String`, ineficiente. [Um artigo Dia de Hackage de 2012 fala sobre o pacote `text`](https://ocharles.org.uk/blog/posts/2012-12-12-24-days-of-hackage-text.html).
Já que a biblioteca em PCRE escrita em C usada nos bastidores usa bytes, eu
geralmente uso bytestrings com o `pcre-heavy`.

O código do `main` no exemplo usa I/O preguiçoso para ler do input. Isso é
superficialmente muito elegante e conciso para propósitos pedagógicos, mas na
vida real isso é uma fonte de vazamentos de memória e outros problemas.
Inclusive faz as pessoas pensarem que "Haskell é ineficiente". Para trabalho
real, eu gosto de usar o pacote [`pipes`](http://hackage.haskell.org/package/pipes),
que foi coberto em outro [dia de Hackage de 2012](https://ocharles.org.uk/blog/posts/2012-12-16-24-days-of-hackage-pipes.html)
e também tem um
[tutorial extensivo e bonito](https://hackage.haskell.org/package/pipes-4.1.7/docs/Pipes-Tutorial.html)
por seu autor, Gabriel Gonzalez, que também tem um blog fantástico, ativo e de longa
data
["Haskell for all"](http://www.haskellforall.com/) que todos os Haskellers
deveriam seguir.

Finalmente, a expressão regular foi a escolha certa aqui? Foi simples o bastante
para esse problema, mas você pode ver pelo _pattern matching_ ad-hoc,
strings _hardcoded_, o número de grupos e a frágil ordem posicional que as
coisas ficariam suscetíveis a erros muito rápido se a expressão regular ficasse
um pouco mais complexa e nós quisessemos tratar erros quando o _match_ falhasse.

## Conclusão

Suporte a regexes não é o ponto mais forte do ecossistema Haskell, que vai mais
para o lado de parsing estruturado, mas há opções se você quer mesmo usar
regexes, e eu gosto da família `pcre-light` de bibliotecas de regex estilo-Perl
que agora incluí `pcre-heavy`.

Eu também mostrei como adicionar duas linhas no topo de um programa Haskell para
o transformar em um script do Stack.

## Todo o código
Todo o código para a série estará [nesse repositório do GitHub](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).

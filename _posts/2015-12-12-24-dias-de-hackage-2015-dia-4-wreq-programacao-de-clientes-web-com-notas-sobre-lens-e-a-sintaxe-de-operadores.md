---
layout: post
title: "24 dias de Hackage, 2015 - dia 4 - wreq: Programação de clientes Web; com notas sobre lens e a sintaxe de operadores"
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

# Dia 4

No final dos anos 90, eu entusiasmadamente comprei o livro
["Web Client Programming with Perl" _[N.T. "Programação de Clientes Web com Perl"_](http://www.oreilly.com/openbook/webclient/)
e usei a biblioteca [LWP](http://search.cpan.org/dist/libwww-perl/lib/LWP.pm)
para fazer _Web scraping_ automatizado. Eu continuei fazendo isso anos 2000
adentro. Estou feliz de que hoje, eu posso só usar Haskell para fazer esse tipo
de programação e o fazer de forma sucinta.

<!-- more -->

O tópico de hoje é o [`wreq`](http://www.serpentine.com/wreq/), a biblioteca de
alto nível de [Bryan O'Sullivan](http://www.serpentine.com/blog/) para fazer
programação de clientes Web e desenhada com usabilidade em mente.

_[N.T. Pense `requests` para Python ou `superagent` para Node.js]_

`wreq` usa o ecossistema do [`aeson`](https://hackage.haskell.org/package/aeson)
para JSON e [`lens`](https://hackage.haskell.org/package/lens) e seu
ecossistema, incluindo
[`lens-aeson`](https://hackage.haskell.org/package/lens-aeson), então você pode
querer conferir os posts dos 24 dias de Hackage de 2012 pelo Ollie sobre o
[aeson](https://ocharles.org.uk/blog/posts/2012-12-07-24-days-of-hackage-aeson.html)
e [lens](https://ocharles.org.uk/blog/posts/2012-12-09-24-days-of-hackage-lens.html).

Já que o `wreq` já tem um
[tutorial e documentação](http://www.serpentine.com/wreq/) extensivos, eu não
vou repetir suas explicações. Ao invés disso, vou dar um exemplo de uso que deve
ser simples o suficiente para ser entendido dentro do contexto e discutir os
problemas de usar a sintaxe de operadores em Haskell.

- - -

## A tarefa

Sou um membro de muitos grupos no [Meetup](http://www.meetup.com/). É
frequentemente útil para mim de conseguir informação usando a [API oficial do
Meetup](http://www.meetup.com/meetup_api/) ao invés de ficar clicando em um
site ou um app. Por que na mão o que posso fazer corretamente e eficientemente
com código?

Aqui está um exemplo muito simplificado de algo que eu posso querer fazer com o
Meetup. Eu sou ativo na comunidade [Pittsburgh Code and Supply](http://www.codeandsupply.co/),
que tem uma [página no Meetup](http://www.meetup.com/Pittsburgh-Code-Supply/)
com uma agenda cheia de eventos (está em hiato agora em Dezembro para as festas,
mas em geral é muito ativa). Talvez eu queira descobrir quais são os próximos
eventos e buscar por eventos que são interessantes de acordo com algum critério.
Para nosso exemplo, vamos dizer que eu quero encontrar os dez próximos eventos,
seus nomes e locais, e ter certeza de que pelo menos um evento que tem um nome e
local já configurados (as vezes um evento já foi proposto, mas ainda não tem um
local).

## Um teste

Ontem,
[dia 3](/2015/12/10/24-dias-de-hackage-2015-dia-3-hspec-a-importancia-de-testes.html)
dessa série de artigos, mencionei gostar de usar o HSpec, então vamos usar o
HSpec.

{% highlight haskell %}
{-# LANGUAGE OverloadedStrings #-}

import WreqExample (GroupId, eventName, venueName, getMeetupEventInfos)
import Test.Hspec ( Spec, hspec, describe, it
                  , shouldSatisfy, shouldNotSatisfy
                  )
import qualified Data.Text as Text
{% endhighlight %}

Estamos usando o tipo "empacotado" Unicode de strings
[`text`](https://hackage.haskell.org/package/text)
porque é isso que o `wreq` usa. `OverloadedStrings` é uma extensão do GHC
conveniente que permite literais de string no código serem tratados como `Text`
ao invés de `String`. Ollie discutiu essa extensão no seu [Dias de Extensões do
GHC 2014](https://ocharles.org.uk/blog/posts/2014-12-17-overloaded-strings.html).

_[(Nota em português sobre Text e OverloadedStrings)](http://localhost:4000/2015/12/12/sobre-text-e-overloadedstrings.html)_

Also, since I'm operating in test-driven development style, I wrote
this test first, before writing the `WreqExample` module: I only wrote
the imports for what I need for the test.

{% highlight haskell %}
spec :: Spec
spec =
  describe "wreq" $ do
    it "there are named, located Pittsburgh Code and Supply events coming up" $ do
      -- Aviso! Esse teste vai fazer uma requisição HTTP
      events <- getMeetupEventInfos pittsburghCodeAndSupplyId
      events `shouldNotSatisfy` null
      events `shouldSatisfy` any
        (\event -> (not . Text.null . eventName) event
                   && (not . Text.null . venueName) event)

pittsburghCodeAndSupplyId :: GroupId
pittsburghCodeAndSupplyId = "13452572"
{% endhighlight %}

## Assinaturas para Módulos

Se Haskell tivesse
[assinaturas para módulos, como Standard ML e OCaml](http://jozefg.bitbucket.org/posts/2015-01-08-modules.html),
eu escreveria uma assinatura explícita para o módulo que eu pretendo implementar
que vai conformar com ela, mas Haskell não as tem, então o melhor que podemos
fazer é operar à moda do _"duck typing"_ no nível dos módulos, confiando
implicitamente que compilação falhe em um import de uma implementação ao invés
de usar uma assinatura explícita sem implementação.

Aqui estão os tipos que precisamos (em pseudo-código fingindo que Haskell
tivesse assinaturas para módulos):

{% highlight haskell %}
type GroupId    -- abstract

type EventInfo  -- abstract

-- getters
eventName :: EventInfo -> Text
venueName :: EventInfo -> Text

getMeetupEventInfos :: GroupId -> IO [EventInfo]
{% endhighlight %}

## Implementação

### Imports

{% highlight haskell %}
import Network.Wreq (Options, defaults, param, getWith, asValue, responseBody)
import Data.Text (Text)
import Data.Aeson (Value)
import Control.Lens (view, set, toListOf)
import Data.Aeson.Lens (key, _Array, _String)
{% endhighlight %}

### Tipos

{% highlight haskell %}
-- | A informação que nos importa de um evento do Meetup.
data EventInfo =
  EventInfo { eventName :: Text
            , venueName :: Text
            }
  deriving (Show)

-- | Um group ID do Meetup
type GroupId = Text
{% endhighlight %}

### A parte do cliente Web

Já que só estamos fazendo um request, e não estamos lidando com nenhum erro,
mas deixando o `wreq` atirar exceções, a parte do cliente Web é muito curta. A
API do Meetup pode retornar informação como JSON.

{% highlight haskell %}
meetupEventsUrl :: String
meetupEventsUrl = "https://api.meetup.com/2/events"
{% endhighlight %}

Nós fazemos um `GET` com alguns parâmetros QueryString. `wreq` usa "lens" como
sua DSL para criar opções para o `GET`, então vamos criar um valor `Options` do
`wreq`, configurando parâmetros um depois do outro usando o "builder pattern" e
começando com o `defaults`:

{% highlight haskell %}
eventsOptions :: GroupId
              -> Options
eventsOptions groupId =
  set (param "page") ["10"] (
    set (param "order") ["time"] (
      set (param "status") ["upcoming"] (
        set (param "group_id") [groupId] (
          set (param "format") ["json"] defaults))))
{% endhighlight %}

Então fazemos o request e conseguimos uma resposta, que é uma
[`ByteString` lazy](https://hackage.haskell.org/package/bytestring-0.10.6.0/docs/Data-ByteString-Lazy.html):

{% highlight haskell %}
getMeetupEventInfos :: GroupId -> IO [EventInfo]
getMeetupEventInfos groupId = do
  response <- getWith (eventsOptions groupId) meetupEventsUrl
{% endhighlight %}

### A parte do JSON

Então precisamos parsear essa resposta `ByteString` lazy, incluindo os headers,
em um objeto JSON, um [`Value`](https://hackage.haskell.org/package/aeson-0.10.0.0/docs/Data-Aeson.html#t:Value)
do `aeson`:

{% highlight haskell %}
  jsonResponse <- asValue response
{% endhighlight %}

O tipo `Value` é um ADT com muitas clausulas:

{% highlight haskell %}
type Object = HashMap Text Value

type Array = Vector Value

data Value = Object !Object
           | Array !Array
           | String !Text
           | Number !Scientific
           | Bool !Bool
           | Null
{% endhighlight %}

### A parte lens

Foi irritante descobrir a partir da API oficial do Meetup quais campos eu
precisava da resposta e quais eram seus tipos. Na prática, eu só salved o JSON
de um request que sabia que faria e olhei para os campos que queria. Me
disseram onde encontrar a documentação gerada de todos os métodos da API, mas
ela não era ideal. Um outro dia de Hackage vai discutir o que fiz quanto a esse
problema.

Nós extraímos a lista de eventos, usando uma "traversal" para extrair toda a
lista, que está codificada como um array JSON no campo "results" da resposta:

{% highlight haskell %}
  let events = toListOf (responseBody
                         . key "results"
                         . _Array . traverse
                        ) jsonResponse
{% endhighlight %}

Aqui nós usamos `toListOf` do lens com uma "traversal" e um objeto JSON para
extrair o que precisamos.

Finalmente, já que nós só queremos, para cada evento, seu nome e seu local:

{% highlight haskell %}
  return (map jsonToEventInfo events)
{% endhighlight %}

Usamos lens de novo, no nível de cada objeto de evento, para extrair o que
queremos deles:

{% highlight haskell %}
-- | Extrai o nosso modelo de um objeto JSON
jsonToEventInfo :: Value -> EventInfo
jsonToEventInfo json =
  EventInfo { eventName = view (key "name" . _String) json
            , venueName = view (key "venue"
                                . key "name" . _String) json
            }
{% endhighlight %}

Aqui usamos a função `view` do `lens`, para aplicar uma lens no objeto JSON e
extrair um campo dele.

E acabamos! Escrevemos um script que parece mais ou menos o que você escreveria
em Python ou Perl. Ele também vai "falhar" de formas similares, porque nós não
estamos usando muitos tipos; mesmo o resultado final só tem strings, que podem
ser vazias, por exemplo, o que quer que isso possa significar. Por exemplo, se
você tentar encontrar um campo que não existe, o código aqui vai só dar uma
string vazia. Podemos fazer melhor? Sim, de muitas maneiras. Fique ligado para
outro dia de Hackage.

## Sintaxe de operadores lens

Se você já usou o `wreq` ou o `lens`, você deve ter percebido algo estranho: eu
não usei nenhum operador do `lens`. Isso foi proposital. Apesar de que o
tutorial do `wreq` dá
[um pouco de _background_ para o lens](http://www.serpentine.com/wreq/tutorial.html#a-quick-lens-backgrounder),
a realidade é que quando pessoas que não são experientes com lens ou Haskellers
me perguntaram como programar clientes Web em Haskell, e eu disse que `wreq`
era uma boa escolha, eles travaram imediatamente na parte de lens. Olhando para
o tutorial, eu vejo que ele pula diretamente para uma sopa de operadores. É uma
pena. Você pode usar bibliotecas como o `wreq`, sem ter os operadores do lens
memorizados. Você tem que entender alguns fatos (como o uso de composição de
funções para compor _lenses_) e ter uma ideia de como os tipos funcionam, mas
uma coisa da qual você não precisa são os operadores engraçadinhos. Eu acho que
é melhor entender como fazer as coisas sem operadores antes de começar a os
usar.

Por exemplo, uma forma idiomática de criar o objeto de opções, como apresentado
na seção "whirlwind tour" do tutorial do `wreq`, é:

{% highlight haskell %}
import Control.Lens ((&), (.~))

eventsOptions :: GroupId
              -> Options
eventsOptions groupId = defaults
  & param "format" .~ ["json"]
  & param "group_id" .~ [groupId]
  & param "status" .~ ["upcoming"]
  & param "order" .~ ["time"]
  & param "page" .~ ["10"]
{% endhighlight %}

Eu não gosto da ideia de recem-chegados à biblioteca copiarem e colarem coisas
sem entender o que elas fazem, ou ter a ideia que esses operadores fazem parte
da linguagem Haskell ou que eles são um requisito para usar a biblioteca. As
pessoas têm essas impressões.

Eu gosto muito do operador de funções reverso `&`, apesar de que ele não é tão
sugestivo quanto o mesmo operador em outras linguagens (como F#, OCaml, Elm,
Elixir) que usam um pipe
[`|>`](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Basics#|%3E),
então não me importo de o usar.

Mas não acho o `.~` muito sugestivo para recem-chegados para o `lens`. Será que
`set lens newValue object` é tão pior de ler e escrever que `object & lens .~
newValue`?

## Code golf?

Para ilustrar tanto os pros e contras de usar operadores (mas nesse caso
principalmente contras, eu acho), aqui está uma versão "code golf" de todo o
código:

{% highlight haskell %}
import Network.Wreq (Options, defaults, param, getWith, asValue, responseBody)
import Data.Text (Text)
import Control.Lens ((&), (.~), (^.), (^..))
import Data.Aeson.Lens (key, _Array, _String)
import Control.Arrow ((>>>), (&&&))

meetupEventsUrl :: String
meetupEventsUrl = "https://api.meetup.com/2/events"

-- | Um group ID do Meetup
type GroupId = Text

-- | Para pesquisar por eventos em um grupo do Meetup.
eventsOptions :: GroupId
              -> Options
eventsOptions groupId = defaults
  & param "format" .~ ["json"]
  & param "group_id" .~ [groupId]
  & param "status" .~ ["upcoming"]
  & param "order" .~ ["time"]
  & param "page" .~ ["10"]

-- | Versão code gold. Não faça isso?
getMeetupNameAndVenues :: GroupId -> IO [(Text, Text)]
getMeetupNameAndVenues groupId =
  getWith (eventsOptions groupId) meetupEventsUrl
  >>= asValue
  >>= ((^.. responseBody
        . key "results"
        . _Array . traverse)
       >>> map ((^. key "name" . _String)
                 &&& (^. key "venue"
                      . key "name" . _String)
                 )
       >>> return
      )
{% endhighlight %}

De certa forma, isso parece legal porque o código com pipes à direita e à
esquerda é fácil e natural de ler; isso é, se você sabe todos os operadores e
gosta de combinadores point-free e secção de operadores. Mas quando eu mostrei
isso para meus amigos que não são tão fluentes em Haskell, eles não gostaram
muito. Note que eu fiz concessões para construir essa pipeline. Eu perdi
comentários, os procedimentos nomeados e até meu tipo definido do resultado. Eu
acho que algo foi perdido por escrever as coisas nesse estilo, ainda que alguma
parte de mim goste dele.

## Uma entrevista com Bryan O'Sullivan

Recentemente (Setembro de 2015), [o Haskell Cast](http://www.haskellcast.com/)
entrevistou Bryan O'Sullivan. Eu recomendo escutar todo o [episódio](
http://www.haskellcast.com/episode/010-bryan-osullivan-on-performance-and-efficiency/).
Ele tinha histórias para contar sobre como ele começou com Haskell, como ele
acabou escrevendo todas essas bibliotecas e como ele desenha elas e quais são
suas metas enquando as implementa. Note que `aeson` e `text`, que todo mundo
usa, são suas criações. Obrigado, Bryan, por tudo que fez para a comunidade
Haskell!

## Recursos sobre lens

Gabriel Gonzalez escreveu um
[tutorial sobre lens](https://hackage.haskell.org/package/lens-tutorial)
que é útil. Obrigado, Gabriel, por escrever tutorials não só para suas próprias
bibliotecas, mas também para outras!

## Conclusão

Para o dia 4, eu apresentei um pequeno exemplo que usa `wreq` com `aeson` e
`lens` para realizar uma tarefa simples de extrair informação da Web e tentei
fazer o `wreq` um pouco mais acessível sem requerir o uso dos operadores `lens`
de cara.

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

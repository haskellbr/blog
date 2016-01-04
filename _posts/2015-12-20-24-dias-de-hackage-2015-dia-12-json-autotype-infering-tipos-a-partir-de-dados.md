---
layout: post
title: "24 dias de Hackage 2015 - dia 12 - json-autotype: Inferindo tipos a partir de dados"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original.](http://conscientiousprogrammer.com/blog/2015/12/12/24-days-of-hackage-2015-day-12-json-autotype-inferring-types-from-data/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

# Dia 12

Hoje, revisitamos um problema do
[dia 4](/2015/12/12/24-dias-de-hackage-2015-dia-4-wreq-programacao-de-clientes-web-com-notas-sobre-lens-e-a-sintaxe-de-operadores.html),
onde nós pegamos um JSON da Web para extrair informações sobre ele. Eu
mencionei então que estavámos usando uma representação não tipada para o
JSON. Idealmente, se não estamos sob muita pressão para realizar a tarefa,
queremos ter uma representação tipada dos dados, para evitar erros comuns como
tentar acessar um campo que não existe. A biblioteca `Aeson` nos permite
escrever nossas próprias estruturas de dados e as converter de e para JSON. Se
estivermos sob controle dos dados e do modelo de dados, esse é o caminho que
queremos seguir.

<!-- more -->

E se, por qualquer razão, nós não já tivermos um modelo de dados tipado, mas,
por exemplo, estivermos consumindo JSON de uma fonte que não nos deu uma
especificação das estruturas de dados? (Talvez isso pudesse ser feito com JSON
Schemas). Então, precisaríamos fazer engenharia reversa dos tipos de dados,
possivelmente a partir de uma especificação informal das estruturas (o que é
algo problemático).

Ou poderíamos tomar a rota preguiçosa e *interir* tipos plausíveis a partir de
alguma amostra representativa dos dados. Isso é o que o útil pacote
[`json-autotype`](http://hackage.haskell.org/package/json-autotype) faz. Sua
documentação está na
[sua página do GitHub](https://github.com/mgajda/json-autotype)

## Gerando um módulo de tipos a partir de dados JSON

Eu salvei um pequeno documento JSON,
`pittsburgh-code-and-supply-events.json` de uma query para a API de eventos do
Meetup.

A forma mais fácil de usar o `json-autotype` é usar sua ferramenta de linha de
comando, a instalando globalmente antes de qualquer outra coisa.

{% highlight console %}
$ stack install json-autotype
{% endhighlight %}

Para nosso exemplo rápido, eu só gerei um pouco de Haskell manualmente
(idealmente, isso faz parte de um processo de compilação automatizado):

{% highlight console %}
$ json-autotype pittsburgh-code-and-supply-events.json -o
MeetupEventsJSON.hs
$ mkdir generated-src
$ mv MeetupEventsJSon.hs generated-src/
{% endhighlight %}

Note que, apesar de que para esse exemplo nós só rodamos a inferência em um
único documento JSON, podemos a rodar em vários documentos e receber tipos mais
precisos.

### Um olhar de relance no código gerado

Aqui está um exemplo do que foi gerado (reformatado e editado por clareza):

{% highlight haskell %}
{-# LANGUAGE TypeOperators #-}

import Data.Aeson.AutoType.Alternative ((:|:))

data TopLevel = TopLevel {
    topLevelResults :: [ResultsElt],
    topLevelMeta :: Meta
  } deriving (Show,Eq,Generic)

data ResultsElt = ResultsElt {
    resultsEltStatus :: Text,
    resultsEltGroup :: Group,
    resultsEltTime :: Int,
    resultsEltWaitlistCount :: Int,
    resultsEltVenue :: (Maybe (Venue:|:[(Maybe Value)])),
    resultsEltCreated :: Int,
    resultsEltUtcOffset :: Int,
    resultsEltEventUrl :: Text,
    resultsEltYesRsvpCount :: Int,
    resultsEltHeadcount :: Int,
    resultsEltFee :: (Maybe (Fee:|:[(Maybe Value)])),
    resultsEltVisibility :: Text,
    resultsEltMaybeRsvpCount :: Int,
    resultsEltName :: Text,
    resultsEltId :: Text,
    resultsEltRsvpLimit :: (Maybe (Int:|:[(Maybe Value)])),
    resultsEltUpdated :: Int,
    resultsEltDuration :: (Maybe (Int:|:[(Maybe Value)])),
    resultsEltDescription :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)

instance FromJSON ResultsElt where
  parseJSON (Object v) = ResultsElt
    <$> v .:   "status"
    <*> v .:   "group"
    <*> v .:   "time"
    <*> v .:   "waitlist_count"
    <*> v .:?? "venue"
    <*> v .:   "created"
    <*> v .:   "utc_offset"
    <*> v .:   "event_url"
    <*> v .:   "yes_rsvp_count"
    <*> v .:   "headcount"
    <*> v .:?? "fee"
    <*> v .:   "visibility"
    <*> v .:   "maybe_rsvp_count"
    <*> v .:   "name"
    <*> v .:   "id"
    <*> v .:?? "rsvp_limit"
    <*> v .:   "updated"
    <*> v .:?? "duration"
    <*> v .:?? "description"
  parseJSON _          = mzero

data Venue = Venue {
    venueRepinned :: Bool,
    venueState :: Text,
    venueCountry :: Text,
    venueZip :: (Maybe (Text:|:[(Maybe Value)])),
    venueLat :: Int,
    venueName :: Text,
    venueCity :: Text,
    venueId :: Int,
    venueLon :: Int,
    venueAddress1 :: Text
  } deriving (Show,Eq,Generic)
{% endhighlight %}

Não é muito melhor do que inferir e escrever o _boilerplate_ na mão?
Podemos _"parsear"_ JSON diretamente para nosso conjunto de tipos.

O operador engraçadinho `:|:` é um construtor de um tipo parecido com o
`Either` usado para lidar com o fato de que, quando um campo está faltando em
um dos dados de exemplo, nós não podemos inferir se ele pode potencialmente ser
um objeto complexo que nós simplesmente desconhecemos na nossa amostra.

## Usando os tipos gerados

Algums imports:

{% highlight haskell %}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module JSONAutoTypeExample where

-- Re-utilizando o que fizemos no dia 4
import WreqExample (EventInfo(..), GroupId, getMeetupEventsJSONBytes)

-- O módulo automaticamente gerato pelo json-autotype.
import qualified MeetupEventsJSON as Meetup
import qualified Data.Aeson as Aeson
import Control.Arrow ((>>>))
import Data.Aeson.AutoType.Alternative ((:|:)(AltLeft, AltRight))
import qualified Data.Text as Text
{% endhighlight %}

Nosso substituto para o `getMeetupEventInfos`:

{% highlight haskell %}
getMeetupEventInfos :: GroupId -> IO (Either String [EventInfo])
getMeetupEventInfos groupId =
  getMeetupEventsJSONBytes groupId
  >>= (Aeson.eitherDecode
       >>> fmap extractEventInfos
       >>> return
      )
{% endhighlight %}

Note imediatamente as diferenças com a versão anterior. Nós estamos usando
`Aeson.eitherDecode` para transformar bytes de JSON em um tipo completo
`Meetup.TopLevel` e, portanto, podemos detectar de cara se o JSON que recebemos
era válido (no sentido de ter a mesma estrutura dos tipos que inferimos e
conhecemos).

Extraindo eventos:

{% highlight haskell %}
extractEventInfos :: Meetup.TopLevel -> [EventInfo]
extractEventInfos =
  Meetup.topLevelResults
  >>> map extractEventInfo
{% endhighlight %}

Isso também é diferente do que tínhamos antes, já que não estamos só usando
strings para (se tudo der certo) encontrar os campos que queremos no JSON. Nós
temos uma estrutura de dados `Meetup.TopLevel` e só usamos o acesso a
propriedades normal para records e mergulhamos nos dados.

Extraindo informação de um só evento:

{% highlight haskell %}
extractEventInfo :: Meetup.ResultsElt -> EventInfo
extractEventInfo event =
  EventInfo { eventName = Meetup.resultsEltName event
            , venueName = extractVenueName (Meetup.resultsEltVenue event)
            }
{% endhighlight %}

De novo, nós só usamos os campos tipados de `Meetup.ResultsElt`, ao invés de
strings como chaves em um objeto.

Finalmente, indo até o talo tentando extrair o nome do local a partir do objeto
do local de um evento:

{% highlight haskell %}
-- | É mais difícil porque o json-autotype encontrou eventos que não tinham um
-- local (mas mais correto)!
extractVenueName :: Maybe (Meetup.Venue :|: [Maybe Aeson.Value]) -> Text.Text
extractVenueName Nothing = ""
extractVenueName (Just (AltLeft venue)) = Meetup.venueName venue
extractVenueName (Just (AltRight jsonValues)) =
  Text.pack ("(unexpected JSON venue: " ++ show jsonValues ++ ")")
{% endhighlight %}

Aqui nós percebemos que talvez não haja nenhum local ou que o campo de JSON
`venue` não corresponda ao tipo de dados `Venue`. Na prática, reconhecer esse
fato pode resultar em nós mudarmos o nosso tipo `EventInfo` para que ele não
assuma que tem um `Text` como seu `venueName`, mas aqui e agora nós só ficamos
no mundo _"tipado por strings"_ e tentamos retornar uma string útil em todos os
casos.

De qualquer forma, você pode ver como ter uma estrutura de dados definida para
um JSON pode nos levar a repensar o que pressupomos quando desenhamos nosso
tipo `EventInfo`. Talvez até melhorar nosso desenho. O que é legal no
`json-autotype` é que você pode o usar sem ter que escrever os tipos para o
JSON na mão.

## Provedores de tipos

A linguagem F# provem suporte para
[_provedores de tipos_](https://msdn.microsoft.com/en-us/library/hh156509.aspx),
que são formas de conseguir tipos de algum lugar sem ter que os escrever você
mesmo. Isso é uma funcionalidade muito legal que eu gostaria de ver em
ecossistemas de linguagens mais tipadas. Por exemplo, existe uma biblioteca de
[_provedores de tipos_ para Idris](https://github.com/david-christiansen/idris-type-providers).

Eu desconheço quanto trabalho existe em um ecossistema de _provedores de tipos_
para Haskell, mas eu imagino que você poderia usar Template Haskell para, por
exemplo, automatizar um pouco do que fizemos aqui com o `json-autotype` como
uma forma crua de um _provedor de tipos_.

## Conclusão
`json-autotype` é uma boa biblioteca que ajuda em entender o JSON que aparece
no seu caminho, mas sem um conjunto de tipos já especificado. Ele infere os
tipos e escreve um módulo de Haskell automaticamete.

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

[Há um milestone no GitHub com tarefas esperando por você.](https://github.com/haskellbr/blog/milestones/24%20dias%20de%20Hackage%202015)

[Não quer traduzir posts? Escreva código que impacta o ecossistema de Haskell conosco](https://github.com/haskellbr)

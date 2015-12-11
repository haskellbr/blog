---
layout: post
title: "24 dias de Hackage, 2015 - dia 3 - HSpec; A importância de testes"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original](http://conscientiousprogrammer.com/blog/2015/12/03/24-days-of-hackage-2015-day-3-hspec-the-importance-of-testing/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

- - -

# Dia 3

Passei meus anos formadores escrevendo software antes de "framework de testes"
estar no meu vocabulário; antes de ["desenvolvimento guiado a testes"
(TDD)](https://en.wikipedia.org/wiki/Test-driven_development) ser uma tendencia.
Eu tenho arrepios de pensar nesses anos, porque agora eu sou um crente dos
testes e do desenvolvimento guiado a testes (TDD) - segundo minha interpretação
do que isso significa (já que todos temos definições diferentes).

Há uma série de ferramentas de testes disponíveis para o ecossistema de Haskell
há algum tempo. Sendo mais preciso, Ollie nos seus "24 dias de Hackage" cobriu:

- [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck)
[em 2012](https://ocharles.org.uk/blog/posts/2012-12-08-24-days-of-hackage.html)
- [`doctest`](https://hackage.haskell.org/package/doctest) [em 2013](https://ocharles.org.uk/blog/posts/2013-12-18-doctest.html)
- [`tasty`](http://documentup.com/feuerbach/tasty) [em 2013](https://ocharles.org.uk/blog/posts/2013-12-03-24-days-of-hackage-tasty.html)

e sinceramente recomendo pesquisar sobre essas ferramentas.

Mas hoje, vou mostrar o uso do [`HSpec`](http://hspec.github.io/) (noto que um
framework como o `tasty` ou o
[`test-framework`](`https://batterseapower.github.io/test-framework/) são muito
mais completos _[N.T. "fancy"]_).

- - -

## Por que testes?

Eu comecei a escrever testes por dois motivos:

- Usando linguagens como Perl, era essencialmente impossível de ser produtivo
  sem escrever testes.
- Esse tipo de linguagem deu origem às ferramentas para diminuir a dor de
  escrever, rodar e ter feedback dos testes.

Mas depois que comecei, eu não olhei pra trás, mesmo usando outras linguagens
como Scala ou Haskell. Hoje, independente da linguagem que estiver usando,
espero ter um framework de testes decente que possa começar a usar
imediatamente. Eu até fiz o experimento de
[aprender uma linguagem novíssima, Elixir, escrevendo testes](http://conscientiousprogrammer.com/blog/2013/08/26/openhack-pittsburgh-learning-elixir-test-driven-and-package-publishing/).
Não posso levar um ecossistema a sério se não há pelo menos um framework de
testes padrão que faça parte dele.

Há o mito (ou piada) sobre usar linguagens como Haskell que tem um sistema de
tipos decente: você não precisa de testes, porque você tem tipos. Daí vem o
ditado infortúno "testes versus tipos". Discordo completamente disso. Eu
quero meus tipos e também meus testes: quero usar todas as ferramentas possíveis
que me ajudem a desenhar, verificar e consertar meu código! No [Pittsburgh TechFest](http://pghtechfest.com/) 2014,
dei uma palestra ["Exploring type-directed, test-driven development" _[N.T. "Explorando desenvolvimento guiado a testes e direcionado por tipos"]_](http://www.slideshare.net/FranklinChen/presentation-37257104)
dando minha opinião pessoal sobre fazer o melhor uso de tipos e testes como
parte de um processo iterativo de refinar, entender e expressar uma solução para
uma tarefa (esse era antes do termo "desenvolvimento direcionado a tipos" virar
o título de um livro que será lançado em breve sobre usar Idris,
"Desenvolvimento direcionado a tipos com Idris")

O tópico geral sobre como combinar testes e tipos da melhor forma está bem fora
do escopo desse artigo, mas eu queria fazer um ponto: o maior benefício dos
testes vem do seu papel como *documentação explícita da intenção durante um
processo de desenho*. Idealmente, nós preferimos escrever tipos espressivos que
codifiquem a intenção totalmente, e linguagens com tipos dependentes como Idris
permitem transformar muito do que é costumavam ser testes de runtime em testes
durante a compilação por meio de checagem de tipos. Você pode fazer uma boa
parte desse tipo de coisa com Haskell _hoje_ se trabalhar duro o suficiente (e
[Haskell Dependente](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell) é
um trabalho em progresso), mas não há nada de errado em escrever testes hoje que
talvez virem tipos algum dia.

### Extra! Extra!

Por mera coincidencia, [um novo framework de testes acaba de ser anunciado para
o OCaml](https://blogs.janestreet.com/testing-with-expectations/).

E hoje, logo depois de publicar esse artigo, eu encontrei no meu feed de
notícias um anúncio sobre [QuickFuzz](http://quickfuzz.org/), um _grammar fuzz
tester_ para Haskell!

É ótimo que testes estejam sendo levados mais a sério em todos os lugares e por
todos.

## Por que HSpec?

Por que eu uso HSpec e não outras frameworks de testes com mais funções? Eu não
excluo a possibilidade de migrar para uma delas no futuro, mas por enquanto,
usar o HSpec é muito fácil e confortável, e é bom o bastante para mim. Eu sou
tão preguiçoso que sou capaz de não escrever testes se ficar intimidado por
qualquer possibilidade de fricção. E vou adimitir que o [Web site](http://hspec.github.io/)
do framework é muito bom! Acho que marketing importa.

Além disso, quando usava Ruby, eu fiquei acostumado em usar o [RSpec](http://rspec.info/),
que claramente foi a inspiração para o HSpec.

## É tudo sobre descoberta automática

Antes de dizer qualquer outra coisa sobre o HSpec, quero dizer que um dos
motivos para eu o usar foi a descoberta automática de testes. Dê uma olhada no
[manual](http://hspec.github.io/hspec-discover.html) para detalhes.

Descoberta automática significa que dado um boilerplate simples, você pode usar
"convenção sobre configuração" e só criar arquivos chamados
"AlgumaCoisa**Spec**.hs" e os botar em qualquer lugar no seu diretório de testes
e eles serão compilados e executados quando você executar `stack test`. Isso
significa que podemos escrever módulos de teste quando bem entendermos, os
mover, apagar, adicionar e refatorar sem ter que nos preocupar com escrever um
módulo que amarre as pontas, importanto todos os módulos de testes e juntando
eles em um só suite de testes.

Aqui está o setup que eu tenho em todos os meus projetos que usam HSpec. Eu
o provi no meu template de projeto de exemplo descrito no
[dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html),
então você pode gerar um projeto pronto com HSpec rodando:

{% highlight console %}
$ stack new my-new-project franklinchen
{% endhighlight %}

Há um diretório `test/` com um único arquivo nele, o arquivo de descoberta
automática chamado `test/Spec.hs`, que tem uma linha de código; um comentário:

{% highlight haskell %}
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{% endhighlight %}

Isso funciona porque quando você instala o HSpec, um programa `hspec-discover`
também é instalado, e ele é chamado pelo GHC para fazer o trabalho. Cada módulo
de testes deve exportar `spec`, porque isso é que o programa vai tentar chamar.

## Escrevendo e refatorando testes

Eu não mencionei no [meu post de ontem sobre usar uma regex](/2015/12/09/24-dias-de-hackage-2015-dia-2-expressoes-regulares-com-pcre-heavy-scripts-standalone-usando-stack.html)
para resolver um problema, mas quando eu escrevi uma série de exemplos de
strings que deveriam dar _match_ e outras que não deveriam dar _match_, eu
simplesmente copiei-e-colei eles dos testes que tinha escrito.

Vamos desconstruir o módulo de testes `PCREHeavyExampleSpec.hs`, passo por
passo.

### Versão inicial do código de testes

Primeiro, vou apresentar código que eu nunca escrevi inicialmente, porque eu
pulei essa etapa e refatorei o código na minha cabeça de imediato. Mas eu decidi
que para mostrar a força de Haskell como uma linguagem para embedar linguagens
de domínio específico (DSLs), eu retroativamente escrevi o código mais óbvio que
mostra como o HSpec funciona sem introduzir coisas que não tem a ver com ele. (O
código está no branch [`boilerplated-hspec`](https://github.com/FranklinChen/twenty-four-days2015-of-hackage/tree/boilerplated-hspec).)

{% highlight haskell %}
module PCREHeavyExampleSpec where

import PCREHeavyExample (mediaRegex)

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Text.Regex.PCRE.Heavy ((=~))

-- | Obrigatório para a descoberta automática
spec :: Spec
spec =
  describe "pcre-heavy" $ do
    describe "match" $ do
      it "has audio" $ do
        "@Media:\thas-audio,   audio" `shouldSatisfy` (=~ mediaRegex)
      it "has video" $ do
        "@Media:\thas-video,video" `shouldSatisfy` (=~ mediaRegex)
      it "has audio but missing" $ do
        "@Media:\thas-audio-but-missing, audio, missing" `shouldSatisfy` (=~ mediaRegex)
      it "has video but unlinked" $ do
        "@Media:\thas-video-but-unlinked  , video,      unlinked" `shouldSatisfy` (=~ mediaRegex)
    describe "no match" $ do
      it "no audio or video" $ do
        "@Media:\tno-audio-or-video" `shouldSatisfy` (not . (=~ mediaRegex))
      it "missing media field" $ do
        "@Media:\tmissing-media-field, unlinked" `shouldSatisfy` (not . (=~ mediaRegex))
{% endhighlight %}

A coisa principal para entender é que o uso mais simples (sem fixtures, efeitos
etc.), um teste unitário com uma descrição é introduzido com `it` e um
`describe` pode conter muitos desses, assim como outros `describe`s filhos.

Aqui, nós temos dois sub-`Spec`s, um para os exemplos que **devem dar _match_**
no regex e um para os exemplos que **não o devem**.

Note que importamos e usamos `mediaRegex` do módulo `PCREHeavyExample`.

Diferente do nosso programa de exemplo de ontem, que usou `scan` do `pcre-heavy`
para coletar bindings dos _matches_, nós só nos importamos se algo deu _match_
ou não. Por isso usamos o operador `=~` ao invês do `scal`. Ele pega uma string
e uma regex e retorna um `Bool`.

O código dos testes é conciso o bastante e o domínio do problema bem entendido.
Mesmo que a sintaxe pareça estranha, espero que seja claro **o quê** está
acontecendo, ainda que não seja claro **como**.

### Uma nota sobre sintaxe em código Haskell

Agora é um bom momento para falar sobre o problema de sintaxe no código escrito
em Haskell, porque estou assumindo que você possa ainda não conhecer o HSpec, e
não posso assumir seja um desenvolvedor de Haskell experiente, porque não estou
escrevendo essa série para Haskellers avançados mas para as pessoas que estão
começando a molhar os pés no ecossistema e com experiência limitada com Haskell.

É conveniente usar o estilo "point-free" acima, mas poderia ter escrito:

{% highlight haskell %}
text `shouldSatisfy` (\inputString -> inputString =~ mediaRegex)
{% endhighlight %}

Além disso, também é conveniente usar a
[sintaxe infixa para funções nomeadas](https://wiki.haskell.org/Infix_operator)
quando é sensível, mas não é necessário. Poderia ter escrito:

{% highlight haskell %}
shouldSatisfy text (\inputString -> inputString =~ mediaRegex)
{% endhighlight %}

E o `(not . (=~ mediaRegex))` pode ser escrito como:

{% highlight haskell %}
\inputString -> not (inputString =~ mediaRegex)
{% endhighlight %}

Eu menciono isso, porque eu ouvi muitas vezes de pessoas começando a estudar
Haskell que a linguagem é confusa por causa de todos os operadores. Mas você não
é obrigado a usar essa sintaxe se você não quiser. Muito do que parece estranho
em Haskell não é algo sobre a linguagem em si, mas sobre uma sintaxe opcional e
estilo. Não é só sobre operadores, mas também sobre muito mais sintaxe opcional
também. Se você é relativamente novo à sintaxe, o Gabriel Gonzalez escreveu um
bom tutorial sobre "decodificar a sintaxe" cobrindo um pouco disso [aqui](http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html).

Mas dizer "você não precisa escrever" não sinifica nada se "todos estão
escrevendo assim" e você tem que *ler* o código de qualquer jeito. Mas é a
comunidade como um todo que dita o tom do que é escrito e lido.

Um motivo pelo qual eu gosto do HSpec é que ele não exagera com a sintaxe.
O [HUnit](https://hackage.haskell.org/package/HUnit), um framework de testes
mais antigo, provê operators engraçadinhos que me desanimaram um pouco,
como
[`~=?`](https://hackage.haskell.org/package/HUnit-1.3.0.0/docs/Test-HUnit-Base.html).
Eu gosto do artigo do Gabriel Gonzalez
["How to make your Haskell code more readable to non-Haskell programmers" _["Como fazer seu código em Haskell mais legível para programadores de outras linguagens"]_](http://www.haskellforall.com/2015/09/how-to-make-your-haskell-code-more.html).
Ele também se aplica em fazer o código mais legível para Haskellers experientes!

Eu Adimito que sou culpado de algumas práticas que ele denuncia. Não sei se devo
desistir de todas elas, o tempo todo. Por exemplo, parece idiomático usar o
operador `$` para DSLs como o HSpec, ao invés de parentisar tudo. Estou curioso
sobre o que você acha. Você preferiria ler o seguinte, que é o que o operador
`$` está evitando?

{% highlight haskell %}
spec :: Spec
spec =
  describe "pcre-heavy" (do
    describe "match" (do
      it "has audio" (do
        "@Media:\thas-audio,   audio" `shouldSatisfy` (=~ mediaRegex)
        )
      -- ...
      )
    describe "no match" (do
      -- ...
      )
    )
{% endhighlight %}

Eu acho que linguagens com um bloco `begin`/`end` (como Pascal, Ruby), ao invés
de colchetes ou parenteses, tem uma vantagem por serem mais legíveis (para mim).
Uma pesquisa recente
["An empirical investigation into programming language syntax" _[N.T. "Uma investigação empírica da sintaxe das linguagens de programação"]](http://dl.acm.org/citation.cfm?id=2534973)
afirma ter evidencias disso.

Enquanto isso, nós fazemos o que podemos com a linguagem que temos e
aprendemos/ensinamos suas partes estranhas e funcionalidades. É lamentável
também que o Inglês e o Chinês são línguas difíceis de se usa, mas nós
precisamos relevar isso se queremos fazer parte da comunidade na China ou nos
Estados Unidos. E vai nos dois sentidos: se eu quiser fazer parte da comunidade,
eu tenho que investir em a entender, e se a comunidade quiser crescer, ela tem
que puxar pessoas pra dentro, ao invés de dizer _"deal with it"_. Pense na
quantidade de tempo enorme que vai em promover alfabetismo universal.

### Um review de um minuto do TDD

Vamos continuar da onde estávamos com o processo de escrever testes.

Quando estamos praticando TDD, nós escrevemos uma suite do HSpec primeiro,
*antes mesmo de escrever qualquer implementação*. TDD é quando você mostra como
algo deve funcionar antes de você de fato escrever essa coisa. Em um contexto
com tipos estáticos, isso significa que nós recebemos um erro de compilação
quando tentamos escrever o teste, que nós consertamos criando `PCREHeavyExample`
como um novo módulo com um stub:

{% highlight haskell %}
module PCREHeavyExample (mediaRegex) where

mediaRegex = undefined
{% endhighlight %}

Claro, todos os testes falham (no terminal os erros ficam em vermelho):

{% highlight console %}
$ stack test
PCREHeavyExample
  pcre-heavy
    match
      has audio FAILED [1]
      has video FAILED [2]
      has audio but missing FAILED [3]
      has video but unlinked FAILED [4]
    no match
      no audio or video FAILED [5]
      missing media field FAILED [6]

Failures:

  test/PCREHeavyExampleSpec.hs:13:
  1) PCREHeavyExample.pcre-heavy.match has audio
       uncaught exception: ErrorCall (Prelude.undefined)

  test/PCREHeavyExampleSpec.hs:15:
  2) PCREHeavyExample.pcre-heavy.match has video
       uncaught exception: ErrorCall (Prelude.undefined)

  test/PCREHeavyExampleSpec.hs:17:
  3) PCREHeavyExample.pcre-heavy.match has audio but missing
       uncaught exception: ErrorCall (Prelude.undefined)

  test/PCREHeavyExampleSpec.hs:19:
  4) PCREHeavyExample.pcre-heavy.match has video but unlinked
       uncaught exception: ErrorCall (Prelude.undefined)

  test/PCREHeavyExampleSpec.hs:22:
  5) PCREHeavyExample.pcre-heavy, no match, no audio or video
       uncaught exception: ErrorCall (Prelude.undefined)

  test/PCREHeavyExampleSpec.hs:24:
  6) PCREHeavyExample.pcre-heavy, no match, missing media field
       uncaught exception: ErrorCall (Prelude.undefined)
{% endhighlight %}

#### Na tangente dos relatórios de erros do GHC

Algo muito irritante, e um problema de longa-data do GHC, é que usar `undefined`
não produz logs úteis. Eu estou esperando anciosamente pela nova feature do
[GHC 8.0](https://ghc.haskell.org/trac/ghc/wiki/Status/GHC-8.0.1)
de
[parâmetros implícitos contendo a localização do erro nocódigo](https://ghc.haskell.org/trac/ghc/wiki/ExplicitCallStack/ImplicitLocations).
Esse tipo de coisa é importante! Está na hora de termos números de linhas e
call stacks para erros sem ter que nos contorcer.

### Pulando para o final, assuma que terminamos a implementação

Ok, vamos assumir que nós terminamos a implementação, que é simplesmente
escrever a regex para `mediaRegex`. Agora os testes passam (e são exibidos em
verde no terminal):

{% highlight console %}
PCREHeavyExample
  pcre-heavy
    match
      has audio
      has video
      has audio but missing
      has video but unlinked
    no match
      no audio or video
      missing media field

Finished in 0.0010 seconds
6 examples, 0 failures
{% endhighlight %}

## Testes são código também!

É fácil não levar o código de testes a sério e não o elevar aos mesmos padrões
do resto do código. Isso é um erro: o código dos testes deveria ser mais limpo e
bem fatorado que a implementação principal, porque é nossa *documentação
executável* e é o que precisamos deixar fácil de ler, escrever e modificar
quando os requisitos mudam.

### Refatorando, parte 1

Perceba a quantidade tremenda de duplicação de código nos testes. Nós podemos
fazer melhor que esse trabalho item-por-item copia-e-cola. Podemos escrever
código para gerar todos os exemplos, refatorando os dados em uma tabela e
extraíndo uma função para conseguir um `Spec`.

Aqui está a tabela que liga a descrição dos testes com cada exemplo de string de
input:

{% highlight haskell %}
matchExamples :: [(String, String)]
matchExamples =
  [ ( "has audio"
    , "@Media:\thas-audio,   audio"
    )
  , ( "has video"
    , "@Media:\thas-video,video"
    )
  , ( "has audio but missing"
    , "@Media:\thas-audio-but-missing, audio, missing"
    )
  , ( "has video but unlinked"
    , "@Media:\thas-video-but-unlinked  , video,      unlinked"
    )
  ]

{% endhighlight %}

Aqui está uma função que gera um item de spec dado um desses pares:

{% highlight haskell %}
matchSpec :: (String, String) -> Spec
matchSpec (description, text) =
  it description $ do
    text `shouldSatisfy` (=~ mediaRegex)
{% endhighlight %}

Faríamos o mesmo para exemplos que não devem dar _match_.

E o spec refatorado:

{% highlight haskell %}
spec :: Spec
spec =
  describe "pcre-heavy" $ do
    describe "match" $ do
      mapM_ matchSpec matchExamples
    describe "no match" $ do
      mapM_ nonMatchSpec nonMatchExamples
{% endhighlight %}

### Refatorando parte 2

Uh oh, eu disse "o mesmo". Em geral quando algo é "o mesmo", podemos refatorar
um pouco mais.

Mas **Haskell faz a refatoração ser prazeiroza**.

Haskell é uma linguagem expressiva, onde "talvez poder", normalmente significa
"poder" e "poder" frequentemente significa "dever". Na minha experiencia, a
melhor qualidade da linguagem de programação Haskell em termos da experiência do
usuário é o seu suporte para refatorar a vontade e com confiança de que tudo
ainda vai significar exatamente a mesma coisa antes e depois de mexer no código.

Eu estou esperando particularmente o desenvolvimento de um
[engine universal para uma IDE de Haskell](https://github.com/haskell/haskell-ide-engine)
refatorar ainda mais fácilmente, e.g., suporte a _folding in_
[`HaRe`](http://www.cs.kent.ac.uk/projects/refactor-fp/).

Vemos um padrão de exemplos positivos e negativos usando um predicado e sua
negação. Vamos abstrair esse padrão. Vamos coletar os exemplos positivos e
negativos em um só lugar. Para simplificar, vamos os botar em uma tupla.

E agora que estamos lidando com predicados arbitrários, nós não temos mais que
espalhar `(=~ mediaRegex)` ou `String` em todo lugar. Podemos usar o
polimorfismo no tipo do predicado, substituindo `matchSpec` e `nonMatchSpec` com
um único `predSpec`.

O resultado final

{% highlight haskell %}
spec :: Spec
spec =
  describePredicate "pcre-heavy"
    ("match", (=~ mediaRegex))
    (matchExamples, nonMatchExamples)

describePredicate :: Show a =>
     String                           -- ^ description
  -> (String, a -> Bool)              -- ^ (base description, predicate)
  -> ( [(String, a)], [(String, a)] ) -- ^ positive and negative examples
  -> Spec
describePredicate description
                  (baseDescription, predicate)
                  (positiveExamples, negativeExamples) =
  describe description $ do
    describe baseDescription $ do
      mapM_ (predSpec predicate) positiveExamples
    describe ("not " ++ baseDescription) $ do
      mapM_ (predSpec (not . predicate)) negativeExamples

predSpec :: Show a => (a -> Bool) -> (String, a) -> Spec
predSpec predicate (description, a) =
  it description $ do
    a `shouldSatisfy` predicate
{% endhighlight %}

Note que `describePredicate` e `predSpec` podem então ser isolados em um módulo
de utilitários para testes para usar em outros specs com o mesmo padrão.

Infelizmente, essa refactoração, apesar de boa em alguns sentidos, veio com um
custo. Não parece muito bom para mim. Parece pra você?

### Refatoração, parte 3

Ona razão pela qual o código refatorado não parece bom é que nossa refatoração
trouxe muitos tipos primitivos dentro de outros (["obsessão primitiva"](http://c2.com/cgi/wiki?PrimitiveObsession))
e uma explosão no número de argumentos posicionais para nosso novo
`describePredicate`. Combinemos, chamar `describePredicate` é criptico, chamando
para "argumentos nomeados" (em uma linguagem que os suporte).

Em Haskell, "argumentos nomeados" significam que há uma estrutura de dados de
configuração gritando para ser definida. Um indício do problema é que documentar
os parâmetros da última versão é bem estranho. Cada um dos parâmetros deveria
ser sua própria coisa, não só uma sopa de tuplas.

Se nós estivermos sérios mesmo sobre refatorar, deveríamos amarrar botar nossos
dados em tipos que são um modelo explícito do que queremos fazer quando
classificando e testando exemplos. Nós podemos até transformar tudo em sua
própria DSL embedada no HSpec.

Isso ilustra como a refatoração pode as vezes trazer mais complexidade que ainda
não existia. Há muitas perdas e ganhos constantemente. Abstração por abstração
nem sempre faz as coisas ficarem mais claras. Por esse motivo, eu não fui tão
longe inicialmente para o código do exemplo de ontem. Achei que simplesmente não
valia a pena. Deixei essa última versão no branch 
[`refactoring-2`](https://github.com/FranklinChen/twenty-four-days2015-of-hackage/tree/refactoring-2)
do repositório.

## Combinando frameworks de testes

Uma última coisa sobre o HSpec: você pode o usar dentro de um framework de
testes maior ou embedar outro framework de testes dentro dele. Por exemplo, eu
gosto de usar o
[QuickCheck no HSpec](http://hspec.github.io/quickcheck.html) como parte do
"desenvolvimenteo direcionado a tipos".

## Conclusão

Testes são importantes, mas poucos amam os escrever. Fazer com que os testes
sejam fáceis de escrever e usar ajuda muito com isso. Eu gosto do HSpec porque é
simples de escrever e por causa da descoberta automática. Espero que você
considere o usar nos seus próprios projetos se você já não usa outro framework
de testes.

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

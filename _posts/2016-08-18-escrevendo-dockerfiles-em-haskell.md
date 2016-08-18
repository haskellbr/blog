---
layout: post
title: Escrevendo Dockerfiles em Haskell
author: Pedro Tacla Yamada
author_url: "https://github.com/yamadapc"
---
Nesse post, anuncio o
[`language-dockerfile`](http://hackage.haskell.org/package/language-dockerfile),
um parser, pretty-printer, linter e EDSL para escrever **Dockerfiles** em
**Haskell**.  Tentarei explicar sua motivação, como o usar e onde gostaria de
chegar no futuro. Fica para um outro post discutir sua implementação usando
**Monads** livres.

- - -

**Dia 27/09/2016**, teremos o **8º Encontro de Haskellers de São Paulo** na
Amazon. Reserve sua vaga no
[Meetup.com](http://www.meetup.com/haskellbr-sp/events/233395066/) e envie sua
palestra e/ou ideia para
[https://goo.gl/forms/5Xh3VbziQld29pFw1](https://goo.gl/forms/5Xh3VbziQld29pFw1).

- - -

Minha agenda será:

- Apresentar e problematizar dois exemplos de fricção entre o óbvio e as
  melhores práticas
- Apresentar como o `language-dockerfile` pode solucionar esses problemas

## Dockerfiles seguindo as normas
A plataforma de "containerização" **Docker** emergiu recentemente, como uma
forma simples de esconder a complexidade de criar e orquestrar aplicações
isoladas em "containers" dos times de desenvolvimento.

Junto com seu cliente de linha de comando, daemon, registro de imagens e outras
funcionalidades, o **Docker** se utiliza de uma linguagem declarativa para a
definição de imagens para rodar aplicações por meio das **Dockerfiles**. A
sintaxe dessa linguagem é muito simples, começamos declarando qual é a imagem
"base" para nossa construção, usando a diretiva `FROM` e seguimos adicionando
arquivos e executando comandos sobre ela com as diretivas `RUN` e `ADD`.

### Dockerfiles para linguagens interpretadas
Vamos analisar um exemplo de uma **Dockerfile** escrita para criar uma imagem de
um projeto **Node.js**, adicionando seu código-fonte e instalando suas
dependencias.

{% highlight docker %}
FROM node:6
ADD . /app
RUN cd /app && npm install
{% endhighlight %}

Para cada "etapa" ou diretiva nessa definição, o **Docker** criará uma imagem
intemediária que ficará em cache na sua máquina até que suas dependências mudem.
Se escrevermos essa declaração em um arquivo `Dockerfile` no root do nosso
projeto, podemos construir e rodar uma imagem com ele, rodando:

{% highlight bash %}
docker build -t myapp:latest .
docker run -it --rm myapp:latest bash -c 'cd /app && node .'
{% endhighlight %}

Se você é familiar com o Docker e já sabia disso, sabe que esse exemplo não é
idiomático. Isso é porque, toda vez que o código do nosso projeto mudarem,
teremos que reconstruir tudo desde a segunda diretiva, que copia o projeto para
o diretório `/app` da imagem, mesmo se nossas dependencias não mudarem, o que
pode ser proibitivamente demorado.

A prática comum seria tentar instalar as dependencias primeiro, depois copiar o
código do projeto; assim, somente quando as dependencias mudarem, precisaríamos
as re-instalar:

{% highlight docker %}
FROM node:6
ADD ./package.json /app/package.json
RUN cd /app && npm install
ADD . /app
{% endhighlight %}

Também seria uma prática comum, usar as diretivas `CMD` e `WORKDIR`, para que a
imagem em si contivesse qual o comando necessário para subir esse container e
qual seu diretório de trabalho:
{% highlight docker %}
FROM node:6
ADD ./package.json /app/package.json
WORKDIR /app
RUN npm install
ADD . /app
CMD node .
{% endhighlight %}

Com isso, podemos encurtar o script para construir e rodar a imagem para:

{% highlight bash %}
docker build -t myapp:latest .
docker run -it --rm myapp:latest
{% endhighlight %}

Sem a parte `bash -c 'cd ...'` que deixa vazar essa informação para o
"callee"/"end-user" da imagem.

Também poderíamos usar uma imagem menos pesada, como uma versão do **Node.js**
rodando sobre a seca distribução do Linux **Alpine**.

- - -

Como nossa única forma de compartilhar lógica desses arquivos de geração de
imagens é a herança a partir de uma imagem base (a diretiva `FROM`), esse tipo
de "melhor prática" deve ser lembrada de novo e de novo e re-codificada para
cada serviço construído.

Você poderia escrever scripts que compartilha entre **Dockerfiles**, mas isso se
limita de algumas formas. Por exemplo, não há como escrever um script que
encapsulem a ideia de que vou adicionar o manifesto das minhas dependencias,
executar um comando e então adicionar meu projeto, já que um script não tem
acesso às diretivas do Docker como `ADD`, `CMD` etc.

### Dockerfiles para linguagens compiladas
No exemplo acima, olhamos para **JavaScript**, plataforma na qual, para o
servidor, o artefato entregue envolve o código fonte e suas dependencias, em
geral, sem uma etapa de compilação. Em contraste, no caso de uma linguagem como
**Haskell** ou **Go**, o artefato entregue é um binário, gerado a partir do
código-fonte e as dependencias para gerar esse binário e o executar são
diferentes.

Poderíamos criar uma imagem para compilar e executar um projeto Haskell da
seguinte forma:

{% highlight dockerfile %}
FROM haskell:7
ADD ./myapp.cabal /app/myapp.cabal
WORKDIR /app
RUN cabal update
RUN cabal sandbox init
RUN cabal install --only-dep -j
ADD . /app
RUN cabal install
CMD myapp
{% endhighlight %}

Aqui usamos a mesma imagem para compilar o projeto e executar o binário gerado.
Apesar de simples, essa estratégia é lenta e custosa e não é recomendada.

Idealmente gostaríamos de ter duas imagens:

- Uma com todas as dependencias necessárias para gerar o binário que vamos entregar
- Outra apenas com as dependencias necessárias para executar esse binário

### Scripts que geram Dockerfiles e Imagens
Outra abordagem é a de ter um script gerando nossos artefatos e então suas
imagens equivalentes.

A nova ferramenta [`stack`](https://haskellstack.org) do Haskell resolve parte
desse problema oferecendo integração built-in com o Docker para compilar
projetos dentro de um container, compartilhando o cache de compilação com o
resto do sistema.

Podemos escrever um script como:

{% highlight bash %}
stack docker pull
stack build --docker
# ^ Compila o projeto e todas suas dependencias em um container

install_root=`stack path --local-install-root --docker`
# ^ O path para o diretório contendo os arquivos gerados pelo build

cat > Dockerfile <<EOF
# Partimos de uma imagem muito pequena do Alpine Linux
FROM alpine
# Adicionamos as bibliotecas compartilhadas que nosso executável precisa
RUN apk add gmp glib musl musl-dev
# Adicionamos o binário
ADD $install_root /app/
# Registramos o binário como o ponto de entrada com a localização das
# bibliotecas
CMD LD_LIBRARY_PATH=/usr/local/lib:/lib /app/bin/myapp
EOF
# ^ Um Template para criar a imagem com o binário que geramos
{% endhighlight %}

Apesar de que o fato do toolchain de Haskell não produzir binários completamente
estáticos traz um pouco de complexidade, há algumas coisas das quais não
conseguiríamos fugir mesmo em uma linguagem que de fato gerasse um único
artefato executável independente.

Primeiro, precisamos de uma etapa de **construção do artefato** orquestrada com
uma etapa de **construção da imagem**.

Segundo, a definição da **construção imagem** depende da primeira etapa, ou, ao
menos, precisa saber onde encontrar o binário e arquivos de suporte a serem
entregues.

De novo, a abstração de herança por meio imagens bases é insuficiente para
chegar no resultado final de forma modular. O conhecimento codificado nesse
script está isolado e só pode ser re-utilizado se copiado de um lugar para o
outro.

- - -

## Em suma
Nesses dois exemplos encontramos dois problemas com o uso isolado de
**Dockerfiles**:

- Não podemos compartilhar entre mais de uma **Dockerfile** nenhuma lógica de
  contrução de containers, senão scripts
- Não podemos expressar a ideia de imagens separadas para a compilação e
  produção

- - -

## "The Dockerfile Explosion and the need for higher level tools"
Antes de apresentar minha solução para essas questões, devo mencionar que
comecei a pensar seriamente nesse problema depois de assistir a palestra _"The
Dockerfile Explosion and the need for higher level tools"_ da **DockerCon
2016**.

<iframe width="560" height="315" src="https://www.youtube.com/embed/IyuyA8rSBAo" frameborder="0" allowfullscreen></iframe>

Na palestra, [Gareth Rushgrove](https://github.com/garethr), um engenheiro
trabalhando no sistema manejamento de infraestrutura
[Puppet](https://puppet.com/), tenta apontar essa questão da dificuldade de
compartilhar conhecimento de uma receita de construção de uma imagem para outra,
dentro de uma grande organização ou da comunidade como um todo. Olhando para
algumas das alternativas.

### Rocker - Um formato alternativo para Dockerfiles
Outra ferramenta mencionada é o
[`rocker`](http://tech.grammarly.com/blog/posts/Making-Docker-Rock-at-Grammarly.html),
um formato extendido da sintaxe de **Dockerfiles** que permite, entre outras
coisas, suporte a mais de uma diretiva `FROM` em uma única declaração, o que
resolve de certa forma o problema de ter imagens intermediárias entre o
código-fonte e o resultado.

Do post linkado acima:

{% highlight dockerfile %}
# O primeiro FROM descreve a imagem de compilação
FROM google/golang:1.4
ADD . /src
WORKDIR /src
# Gera app.o no diretório /src
RUN CGO_ENABLED=0 go build -a -installsuffix cgo -v -o app.o app.go
# Exporta app.o para o próximo FROm
EXPORT app.o

# O segundo FROM descreve a imagem de runtime
FROM busybox
# Importa app.o da primeira imagem
IMPORT app.o /bin/app
CMD ["/bin/app"]
TAG app:latest
{% endhighlight %}

Ainda assim, não podemos modularizar as instruções de compilação ou construção
do container de runtime. Tal conhecimento deve ser codificado de novo e de
novo. Apesar de que a biblioteca de templates do `Go` é exposta em alguma
medida para o formato, isso não é o suficiente, ao meu ver, para criar
abstrações fáceis de manter e compartilhar.

### Uma EDSL em OCaml
Ele menciona o [`ocaml-dockerfile`](https://github.com/avsm/ocaml-dockerfile),
uma EDSL para o **OCaml** com o mesmo propósito do
[`language-dockerfile`](http://hackage.haskell.org/package/language-dockerfile).

Dois problemas são levantados:

- Podemos fazer qualquer coisa em OCaml e acabar tornando a definição de
  imagens extremamente complicada
- A curva de aprendizado é alta

Uma diferença entre a solução do `language-dockerfile` e esse projeto, é que o
primeiro consegue ler **Dockerfiles** para dentro da EDSL assim como imprimir a
EDSL em **Dockerfiles**, sem nenhuma perda de informação, então você não precisa
começar convertendo todas suas **Dockerfiles** para **Haskell**, mas, ao invés
disso, incrementalmente adicionar inteligência às definições (mais sobre isso
abaixo).

- - -

# Haskell ao nosso resgate
O
[`language-dockerfile`](http://hackage.haskell.org/package/language-dockerfile)
é um fork do linter de **Dockerfiles**
[hadolint](https://github.com/lukasmartinelli/hadolint), que expõe uma EDSL em
**Haskell** para contornar esses problemas. Com o bom suporte para facilmente
nomear blocos de código e compartilhar módulos que já temos em **Haskell**,
temos uma linguagem para expressar a construção de imagens que não se limita a
uma sequência de diretivas.

Além disso, já que **Haskell** pode isolar side-effects no sistema de tipos,
explicitamos a diferença entre "templates" e definições de imagens que podem
fazer qualquer tipo de coisa.

## Trabalhando na imagem de Node.js

Vejamos um exemplo base, algo algo que gere a primeira **Dockerfile**
apresentada nesse post. Começamos amarrando o que já escrevemos na EDSL:

{% highlight haskell %}
{-# LANGUAGE QuasiQuotes #-}
import Language.Dockerfile
main = writeFile "./Dockerfile" $ toDockerfileStr $
  [edockerfile|
FROM node:6
ADD ./package.json /app/package.json
WORKDIR /app
RUN npm install
ADD . /app
  |]
{% endhighlight %}

Porque **Haskell** tem suporte a _QuasiQuotes_ (blocos de código escritos em
outras linguagens embedados), não precisamos traduzir nada a princípio.

Nesse exemplo:

- **import Language.Dockerfile** importa a EDSL e alguns helpers
- **writeFile "./Dockerfile"** escreve o resultado de todo o resto do arquivo
  em "./Dockerfile"
- **toDockerfileStr** recebe um bloco da EDSL e o converte para uma `String`
  contendo o resultado gerado
- **edockerfile** nos deixa interpolar dockerfiles em meio a EDSL, mas não
  permite blocos de dockerfiles inválidas; isso resulta em um erro de compilação

- - -

Podemos começar a quebrar essas 5 linhas em algo que seja genérico:
{% highlight haskell %}
nodejsImage = do
    [edockerfile|
FROM node:6
ADD ./package.json /app/package.json
WORKDIR /app
RUN npm install
ADD . /app
CMD node .
    |]
{% endhighlight %}

Então em todos nossos projetos de **Node.js** poderíamos nos resumir a:
{% highlight haskell %}
-- [...] importa a definição acima de alguma forma
import Language.Dockerfile
main = writeFile "./Dockerfile" $ toDockerfileStr nodejsImage
{% endhighlight %}

Poderíamos, também, ter criado algo mais extensível como:
{% highlight haskell %}
nodejsImage extraSteps = do
    from ("node" `tagged` "6")
    extraSteps
    [edockerfile|
ADD ./package.json /app/package.json
WORKDIR /app
RUN npm install
    |]
    add "." "/app"
{% endhighlight %}

Que então poderia ser usado tanto como no exemplo base:
{% highlight haskell %}
-- [...] importa a definição acima de alguma forma
import Language.Dockerfile
main = writeFile "./Dockerfile" $ toDockerfileStr $
    nodejsImage $ return ()
{% endhighlight %}

Quanto com mais instruções, executadas em um momento sensível da definição:
{% highlight haskell %}
-- [...] importa a definição acima de alguma forma
import Language.Dockerfile
main = writeFile "./Dockerfile" $ toDockerfileStr $ 
    nodejsImage $ do
        run "apt-get install imagemagick"
{% endhighlight %}

Cabe notar que essas definições são publicáveis e modulares, assim como qualquer
outro módulo escrito **Haskell**.

## Criando mais diretivas, dessa vez usando o sistema de arquivos no host
Outro exemplo, seria criar um combinador `addGlob`; algo que falta em
`Dockerfiles`, para copiar todos os arquivos que batem um glob para uma imagem:

{% highlight haskell %}
{-# LANGUAGE FlexibleContexts #-}
module DockerGlob where

import           Control.Monad        (forM_)
import           Language.Dockerfile
import qualified System.Directory     as Directory
import qualified System.FilePath      as FilePath
import qualified System.FilePath.Glob as Glob

addGlob pattern destination = do
    fs <- liftIO $ do
        cwd <- Directory.getCurrentDirectory
        fs <- Glob.glob pattern
        return (map (FilePath.makeRelative cwd) fs)
    comment $ "Added by globbing for " ++ pattern
    forM_ fs $ \f ->
        add f destination
{% endhighlight %}

E então usar esse combinador onde preciso:
{% highlight haskell %}
import Language.Dockerfile
import DockerGlob
main = do
    df <- toDockerfileStrIO $ do
        -- ...
        addGlob "_posts/*.md" "/posts"
        -- ...
    writeFile "./Dockerfile" df
{% endhighlight %}

No repositório desse blog, o código acima gera algo como:
{% highlight dockerfile %}
# Added by blogging for _posts/*.md
ADD _posts/2015-12-04-implementando-fibonacci-em-haskell.md /posts
ADD _posts/2015-12-08-aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.md /posts
# ...
{% endhighlight %}

Note que nesse caso, precisamos substituir **toDockerfileStr** por
**toDockerfileStrIO**. Isso é porque a EDSL por si só não o restringe a ter uma
definição pura do seu `Dockerfile`, mas prove essas duas formas de se certificar
do tipo de efeito que pode ser feito na execução de blocos.

Se **toDockerfileStr** é usado, o bloco que segue não pode ser mais do que um
template; o sistema de tipos nos assegura que ele é um bloco puro e sempre se
compartará da mesma forma independente do contexto em que for chamado. Usando
**toDockerfileStrIO**, nós executamos a EDSL no tipo **IO** do **Haskell**, o que
nos deixa executar ações arbitrárias.

Um framework em um nível mais alto, poderia restringir o tipo de ação acessível
para um gerador de imagens.

## Controlando o Docker a partir do Haskell (Scriptting)
Também podemos facilmente escrever helpers como:
{% highlight haskell %}
import           Crypto.Hash                         (hash, MD5)
import qualified Data.ByteString.Char8 as ByteString (pack)
import           System.Process                      (callCommand)

dockerBuild img = do
    let imgStr = toDockerfileStr img
        imgSum = show (hash (ByteString.pack imgStr) :: Digest MD5)
        imgTag = "haskell-language-dockerfile-autogen:" ++ imgSum
        imgFp = "autogen-" ++ imgSum ++ ".dockerfile"
    writeFile imgFp imgStr
    callCommand ("docker build --tag " ++ imgTag ++ " -f " ++ imgFp ++ " .")
    return imgTag
{% endhighlight %}

Aqui, usamos os pacotes `cryptohash`, `process` e `bytestring`, para criar uma
imagem dada uma Dockerfile e retornar seu tag automaticamente gerado.

Seu uso seria:
{% highlight haskell %}
main = do
    image1Tag <- dockerBuild $ do
        from "node"
        -- ...
    -- ...
    image2Tag <- dockerBuild $ do
        from "haskell"
        -- ...
    -- ...
{% endhighlight %}

Sem muito mais esforço poderíamos no meio de qualquer código **Haskell**, criar uma
imagem (se ainda não existir) e a executar.


## Indo mais longe
Até onde eu sei, o
[`language-dockerfile`](http://hackage.haskell.org/package/language-dockerfile)
é único nas estratégias de geração de Dockerfiles no sentido de que para
**qualquer** bloco puro rodando no seu tipo interno `EInstructionM`, nós
necessáriamente temos um `Dockerfile` equivalente, e podemos, como demonstrado,
embedar instruções lidas de um `Dockerfile` normal, por meio de um _QuasiQuoter_
em meio à EDSL usando **Haskell**.

Não seria impossível, portanto, transformar essa EDSL em um sistema de plugins
ao invés de um sistema embedado em **Haskell** (apesar de que eu gosto de
**Haskell** :) ).

No lugar de rejeitar diretivas inválidas de cara durante o parsing, ele poderia
simplesmente as ler, para uma estrutura de dados como `UnknownInstruction {
  uiName :: String, uiArgs :: [String] }`.

Uma vez lido, nós poderíamos pesquisar em um mapa de funções
`Map String ([String] -> EInstructionTM IO ()` e executar essa substituição.

## Conclusão
O pacote
[`language-dockerfile`](http://hackage.haskell.org/package/language-dockerfile)
é uma adição simples para pessoas já familiares com **Haskell**, que pode fazer
do manejamento de construção de imagens mais modular e re-utilizável.

Ele é implementado usando a biblioteca `free` e a EDSL é completamente
desacoplada do fato de que gera texto (com outro interpretador, poderia criar
imagens, por exemplo), e permite a extensão incremental das declarações já
existentes por meio de um _QuasiQuoter_.

Todo o código está sendo distribuido sob a licença GPLv3 no GitHub em:

[https://github.com/beijaflor-io/haskell-language-dockerfile](https://github.com/beijaflor-io/haskell-language-dockerfile)

E no Hackage em:

[https://hackage.haskell.org/package/language-dockerfile](https://hackage.haskell.org/package/language-dockerfile)

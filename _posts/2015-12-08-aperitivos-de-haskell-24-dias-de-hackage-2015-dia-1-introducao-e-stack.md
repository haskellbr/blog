---
layout: post
title: Aperitivos de Haskell - 24 dias de Hackage, 2015 - dia 1 - Introdução e Stack
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original](http://conscientiousprogrammer.com/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/)_

Nessa série de posts traduzidos "24 dias de Hackage, 2015", caminhamos por
uma série de módulos publicados no ecossistema da linguagem de programação
Haskell. Pacotes bonitos, surpreendentes ou simplesmente muito úteis para o uso
de Haskell na vida real.

<!-- more -->

## Índice de toda a série
* **Dia 1:** [Introdução e Stack](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html)
* **Dia 2:** [Expressões regulares com `pcre-heavy`; scripts em Haskell usando o Stack](/2015/12/09/24-dias-de-hackage-2015-dia-2-expressoes-regulares-com-pcre-heavy-scripts-standalone-usando-stack.html)
* **Dia 3:** [`HSpec`; A importância de testes](/2015/12/10/24-dias-de-hackage-2015-dia-3-hspec-a-importancia-de-testes.html)
* **Dia 4:** [`wreq`: Programação de clientes Web; com notas sobre lens e a sintaxe de operadores](/2015/12/12/24-dias-de-hackage-2015-dia-4-wreq-programacao-de-clientes-web-com-notas-sobre-lens-e-a-sintaxe-de-operadores.html)
* **Dia 5:** [`should-not-typecheck`: Fazendo Haskell quase dinamicamente tipado com tipos de erros "deferred"](/2015/12/13/24-dias-de-hackage-2015-dia-5-should-not-typecheck-fazendo-haskell-quase-dinamicamente-tipado-com-deferred-type-errors.html)
* **Dia 6:** [Encontrando utilitários com Hoogle e Hayoo: `MissingH`, `extra`](/2015/12/13/24-dias-de-hackage-2015-dia-6-encontrando-utilitarios-com-hoogle-e-hayoo-missingh-extra.html)
* **Dia 7:** [`semigroups`; lista `NonEmpty` e um caso de estudo de tipos e testes](/2015/12/15/24-dias-de-hackage-2015-dia-7-semigroups-lista-nonempty-e-um-caso-de-estudo-de-tipos-e-testes.html)
* **Dia 8:** [`multiset`; eu queria que isso estivesse no pacote `containers` padrão](/2015/12/16/24-dias-de-hackage-2015-dia-8-multiset-queria-que-estivesse-na-biblioteca-padrao.html)
* **Dia 9:** [Template Haskell: `here`, `interpolate` e `file-embed`](/2015/12/16/24-dias-de-hackage-2015-dia-9-pontos-interessantes-do-template-haskell.html)
* **Dia 10:** (não traduzido) s-cargot: Usando a sintaxe de s-expressions
* **Dia 11:** (não traduzido) monad-loops: Evitando escrever funções recursivas por meio da refatoração
* **Dia 12:** (não traduzido) json-autotype: Inferindo tipos a partir de dados
* **Dia 13:** (não traduzido) hint: Avaliação em runtime para Haskell
* **Dia 14:** (não traduzido) Earley: Uma biblioteca de parsers promisora para Haskell
* **Dia 15:** (não traduzido) IOSpec: Testando IO e algumas dicas para o QuickCheck
* **Dia 16:** (não traduzido) safe; o que é segurança mesmo?
* **Dia 17:**
* **Dia 18:**
* **Dia 19:**
* **Dia 20:**
* **Dia 21:**
* **Dia 22:**
* **Dia 23:**
* **Dia 24:**

- - -


# Dia 1
Alguns dias atrás, eu me deparei com [um tweet do Ollie Charles](https://twitter.com/acid2/status/669882628695281669)
sobre como ele não teve tempo de fazer seus posts de usuais em dezembro "24
dias de..." esse ano.  Fiquei triste porque eu aprendi muito [lendo eles](https://ocharles.org.uk/blog/).
Tanto em 2012 quanto em 2013, ele escreveu "24 dias de Hackage", pequenos e
úteis posts que mostravam como usar alguns pacotes selecionados que você pode
obter pelo arquivo comunitário **Hackage** e, em 2014, ele cobriu extensões da
linguagem no GHC.

Com alguma trepidação, decidi que eu faria uma série "24 dias de Hackage" para
terminar o ano dividindo uma seleção do enorme número de pacotes de Haskell que
acho úteis. Eu achei que seria particularmente apropriado fazer isso dado que
2015 foi o ano em que eu migrei para **usar Haskell como minha linguagem
principal** na maior parte do meu trabalho recente e projetos pessoais e,
assim, esse foi um ano de descobertas consideráveis para mim.

## Todo o código
Todo o código para a série estará [nesse repositório do GitHub](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).

- - -

## Meu critério de seleção

Como escolher o quê cobrir? Eu gosto do que Ollie escreveu em seu
[post inaugural de 2012](https://ocharles.org.uk/blog/posts/2012-12-01-24-days-of-hackage.html):

> Esse será um tour turbulento de alguns módulos que eu uso em uma base (quase)
> diária, incluindo módulos que me inspiraram, módulos que mudam a forma como
> penso sobre código, e alguns módulos que são tão incríveis que eu não sou
> inteligente o suficiente para os usar!

Minha intenção: algumas coisas que vou cobrir já são populares e conhecidas,
algumas podem só ser utilitários pequenos, algumas podem ser completamente
obscuras, mas o tema fundamental será "coisas que eu uso e sobre as quais posso
dizer algo útil brevemente".

## Stack

Foi muito fácil escolher o tópico para o primeiro dia:
[Stack](http://haskellstack.org/), a maior nova coisa para Haskell em 2015 fora
o [GHC](https://www.haskell.org/ghc/) 7.10.

**Stack mudou minha vida (com Haskell).**

Stack é um _game changer_ para a comunidade Haskell. É uma solução _all-in-one_
para criar projetos Haskell, manejar dependências, compilar e mais. Desde que o
Stack foi publicado, eu tenho lentamente migrado projetos antigos para o usa-lo, e
eu uso o Stack para todos os novos projetos, incluindo o [repositório para essa série](https://github.com/FranklinChen/twenty-four-days2015-of-hackage)

Eu não vou dar um tutorial completo no Stack aqui hoje, só um gostinho, e você
pode ler a [documentação oficial](http://haskellstack.org/) para detalhes, mas o
que eu gostaria de enfatizar é que o Stack é útil não só para desenvolvedores
experientes, mas especialmente para recem-chegados, então parte do artigo de
hoje é destinado especificamente para recem-chegados (ou aqueles que tentaram
Haskell e estão interessados em um recomeço com ferramentas melhores).

## "Como eu começo com Haskell"?

Quando eu comecei o [Pittsburgh Haskell](http://pittsburghhaskell.org/) em
Fevereiro desse ano (2015), eu encontrei uma dificuldade enorme: ajudar
recem-chegados a Haskell a começar. Eu criei uma sessão de workshop introdutória,
mas uma quantidade enorme de pessoas se desencorajaram pela minha melhor
tentativa de criar um agora [obsoleto guia de instalação para Haskell](https://github.com/pittsburgh-haskell/haskell-installation)
que funcionaria no OSX, Windows e Linux, e as pessoas tiveram problemas
instalando as ferramentas básicas, e problemas de versão se já tivessem uma
versão antiga do GHC instalada. Muito tempo foi desperdiçado em tentar ajudar as
pessoas com a instalação.

Pittsburgh Haskell deixou de estar ativo em Abril, já que fiquei ocupado
com muitas outras coisas e não havia momento no tempo para o manter andando, mas
eu acredito que um problema grande em tentar criar uma nova comunidade de
Haskell local foi a irritação com ferramentas e _setup._

**Stack resolve esse problema.** Se eu desse um workshop introdutório a Haskell
de novo, eu definitivamente usaria o Stack.

## Um exemplo de começar a usar o Stack usando um template customizado

Se você ainda não usa o Stack, [baixe ele](http://docs.haskellstack.org/en/stable/README.html#how-to-install).

O Web site do Stack já tem a documentação de como começar a usar o Stack usando
o _template_ _default_. Aqui, eu quero promover a ideia de usar e dividir
templates customizados. Isso não é bem documentado, mas eu acho que vai ser mais
e mais importante para recem-chegados, e claro que também é útil para qualquer
um de nós que sempre criamos o mesmo _boilerplate_ para configurar um novo
projeto.

### Usando um template oficial
Eu criei um template customizado chamado `franklinchen` que é parte do
repositório oficial [`stack-templates`](https://github.com/commercialhaskell/stack-templates).

Se você executar

{% highlight console %}
$ stack new stack-template-demo franklinchen
{% endhighlight %}

Prompts aparecerão pedindo a informação para criar um novo projeto chamado
`stack-template-demo`.

### Usando seu próprio template local

Note que o template especificado *não* tem que estar no repositório oficial
`stack-templates`. Ele também pode estar no seu _file system_ local. Por
exemplo, antes de eu enviar meu template para `stack-templates`, eu costumava
rodar:

{% highlight console %}
$ stack new stack-template-demo /path/on/my/computer/to/franklinchen.hsfiles
{% endhighlight %}

onde `franklinchen.hsfiles` é o meu template (leia abaixo sobre como criar seu
próprio template).

(Eu botei uma instância do projeto gerado [no GitHub](https://github.com/FranklinChen/stack-template-demo), caso você queira olhar na estrutura sem ter que instalar e rodar o Stack agora.)

### Começando com o novo projeto gerado

Entre no diretório do projeto:

{% highlight console %}
$ cd stack-template-demo
{% endhighlight %}

### O Stack baixo o GHC para você

Rode

{% highlight console %}
$ stack setup
{% endhighlight %}

Se você ainda não tem uma versão apropriada do GHC instalada, o Stack vai
*automaticamente* baixar e instalar ela pra você, em uma área do diretório de
configuração do Stack `~/.stack`. A coisa importante a se notar é que quando
usando o Stack, muitas versões do GHC podem coexistir como desejado para muitas
configurações de compilação diferentes e setups. Essa _feature_ é muito
importante, porque nem todo mundo usa a mesma versão do GHC e você pode compilar
seu projeto usando muitas versões do compilador facilmente.

Essa _feature_ de download automático é particularmente útil para recem-chegados
que não precisam ficar mexendo com uma instalação global separada que requira
privilégios especiais.

No _output_, se o Stack precisar baixar alguma coisa:

{% highlight console %}
Preparing to install GHC to an isolated location.
This will not interfere with any system-level installation.
Downloaded ghc-7.10.2.
Installed GHC.
stack will use a locally installed GHC
For more information on paths, see 'stack path' and 'stack exec env'
To use this GHC and packages outside of a project, consider using:
stack ghc, stack ghci, stack runghc, or stack exec
{% endhighlight %}

### Executando o REPL GHCi

A coisa mais importante para um recem-chegado a Haskell é começar com o REPL
GHCi, então vamos fazer isso agora. Fazer isso no contexto de um projeto
carregando seus módulos é simples com o Stack.

Rode:

{% highlight console %}
$ stack ghci
{% endhighlight %}

Note que **somente da primeira vez** que você fizer isso (ou outros comandos que
precisem de dependências), Stack pode demorar um pouco para baixar e compilar
elas. As dependências acabam instaladas e botadas em um _cache_ para que *outros
projetos* no futuro possam as usar de novo. Essa é uma vantagem enorme do Stack
versus os dias antes do Stack, onde nós sempre tínhamos o problema de re-baixar
e re-compilar as mesmas bibliotecas para projetos diferentes; isso era um
desperdício enorme de tempo e espaço! Stack decide inteligentemente o que pode
ou não ser dividido consistentemente.

Stack executa um REPL GHCi com nossos módulos carregados:

{% highlight console %}
Ok, modules loaded: Lib, Main.
{% endhighlight %}

In `src/Lib.hs` no projeto de exemplo, nós temos um módulo besta ilustrando um
pouco os comentários de documentação [Haddock](https://www.haskell.org/haddock/):

{% highlight haskell %}
-- | A library to do stuff.
module Lib
    (
      ourAdd
    ) where

-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y
{% endhighlight %}

Nós podemos acessar o módulo `Lib` do REPL:

{% highlight console %}
*Main> ourAdd 2 3
5
*Main> Lib.ourAdd 4 5
9
{% endhighlight %}

Nós também podemos acessar `Main`, que é definido em `app/Main.hs`:

{% highlight haskell %}
module Main where

import Lib (ourAdd)

import Text.Printf (printf)

main :: IO ()
main = printf "2 + 3 = %d\n" (ourAdd 2 3)
{% endhighlight %}

{% highlight console %}
*Main> main
2 + 3 = 5
{% endhighlight %}

### Compilando e rodando o projeto

Você poderia ter compilado o projeto explicitamente primeiro, antes de executar
o REPL. Na prática em projetos reais, eu começo compilando um projeto para
compilar suas dependências, antes de usar o GHCi, apesar de que o que fiz acima
faz isso para você também:

{% highlight console %}
$ stack build
{% endhighlight %}

Porque defini um binário executável nativo chamado `stack-template-demo` no
nosso arquivo Cabal `stack-template-demo.cabal`, podemos rodar o executável:

{% highlight console %}
$ stack exec stack-template-demo
2 + 3 = 5
{% endhighlight %}

Eu incluí testes unitários para `Lib` em `test/LibSpec.hs` que podem ser rodados
com:

{% highlight console %}
$ stack test
Lib
  Lib
    works
    ourAdd is commutative

Finished in 0.0007 seconds
2 examples, 0 failures
{% endhighlight %}

### Instalando a biblioteca e executável

Você pode agora instalar a biblioteca e executável para seu próprio uso mais
tarde:

{% highlight console %}
$ stack install
...
...
Copying from /Users/chen/stack-template-demo/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/bin/stack-template-demo to /Users/chen/.local/bin/stack-template-demo

Copied executables to /Users/chen/.local/bin:
- stack-template-demo
{% endhighlight %}

Por exemplo (já que `~/.local/bin` está no meu `PATH`):

{% highlight console %}
$ stack-template-demo
2 + 3 = 5
{% endhighlight %}

### Sua própria configuração para templates do Stack
Ao usar templates do Stack, é útil adicionar sua informação para a configuração
global, para que ela seja inserida automaticamente quando você gerar projetos
novos. A documentação para a configuração está [aqui](http://docs.haskellstack.org/en/stable/yaml_configuration.html).
Crie um arquivo em `~/.stack/config.yaml`. O meu tem:

{% highlight yaml %}
templates:
  params:
    author-email: "franklinchen@franklinchen.com"
    author-name: "Franklin Chen"
    category: test
    copyright: "2015"
    github-username: "FranklinChen"
{% endhighlight %}

### Outras coisas brilhantes

Eu tento usar o [Travis CI](https://travis-ci.org/) para qualquer código que
eu publique, então meu template gera um arquivo `.travis.yml` que usa o Stack.
Eu comecei a migrar meus antigos _setups_ do Travis baseados em [`multi-ghc-travis`](https://github.com/hvr/multi-ghc-travis)
para usar o Stack.

## Criando um template de projeto customizado do Stack

Foi surpreendente para mim que criar um template customizado não é coberto pela
documentação principal do Stack. Ao invés disso, eu encontrei como o fazer no
repositório [`stack-templates`](https://github.com/commercialhaskell/stack-templates).

O método de criar templates customizados é um pouco rudimentar, envolvendo criar
um único arquivo com diretivas embedadas para indicar os arquivos e estrutura de
diretórios criada, mas é um começo.

## Conclusão

Para o dia 1 do meu "24 days of Hackage", eu brevemente introduzi como usar o
Stack, a ferramenta de Haskell que estou usando para compilar e rodar todos os
exemplos nessa série de artigos.

Em breve: um pouco de código real!

- - -

### Nota do tradutor
Essa foi uma tradução grossa do primeiro post da série do Franklin Chen. Estou
atrasado com seus posts em 8 dias. Poderia só traduzir todos os posts agora e os
publicar de uma vez, mas acho que vale mais a pena publicar um post por dia a
partir de hoje.

Assim, se você quer acompanhar a série enquanto está fresca, não se esqueça de
visitar o [blog do Franklin](http://conscientiousprogrammer.com/). Esse post
será atualizado diáriamente com links para as próximas traduções.

Não vou ter tempo de revisar muito o texto, então se você encontrar um _typo_,
seja gentil e dê um toque. O maior intuito de traduzir essa série é ter os
artigos traduzidos para a posterioridade. Quando o final de ano chegar, teremos
24 artigos muito relevantes (obrigado Franklin!), em português.

Se você quer ajudar com esse tipo de coisa, agora é a hora. Entre no
[Slack](http://haskellbr.com/slack/) ou no
[IRC](http://irc.lc/freenode/haskell-br) da [HaskellBR](http://haskellbr.com/) e
contribua. Esse blog e outros projetos associados estão na
[organização `haskellbr` no GitHub](https://github.com/haskellbr) e em
[haskellbr.com/git](http://haskellbr.com/git).

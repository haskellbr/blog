---
layout: post
title: "24 dias de Hackage, 2015 - dia 5 - Encontrando utilitários com Hoogle e Hayoo: MissingH, extra"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
---

_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original](http://conscientiousprogrammer.com//blog/2015/12/05/24-days-of-hackage-2015-day-5-should-not-typecheck-making-haskell-sort-of-dynamically-typed-with-deferred-type-errors/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

# Dia 6
Nunca será o caso que tudo que todos consideram "útil" vai estar na "biblioteca
padrão" de qualquer ecossistema de uma linguagem. Uma das funcionalidades mais
legais do ecossistema do Haskell (que impressiona todos os não-Haskellers a
quem eu a mostro) é a habilidade de pesquisar por funções pela sua assinatura
de tipo, usando o
[`Hoogle`](http://hoogle.haskell.org/)
ou o
[`Hayoo`](http://hayoo.fh-wedel.de/examples).
Você também pode usar outros critérios, como nomes; isso pode ser útil se você
tiver um chute do nome de alguma função que precise.

Parecem haver duas filosofias para bibliotecas de utilitários:

* Re-use é ótimo, vamos fazer isso
* Todas as dependencias são um problema em potencial, então vamos reinventar ou
  copiar e colar, ao invés de usar algo que pode não ter qualidade ou não ser
  mantido

Eu tendo a preferir re-uso, mas já copiei (e modifiquei) trechos que precisava,
só porque não queria o resto de uma biblioteca enorme que depende de mais
outras coisas que não preciso. Eu acho que esse é um problema de
granularidade. Já que temos a Web hoje, muitas pessoas propuseram a ideia de
que o conceito de uma "biblioteca" deveria virar obsoleto dando lugar a
"micro-bibliotecas", talvez até no nível de um único indentificador exportado,
mas o tópico está fora do escopo desse artigo. (Para uma dessas ideias, veja o
["The internet of code"](http://www.haskellforall.com/2015/05/the-internet-of-code.html) do Gabriel Gonzalez).

_[N.T. A HaskellBR pode contribuir com isso! Ajude em [haskellbr/missingh](https://github.com/haskellbr/missingh#missingh-haskellbr-fork)]_

A situação fica complicada pelo fato de que frequentemente, podemos reinventar
um monte de coisas com só algumas linhas de Haskell, então porque se importar
em procurar pela implementação de outros no primeiro lugar?

De qualquer forma, vamos assumir que o propósito desse artigo é que você está
interessado em encontrar e usar bibliotecas de utilitários. Eu vou mostrar como
encontrar algumas funções de exemplo e chegar em duas bibliotecas de
utilitários que uso, espertamente e informativamente chamadas de
[`MissingH`](http://hackage.haskell.org/package/MissingH) e
[`extra`](http://hackage.haskell.org/package/extra).

- - -

## Um exemplo de listas/strings
Algum tempo atrás estava manipulando strings (eu tinha uma `String`, não um
`Text` ou uma `ByteString`) e precisava substituir todas as ocorrências de uma
substring em um path com outra substring. Por exemplo, como um teste do HSpec
para uma função hipotética chamada `replace`:

{% highlight haskell %}
  it "replaces all substrings within a string" $ do
    replace "abc" "d" "123abc123abc" `shouldBe` "123d123d"
{% endhighlight %}

Claro, não seria difícil escrever o código para fazer isso, mas porque não
procurar por algo pronto que eu possa usar?

Por qual assinatura deveríamos procurar? Talvez:

{% highlight haskell %}
String -> String -> String -> String
{% endhighlight %}

Ou seja (com algo próximo de sua documentação Haddock)

{% highlight haskell %}
String     -- ^ a substring para substituir
-> String  -- ^ um substituto da substring
-> String  -- ^ string original
-> String  -- ^ string resultado
{% endhighlight %}

Ok, vamos tentar essa assinatura como
[uma busca no Hayoo](http://hayoo.fh-wedel.de/?query=String+-%3E+String+-%3E+String+-%3E+String).
Hmm, os resultados não são promissores. No topo está alguma coisa extranha e
não documentada sobre regexes, que provavelmente não é o que queremos.

## Uma técnica de busca importante: assuma o menor número de coisas possível

Provavelmente a dica mais importante para conseguir resultados bons para uma
busca a partir de um tipo é fazer o tipo o quão genérico quanto possível:
*quanto mais variáveis de tipo, o melhor*, e também só use _constraints_ de
 _type-class_ que você precisar de fato. A operação que nós queremos na verdade
 não é restrita a strings. Na verdade, é uma operação sobre listas. Então o
 tipo que nós queremos é:

{% highlight haskell %}
Eq a => [a] -> [a] -> [a] -> [a]
{% endhighlight %}

Essa função utilitária assume o menor número coisas necessárias para fazer o
trabalho para `String`s, já que `String` é só um nome para `[Char]` e `Char` é
uma instância da _type-class_ `Eq`. Para o propósito de substituir sub-listas,
nós não nos importamos se comparamos characteres: só importa que qualquer que
seja o tipo dos elementos nas listas e sub-listas possa ser comparado por
igualdade.

A
[busca no Hayoo](http://hayoo.fh-wedel.de/?query=Eq+a+%3D%3E+[a]+-%3E+[a]+-%3E+[a]+-%3E+[a])
imediatamente retorna resultados mais promissores do que com `String`, de
pacores como `utility-ht`, `MissingH` e `extra`. O `pandoc` também apareceu,
mas ele é uma
[ferramenta enorme de processamento de texto](http://pandoc.org/),
não uma biblioteca que eu incluiria só por uma função utilitária pequena!
(Note que `pandoc` foi
coberto em um
[dia de Hackage de 2013](https://ocharles.org.uk/blog/guest-posts/2013-12-12-24-days-of-hackage-pandoc.html).

A
[busca no Hoogle em `hoogle.haskell.org`](http://hoogle.haskell.org/?hoogle=Eq+a+%3D%3E+[a]+-%3E+[a]+-%3E+[a]+-%3E+[a]&scope=set%3Astackage)
funciona bem também.
(Note que a busca do Hoogle no site antigo
[dá resultados ruins](https://www.haskell.org/hoogle/?hoogle=Eq+a+%3D%3E+[a]+-%3E+[a]+-%3E+[a]+-%3E+[a]).)

## Modificando nossos testes para checar a função genérica

Eu passei brevemente por refatorar testes do HSpec no
[dia 3](/2015/12/10/24-dias-de-hackage-2015-dia-3-hspec-a-importancia-de-testes.html).
Aqui está como testar muitas implementações da mesma função (vamos ir com
`MissingH` e `extra`), e também testar `replace` com tipos de input diferentes:
tanto `String` (que não passa de `[Char]`) e `[Int]`:

{% highlight haskell %}
module MissingHExtraExampleSpec where

-- | Do MissingH
import qualified Data.List.Utils as ListUtils

-- | Do extra
import qualified Data.List.Extra as ListExtra

import Test.Hspec (Spec, hspec, describe, it, shouldBe)

-- | Necessário para a descoberta automática
spec :: Spec
spec =
  describe "MissingH and extra" $ do
    describeReplace "MissingH" ListUtils.replace
    describeReplace "extra" ListExtra.replace
{% endhighlight %}

Mas o código não compila! Por que?

{% highlight haskell %}
-- | Isso não compila...
describeReplace
  :: String
  -- ^ A descrição
  -> (Eq a => [a] -> [a] -> [a] -> [a])
  -- ^ A implementação de replace
  -> Spec
describeReplace description replace =
  describe description $ do
    it "replaces all substrings within a string" $ do
      replace "abc" "d" "123abc123abc" `shouldBe` "123d123d"
    it "replaces all int sublists within an int list" $ do
      replace [0 :: Int, 1] [100, 101, 102] [0, 1, 2, 3, 0, 0, 1, 4]
        `shouldBe` [100, 101, 102, 2, 3, 0, 100, 101, 102, 4]
{% endhighlight %}

### Uma nota no uso de _higher-rank types_ para refatorar
O erro é útil se você sabe o que está acontecendo, mas não muito caso
contrário. Sim, nós precisamos de _higher-rank types_.

{% highlight console %}
    Illegal polymorphic or qualified type:
      Eq a => [a] -> [a] -> [a] -> [a]
    Perhaps you intended to use RankNTypes or Rank2Types
    In the type signature for ‘describeReplace’:
      describeReplace :: String
                         -> (Eq a => [a] -> [a] -> [a] -> [a]) -> Spec
{% endhighlight %}

_Higher-rank types_ são uma extensão ao GHC discutida em um
[Dia de Extensões do GHC 2014](https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html). _Higher-rank
types_ são muito úteis e uma funcionalidade faltando nos sistemas de tipos da
maior parte das outras linguagens.

Resumidamente, a função `replace` que queremos tem tipo:

{% highlight haskell %}
forall a. Eq a => [a] -> [a] -> [a] -> [a]
{% endhighlight %}

aqui nós quantificamos a variável de tipo `a` para que o _constraint_ `Eq` se
aplique dentro do seu escopo. Leia o tipo como "para todos os tipos `a` onde
`a` é um membro da _type-class_ `Eq`, `[a] -> [a] -> [a] -> [a]`". Haskell
normal sem a extensão, não permite usar esse tipo como um parâmetro para outra
função, porque ele não tem um `forall` explícito e insere um `forall` para
você, implicitamente, no escopo do topo da assinatura, o que é o escopo
incorreto para o que queremos.

Então precisamos adicionar a diretiva e mudar o tipo do parâmetro `"A
implementação de replace"` para ter quantificação explícita e está tudo ok:

{% highlight haskell %}
{-# LANGUAGE RankNTypes #-}

describeReplace
  :: String  -- ^ description
  -> (forall a. Eq a => [a] -> [a] -> [a] -> [a])  -- ^ replace
  -> Spec
{% endhighlight %}

### Uma nota sobre quantificação implícita em Haskell e linguagens relacionadas
Eu gostaria que a quantificação de variáveis de tipo fosse explíta em Haskell,
que anotações `forall` fossem necessárias, como o
[PureScript](http://www.purescript.org/)
[faz](https://github.com/purescript/purescript/wiki/Differences-from-Haskell),
porque entender quantificação de variáveis de tipo é importante para entender
completamente o que acontece a nível dos tipos em linguagens como ML e Haskell.

Por exemplo, esse um bom artigo sobre
[como entender a restrição de valores e a restrição de monomorfismo](http://jozefg.bitbucket.org/posts/2015-03-27-unsafe.html),
que pode ser desafiador se você não tem um modelo mental do que acontece nos
bastidores.

## O prazer de explorar bibliotecas

Uma coisa que pode acontecer se você encontrar uma função utilitária útil, é
que você pode explorar o módulo que a contem ou a biblioteca inteira, só
procurando por mais coisas que podem ser úteis no futuro. Por exemplo, eu acho
a biblioteca do Neil Mitchell `extra` agradável o suficiente (bons nomes, boa
documentação no Hackage) que eu a uso quando posso e recomendo dar uma olhada
nela. O repositório no GitHub do `MissingH` sugere que ela não está mais sendo
mantida, então eu estou tentando evitar seu uso.

No mundo de livros e revistas físicas, eu ainda vou até a livraria e fico
olhando as prateleiras de livros/revistas/DVDs novos e acabo olhando na
prateleira de algum item que eu tenha encontrado para ver se há alguma outra
coisa relacionada que eu possa querer conferir.

## Cavando mais fundo

Note que se você está na busca de bibliotecas potencialmente úteis, mas sem
necessidade imediata, você também pode as encontrar só olhando para a lista de
dependências de bibliotecas populares. Eu confesso que as vezes eu me perco
clicando nas dependências de uma página do Hackage. Se você ficar clicando nas
dependências do pacote
[`lens`](https://hackage.haskell.org/package/lens)
de Edward Kmett vai acabar encontrando um número impressinante de bibliotecas
úteis, porque ele é um mestre do universo de re-uso de código.

A analogia com livros ou papel existe aqui, claro, e é olhar nas referências ou
bibliografia de algo para encontrar mais coisas para ler.

## Conclusão
Para o dia 6, dei um exemplo de como buscar por uma função no Hoogle ou no
Hayoo e passei um pouco por como generalizar assinaturas propriamente. Eu
recomendo o uso da biblioteca de utilitários `extra`.

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

---
layout: post
title: "24 dias de Hackage, 2015 - dia 9 - Pontos interessantes do Template Haskell"
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Hanneli T.
translator_url: "https://github.com/hannelita"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original](http://conscientiousprogrammer.com/blog/2015/12/09/24-days-of-hackage-2015-day-9-template-haskell-goodies-here-interpolate-file-embed/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o
[dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

- - -

## Dia 9

Uma observação negativa que fiz no
[dia 8](/2015/12/16/24-dias-de-hackage-2015-dia-8-multiset-queria-que-estivesse-na-biblioteca-padrao.html)
a respeito de Haskell e seu desconfortável suporte a multi-line string e
interpolação trouxe interessantes comentários de que estava enganado e que de
fato haviam boas soluções para isso. Já utilizo uma delas e não a trouxe à tona
porque não queria tirar o foco do post mencionando outras bibliotecas,
especialmente porque elas são implementadas no
[Template Haskell](https://wiki.haskell.org/Template_Haskell), a extensão do
GHC que permite "macros para o Haskell", habilitando metaprogramação em tempo
de compilação (veja
[2014 Day of Hackage article about Template Haskell](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html)). No
[dia 2](/2015/12/09/24-dias-de-hackage-2015-dia-2-expressoes-regulares-com-pcre-heavy-scripts-standalone-usando-stack.html),
apresentei um pacote que utiliza o Template Haskell; vamos continuar com isso
no post de hoje.

<!-- more -->

Esses pacotes requerem apenas que você ative `QuasiQuotes` em módulos que forem
utilizados. Por exemplo, nosso código teria o header:

{% highlight haskell %}
{-# LANGUAGE QuasiQuotes #-}
{% endhighlight %}

## Melhorando as strings com `here`

Tenho usado o [`here`](http://hackage.haskell.org/package/here) para strings e
interpolações. Acesse a [documentação](https://github.com/tmhedberg/here) para
mais detalhes, mas para contextualizar, vejamos alguns exemplos utilizando
informações que já possuímos.

Imports:

{% highlight haskell %}
import Data.String.Here (hereLit, here, hereFile, i)
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
{% endhighlight %}

A seguir temos uma string multi-line quebrada, onde espaços em branco à
esquerda e à direita são removidos, tornando particularmente fácil copiar e
colar blocos de texto entre os colchetes do `here` quasiquoter.

{% highlight haskell %}
    it "makes trimmed multi-line strings prettier" $ do
      -- In this case we want trimming.
      let original = "words 3\n\
                     \I 2\n\
                     \have 2\n\
                     \so 2\n\
                     \for 1\n\
                     \like 1\n\
                     \many 1\n\
                     \to 1"
      let trimmedHereDoc = [here|
words 3
I 2
have 2
so 2
for 1
like 1
many 1
to 1
|]
      trimmedHereDoc `shouldBe` original
{% endhighlight %}

Para mais controle (tipicamente o que faço para exemplos pequenos), use o
`hereLit`:

{% highlight haskell %}
    it "makes literal multi-line strings prettier" $ do
      -- In this case assume we want the trailing newline.
      let original = "words 3\n\
                     \I 2\n\
                     \have 2\n\
                     \so 2\n\
                     \for 1\n\
                     \like 1\n\
                     \many 1\n\
                     \to 1\n"
      let literalHereDoc = [hereLit|words 3
I 2
have 2
so 2
for 1
like 1
many 1
to 1
|]
      literalHereDoc `shouldBe` original
{% endhighlight %}

Mas por que desistir do copy/paste (copiar/colar)? É muito melhor embutir o
arquivo em questão com `hereFile`. Nós usamos nosso próprio arquivo HSpec
`test/Spec.hs` como exemplo:

{% highlight haskell %}
    it "allows file embed" $ do
      [hereFile|test/Spec.hs|] `shouldBe`
        "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}"
{% endhighlight %}

Finalmente, uma interpolação, utilizando `i`. Embora muito conveniente, tenho
algumas ressalvas sobre o uso exagerado desse quasiquoter, pois ele remete ao
estilo "strict typed" de programação, fazendo apenas uso de qualquer coisa que
implemente `Show` e `Typeable`. É fácil escrever, acidentalmente, um código que
compila mas executa a operação errada quando opera de forma implícita. Mesmo
assim, pode ser algo útil:

{% highlight haskell %}
    it "makes interpolation prettier" $ do
      let list = [1 :: Int, 2]
      let num = 42 :: Int
      [i|number: ${num}, stuff: ${map (+1) list}|] `shouldBe`
        "number: 42, stuff: [2,3]"
{% endhighlight %}

## `interpolate`

[`interpolate`](http://hackage.haskell.org/package/interpolate) é um outro
pacote que remete à mesma ideia. Talvez você goste do
[`unindent`](http://hackage.haskell.org/package/interpolate-0.1.0/docs/Data-String-Interpolate-Util.html),
uma funcionalidade que facilita o copy/paste (copiar/colar) de blocos de texto
embutidos em seu código.

## `file-embed`

[`file-embed`](http://hackage.haskell.org/package/file-embed) também é útil,
pois você pode usá-lo para embutir conteúdos de diretórios inteiros. Ele também
tem uma funcionalidade exclusiva de injeção em um executável.

## Conclusão
Você não precisa se contentar com a sintaxe built-in para criar strings no
Haskell. Com bibliotecas que utilizam o Template Haskell, você pode utilizar
representações muito mais elegantes de strings. Dê uma olhada no `here`,
`interpolate` e `file-embed`.

## Todo o código
Todo o código para a série estará
[nesse repositório do GitHub](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).

- - -

### Nota do tradutor
Se você quer ajudar com esse tipo de coisa, agora é a hora. Entre no
[Slack](http://haskellbr.com/slack/) ou no
[IRC](http://irc.lc/freenode/haskell-br) da [HaskellBR](http://haskellbr.com/) e
contribua. Esse blog e outros projetos associados estão na
[organização `haskellbr` no GitHub](https://github.com/haskellbr) e em
[haskellbr.com/git](http://haskellbr.com/git).

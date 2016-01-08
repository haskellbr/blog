---
layout: post
title: Sua linguagem avança sem um comitê? Como QuasiQuotes fazem Haskell avançar rápido, agora.
author: Pedro Yamada
author_url: "https://github.com/yamadapc"
---
Algumas linguagens mudam do dia para noite, quebrando uma quantidade enorme de
código que estava em produção. Outras não mudam há anos. Há uma formas
melhores de introduzir mudanças organicamente, talvez um sistema de macros
sendo a mais popular delas. Gostaria de discutir o quê o Haskell traz para essa
frente, propostas que acho novas e inusitadas. Em seguida, tentarei mostrar um
um QuasiQuoter que soluciona um problema sério na linguagem e como sua criação
impulsionou mudanças no compilador.

<!-- more -->

Não tenho como recomendar o suficiente o
[paper/talk de Guy Steele "Growing a Language"](https://www.cs.virginia.edu/~evans/cs655/readings/steele.pdf),
no qual discute como a extensibilidade por meio da composição de primitivas
básicas é um conceito fundamental em linguagens e o quão bem sucedidas
serão. Não, não se refere somente à linguagens de programação, ainda que esse
seja seu propósito prático. Se nunca seu esse texto, adicione ele para sua
lista de leituras! Eu o guardo com muito carinho porque foi o primeiro ["Paper
of the Week"](https://www.recurse.com/blog/41-introducing-paper-of-the-week)
enquanto estava na _Hacker School_/_Recurse Center_ e é uma leitura sensacional
e muito fácil. Cito esse texto porque acho que ele foi formador para mim e
tem a ver com esse assunto.

# Template Haskell
Começarei com uma discussão sobre o **Template Haskell**. No
[dia 9 da série 24 dias de Hackage](/2015/12/16/24-dias-de-hackage-2015-dia-9-pontos-interessantes-do-template-haskell.html),
Franklin Chen comentou levemente sobre o **Template Haskell** e como pode
melhorar seu código. Vamos descontruir o que é **Template Haskell**.

Como linkado no dia 9, há um
[dia de hackage de 2014 sobre Template Haskell](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html).
Darei minhas próprias observações sobre a extensão.

## {-# LANGUAGE TemplateHaskell #-}
Haskell é uma linguagem estática sem expressões no top-level que não sejam
declarações. Assim, se eu tiver um arquivo `Main.hs` e escrever:

{% highlight haskell %}
putStrLn "Hello World"
{% endhighlight %}

Vou ter um erro de compilação, ao contrário de uma linguagem como Python ou
JavaScript, onde posso escrever:

{% highlight python %}
print "Hello World"
{% endhighlight %}

E ver mágica na tela.

- - -

Ao adicionar `{-# LANGUAGE TemplateHaskell #-}` ao topo de um arquivo `.hs`,
ativamos a extensão. Há mudança no comportamento da linguagem. Com ela, podemos
ter expressões no _top-level_, desde que retornem representações de código.

O que isso quer dizer? Se em **Python** eu posso digitar `print "Hello World"`
e ver `"Hello World"` na tela, em **Haskell** com **TemplateHaskell**, posso
digitar `funcaoQueRetornaCodigo` e ter o código gerado por essa função incluso
no meu módulo.

Além disso, adiciona a ideia de _"splices"_ ao Haskell. Dada uma
`funcaoQueRetornaCodigo` que retorna código e cabe em algum lugar, podemos
escrever `$(funcaoQueRetornaCodigo)` onde gostariámos que o código gerado fosse
incluso e as coisas funcionariam como se espera.

`funcaoQueRetornaCodigo` pode usar qualquer parte ou módulo já escrito em
Haskell, desde que não use código definido no mesmo módulo em que está sendo
incluso. Isso quer dizer que, se temos código que faz parsing de Markdown para
uma estrutura de dados em Haskell, podemos escrever `$(parseMarkdown
"markdown")` e esperar que a estrutura esteja definida nesse ponto como um
literal.

## O que diabos é "código" ou "representações de código" aqui?
Quando falo em "código" ou "representações de código" quero dizer a _AST_: a
árvore de sintaxe abstrata. Isto é, temos uma série de estruturas de dados, que
representam cada pedaço de sintaxe na linguagem. Entre as mais gerais:

- [`Dec` representa declarações](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/Language-Haskell-TH-Syntax.html#t:Dec)
- [`Lit` representa literais](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/Language-Haskell-TH-Syntax.html#t:Lit)
- [`Exp` representa expressões](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/Language-Haskell-TH-Syntax.html#t:Exp)

Em termos práticos ao digitar `$(retornaCodigo)`, esperamos que `retornaCodigo`
tenha tipo:
{% highlight haskell %}
retornaCodigo :: Q Lit
{% endhighlight %}

Ou:
{% highlight haskell %}
retornaCodigo :: Q [Dec]
{% endhighlight %}

Ou:
{% highlight haskell %}
retornaCodigo :: Q Exp
{% endhighlight %}

## `Q`?!?
`retornaCodigo` tem tipo `Q algumaCoisa` onde `algumaCoisa` é a estrutura de
dados que representa o código apropriado para o contexto onde é incluso e `Q` é
um `Monad` que nos deixa facilmente:

- Gerar nomes únicos que não conflitem com nomes já definidos
- Encontrar os nomes aos quais strings se referem
- Manejar estado
- Fazer `IO` (podemos ler um arquivo ou baixar documentação da web para gerar
  código)

## Derivando instâncias tipo `Show` com `TemplateHaskell`
Vamos para um primeiro exemplo prático. Queremos gerar instâncias de uma
_type-class_ `ShowType` ilustrativa, que é definida como:
{% highlight haskell %}
class ShowType a where
    showType :: a -> String
{% endhighlight %}

Dado um valor `x` de tipo `a`, esperamos que `showType x` imprima `a` como uma
`String`.

Uma implementação manual para um ADT mínimo seria:
{% highlight haskell %}
data MeuTipo = MeuTipo

instance ShowType MeuTipo where
    showType = "MeuTipo"
{% endhighlight %}

Para gerar o código queremos gerar uma declaração da instância `ShowType` para
algum tipo arbitrário. Queremos portanto uma função que receba o nome do tipo,
como um `Name` e retorne um `Q [Dec]` contendo a declaração. `Name` é só um
tipo especial para identificadores, como mencionei acima. Poderíamos fazer o
mesmo usando `String`s, mas íamos perder propriedades importantes. O
`TemplateHaskell` adiciona a sintaxe `'MeuTipo` que transforma essa expressão
em seu `Name`. Afinal, normalmente na linguagem, `MeuTipo` em uma expressão se
referiria ao construtor, não ao nome ou o tipo.

Para isso vamos usar a função `showName` do módulo
`Language.Haskell.TH.Syntax`, que define as estruturas que representam código
linkadas acima, entre outras. Essa função tem tipo `Name -> String`. Também
vamos usar o construtor do tipo `Dec` `InstanceD Cxt Type [Dec]` usado para
definir instâncias de _type-classes_. Nesse construtor, `Cxt` é a _type-class_;
usamos seu construtor `ClassP Name [Type]`, onde `Name` é o identificador para
a classe `ShowType` e `[Type]` são os parâmetros de tipo; no nosso caso

{% highlight haskell %}
deriveShowType :: Name -> Q [Dec]
{% endhighlight %}

## QuasiQuotes

## records



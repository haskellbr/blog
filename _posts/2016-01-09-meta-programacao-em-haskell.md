---
layout: post
title: "Como Haskell usa meta-programação para avançar de forma independente"
author: Pedro Tacla Yamada
author_url: "https://github.com/yamadapc"
---
Algumas linguagens mudam do dia para noite, quebrando uma quantidade enorme de
código que estava em produção. Outras demoram anos para avançar. Há uma formas
melhores da comunidade introduzir mudanças organicamente; um sistema de macros
sendo uma forma popular. Gostaria de discutir o quê o Haskell traz para essa
frente, propostas que para mim são novas e inusitadas. Em seguida, tentarei
mostrar um exemplo de meta-programação que soluciona um problema sério na
linguagem e como sua criação impulsionou mudanças no compilador.

<!-- more -->

Não tenho como recomendar o suficiente o
[paper/talk de Guy Steele _"Growing a Language"_](https://www.cs.virginia.edu/~evans/cs655/readings/steele.pdf),
no qual discute como a extensibilidade por meio da composição de primitivas
básicas é um conceito fundamental em linguagens e o quão bem sucedidas
serão. Não, não se refere somente à linguagens de programação, ainda que esse
seja seu propósito prático. Se nunca seu esse texto, adicione ele para sua
lista de leituras! Eu o guardo com muito carinho porque foi o primeiro
["Paper of the Week"](https://www.recurse.com/blog/41-introducing-paper-of-the-week)
enquanto estava na _Hacker School_/_Recurse Center_ e é uma leitura sensacional
e muito acessível.

Em _"Growing a Language"_, Guy Steele discute a ideia de que "linguagem" não se
trata somente de prover formas de expressão, mas também de prover possibilitar
a criação de novas formas de expressão. Uma boa língua deve ser capaz de ser
extendida, construtos devem ser capazes de serem compostos de forma a criar
novos contrutos, novos sentidos e novas formas de composição.

# Template Haskell
Começarei com uma discussão sobre o **Template Haskell**. No
[dia 9 da série 24 dias de Hackage](/2015/12/16/24-dias-de-hackage-2015-dia-9-pontos-interessantes-do-template-haskell.html),
Franklin Chen comentou levemente sobre o **Template Haskell** e como pode
melhorar seu código. E o que é **Template Haskell**?

Como linkado no dia 9, há um
[dia de hackage de 2014 sobre Template Haskell por Oliver Charles](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html).
Ferei meus próprios comentários sobre a extensão.

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

Entre outros elementos da sintaxe, podemos dizer que a linguagem basicamente se
separa em:

Declarações, como:
{% highlight haskell %}
main = putStrLn "Hello World"
data Something = Something String
{% endhighlight %}

E expressões, como:
{% highlight haskell %}
putStrLn "Hello World"
10 + 20
Something "Here"
if this then that else otherThat
{% endhighlight %}

Também temos _patterns_ e tipos, mas como ambos sempre fazem parte de
declarações ou expressões, acho que podemos dizer que (basicamente) isso de
fato é tudo.

Ao adicionar `{-# LANGUAGE TemplateHaskell #-}` ao topo de um arquivo `.hs`,
ativamos a extensão. Adiciona a ideia de _"splices"_ ao Haskell. Dada uma
`funcaoQueRetornaCodigo` que retorna código e cabe em algum lugar, podemos
escrever:
{% highlight haskell %}
$(funcaoQueRetornaCodigo)
{% endhighlight %}

E o código será gerado a tempo de compilação e incluso nesse local.

## O que é "código"?
Antes de tratar do que é "código" no **Template Haskell**, vamos olhar
brevemente para os sistemas de macros em **C** e em seguida em **LISP** (usando
o **Clojure** como exemplo).

_Nota: Não tenho experiência real em **C** ou **Clojure**, se tiver algo a
dizer, não hesite em comentar! Há uma seção de comentários no fim do post._

### Macros em C, search-and-replace
Em **C**, o sistema de macros é efetivamente um "template", no mesmo sentido de
um template de **HTML**, como o que contém o layout desse post. Uma instância de um
macro em **C** é _estupidamente_ inserida no código, sem garantia de que o que
retorna tem o mesmo sentido ao ser compilado que o autor do macro esperava que
tivesse.

Um exemplo tirado do site
[c4learn.com](http://www.c4learn.com/c-programming/c-mistakes/preprocessor-errors/):

{% highlight c %}
#define SQU(x) x*x
{% endhighlight %}

Aqui definimos um macro `SQU` que deve ser expandido para `x*x`. Dessa forma,
se escrevermos:
{% highlight c %}
SQU(2)
{% endhighlight %}

O preprocessador vai expandir isso para:
{% highlight c %}
2*2
{% endhighlight %}

De fato, se você compilar e executar:
{% highlight c %}
#include<stdio.h>
#define SQU(x) x*x
int main() {
  printf("Answer: %d", SQU(3));
  return 0;
}
{% endhighlight %}

Verá:
{% highlight console %}
Answer: 9
{% endhighlight %}

Claro que escolhi esse exemplo porque ele mostra o que quero dizer com
_estupidamente_. Aqui não estamos protegendo as expressões com parênteses,
então, se escrevermos:

{% highlight c %}
#include<stdio.h>
#define SQU(x) x*x
int main() {
  printf("Answer: %d", SQU(2)/SQU(2));
  return 0;
}
{% endhighlight %}

Teremos o inesperado resultado:
{% highlight console %}
Answer: 4
{% endhighlight %}

Isso é porque o macro acima é expandido para:
{% highlight c %}
#include<stdio.h>
#define SQU(x) x*x
int main() {
  printf("Answer: %d", 2*2/2*2);
  return 0;
}
{% endhighlight %}

E `2*2/2*2` é executado como:

- `2*2/2*2` - Reduz `2*2`
- `4/2*2` - Reduz `4/2`
- `2*2` - Reduz `2*2`
- `4`

Outros problemas vão aparecer se tentarmos escrever `SQU(2 + 2)` e assim por
diante, a solução sendo usar parênteses em volta do macro:

{% highlight c %}
#define SQU(x) ((x)*(x))
{% endhighlight %}

### Macros em Clojure, código como dados
Não poderia discutir de meta-programação sem passar, mesmo que
superficialmente, pelo LISP. Em LISP, temos a grande vantagem de que a sintaxe
é quase exclusivamente composta por listas. Para aplicar uma função, escrevemos:

{% highlight clojure %}
(funcao argumento1 argumento2 argumento3)
{% endhighlight %}

Assim, somamos dois números com:
{% highlight clojure %}
(+ 10 20)
{% endhighlight %}

E mesmo as estruturas de controle de fluxos são expressas com listas:
{% highlight clojure %}
(if condicao "caso true!" "caso false!")
{% endhighlight %}

Isso é muito útil, porque podemos facilmente escrever código que manipula
código. Um macro é simplesmente uma função especial que retorna a representação
do código que deve ser expandida para. Um exemplo do site
[braveclojure.com](http://www.braveclojure.com/writing-macros/):

{% highlight clojure %}
(defmacro infix [infixed]
  (list (second infixed) (first infixed) (last infixed)))
{% endhighlight %}

Acima construímos um macro que permite o uso infixo de funções:

{% highlight clojure %}
(infix (10 + 20))
{% endhighlight %}

Simplesmente, ele se expande para a chamada do nosso segundo exemplo:
{% highlight clojure %}
(macroexpand '(infix (10 + 20)))
; => (+ 10 20)
{% endhighlight %}

Temos uma diferença radical do que há em **C**. Ao invés de lidarmos com texto
que será inserido no código, lidamos com a _AST_: a árvore de sintaxe
abstrata. Isso é, temos uma estrutura de dados que representa a sintaxe da
linguagem. No caso do **LISP**, manipular essa estrutura de dados é facilitado
pelo fato da sintaxe ser extremamente simples e da tradução entre estrutura de
dados e código ser direta.

### Template Haskell, manipulação tipada do código
**Haskell** segue na mesma linha do **LISP**. Trabalhamos sobre a _AST_
tentando retornar e manipular estruturas de dados que representem o código, mas
ao contrário do **LISP** que é dinâmico e tem a vantangem de traduzir a
estrutura de dados para o código diretamente, no Haskell, a árvore é tipada.

Como tratamos acima, a linguagem basicamente é composta por duas estruturas de
dados:

- [Declarações representadas por `Dec`](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/Language-Haskell-TH-Syntax.html#t:Dec)
- [Expressões representadas por `Exp`](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/Language-Haskell-TH-Syntax.html#t:Exp)

Também temos
[literais representados por `Lit`](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/Language-Haskell-TH-Syntax.html#t:Lit)
e
[_patterns_ representados por `Pat`](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/Language-Haskell-TH-Syntax.html#t:Pat).

Ao digitar `$(funcaoQueRetornaCodigo)`, o tipo esperado de
`funcaoQueRetornaCodigo` depende do contexto. No top-level, esperamos que tenha
tipo:

{% highlight haskell %}
funcaoQueRetornaCodigo :: Q [Dec]
{% endhighlight %}

`funcaoQueRetornaCodigo` pode usar qualquer parte ou módulo já escrito em
Haskell, desde que não use código definido no mesmo módulo em que está sendo
incluso. Isso quer dizer que, se temos código que faz parsing de Markdown para
uma estrutura de dados em Haskell e somos capazes de fazer dessa estrutura uma
declaração, podemos escrever `$(parseMarkdown "markdown")` e esperar que a
estrutura esteja definida nesse ponto.

## O que é Q?
`retornaCodigo` tem tipo `Q algumaCoisa` onde `algumaCoisa` é uma das
estruturas de dados para código (`Pat`, `Exp`, `Lit`, `[Dec]` etc.). `Q` é um
`Monad` que nos deixa facilmente:

- Gerar nomes únicos que não conflitem com nomes já definidos
- Encontrar os identificadores para a estrutura a qual strings se referem
- Manejar estado
- Fazer `IO` (podemos ler um arquivo ou baixar documentação da web para gerar
  código; isso é inseguro, porque pode fazer a compilação não ser
  determinística, mas é legal que seja possível e tem muitos usos práticos)

## Derivando Show com Template Haskell
Um exemplo prático de **Template Haskell**. Queremos gerar instâncias de uma
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
como um `Name` e retorne um `Q [Dec]` contendo a declaração. `Name` é um tipo
especial para identificadores; é o tipo que o `Monad` `Q` nos retorna quando
pedimos um nome único ou tentamos encontrar o identificador que uma `String` se
refere para. Poderíamos fazer o mesmo usando `String`s, mas iriamos perder um
pouco da tipagem.

O `TemplateHaskell` adiciona a sintaxe `'coisa` que transforma algo em seu
`Name`. Afinal, normalmente na linguagem, se tiver um ADT `MeuTipo`, `MeuTipo`
em uma expressão se referiria ao construtor, não ao nome do construtor.

{% highlight console %}
ghci> :t 'MeuTipo
'MeuTipo :: Name
{% endhighlight %}

Da mesma forma também adiciona a sintaxe `''Coisa` que transforma um **tipo**
em seu `Name`.

{% highlight console %}
ghci> :t ''MeuTipo
'MeuTipo :: Name
{% endhighlight %}

`'MeuTipo` é o `Name` do construtor `MeuTipo` e `''MeuTipo` é o `Name` do tipo
`MeuTipo`.

- - -

Vamos usar a função `showName` do módulo `Language.Haskell.TH.Syntax`, que
define as estruturas que representam código linkadas acima, entre outras. Essa
função tem tipo `Name -> String`. Também vamos usar o construtor do tipo `Dec`
`InstanceD Cxt Type [Dec]` usado para definir instâncias de
_type-classes_. Nesse construtor, `Cxt` é a _type-class_; usamos seu construtor
`ClassP Name [Type]`, onde `Name` é o identificador para a classe `ShowType` e
`[Type]` são os parâmetros de tipo; no nosso caso:

{% highlight haskell %}
deriveShowType :: Name -> Q [Dec]
{% endhighlight %}

{% highlight haskell %}
deriveShowType name = return [ InstanceD undefined undefined undefined
                             ]
{% endhighlight %}


## QuasiQuotes

## records



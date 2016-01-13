---
layout: post
title: "Meta-programação em Haskell - Parte 1 - C, LISP, Template Haskell e QuasiQuotes"
author: Pedro Tacla Yamada
author_url: "https://github.com/yamadapc"
---
Algumas linguagens mudam do dia para noite, quebrando uma quantidade enorme de
código que estava em produção. Outras demoram anos para avançar. Há uma formas
melhores da comunidade introduzir mudanças organicamente; um sistema de macros
sendo uma forma popular. Gostaria de discutir o quê o Haskell traz para essa
frente, propostas que para mim são novas e inusitadas.

Em um segundo post, tentarei mostrar um exemplo de meta-programação que
soluciona um problema sério na linguagem e como sua criação impulsionou
mudanças no compilador.

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
a criação de novas formas de expressão. Uma boa linguagem deve ser capaz de ser
extendida, estruturas devem ser capazes de serem compostos de forma a criar
novos estruturas, novos sentidos e novas formas de composição.

# Template Haskell
Começarei com uma discussão sobre o **Template Haskell**. No
[dia 9 da série 24 dias de Hackage](/2015/12/16/24-dias-de-hackage-2015-dia-9-pontos-interessantes-do-template-haskell.html),
Franklin Chen comentou levemente sobre o **Template Haskell** e como pode
melhorar seu código. E o que é **Template Haskell**?

Como linkado no dia 9, há um
[dia de hackage de 2014 sobre Template Haskell por Oliver Charles](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html).
Farei meus próprios comentários sobre a extensão.

## {-# LANGUAGE TemplateHaskell #-}
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
`retornaCodigo` tem tipo `Q algumaCoisa`, onde `algumaCoisa` é uma das
estruturas de dados para código (`Pat`, `Exp`, `[Dec]` etc.). `Q` é um `Monad`
que nos deixa facilmente:

- Gerar nomes únicos que não conflitem com nomes já definidos
- Encontrar os identificadores para a estrutura a qual strings se referem
- Manejar estado
- Fazer `IO` (podemos ler um arquivo ou baixar documentação da web para gerar
  código; isso é inseguro, porque pode fazer a compilação não ser
  determinística, mas é legal que seja possível e tem muitos usos práticos)

## QuasiQuotes Take 1: Explorando a AST do Haskell
Há uma última coisa que o **Template Haskell** adiciona à linguagem. são as
**QuasiQuotes**. Adiciona a sintaxe `[quoter| conteúdo |]` onde `quoter` recebe
uma `String` e retorna um `Q` de alguma das estruturas de dados para o
código. Vamos entrar mais profundamente nisso, mas agora, sugiro que você abra
uma sessão do GHCi e brinque com os **QuasiQuoters** que são importados por
padrão ao se ativar a extensão.

Primeiro inicializamos o `ghci` e ativamos a extensão:
{% highlight console %}
ghci> :set -XTemplateHaskell
{% endhighlight %}

Imediatamente podemos ver o tipo de uma **QuasiQuotation**:
{% highlight console %}
ghci> :t [| 10 + 20 |]
[| 10 + 20 |] :: Language.Haskell.TH.ExpQ
{% endhighlight %}

Vamos importar o módulo `Language.Haskell.TH` para entender o que é `ExpQ`:
{% highlight console %}
ghci> import Language.Haskell.TH
ghci> :info ExpQ
type ExpQ = Q Exp       -- Defined in ‘Language.Haskell.TH.Lib’
{% endhighlight %}

Para inspecionar o `Exp` contido nesse `Q`, vamos usar a função `runQ`, cuja
assinatura pode ser simplificada para:
{% highlight haskell %}
runQ :: IO => Q a -> IO a
{% endhighlight %}

Então:
{% highlight console %}
ghci> runQ ([e| 10 + 20 |])
InfixE (Just (LitE (IntegerL 10))) (VarE GHC.Num.+) (Just (LitE (IntegerL 20)))
ghci> runQ ([d| data List = List String |])
[DataD [] List_0 [] [NormalC List_1 [(NotStrict,ConT GHC.Base.String)]] []]
{% endhighlight %}

Temos os `QuasiQuoters` built-in:

- `e` para expressões
- `d` para declarações
- `p` para patterns
- `t` para tipos

Quando chamamos `[| 10 + 20 |]` o quasi quoter, em tempo de compilação:

- Recebe `10 + 20` como uma `String`
- _Parseia_ `"10 + 20"` como uma expressão de **Haskell**
- Retorna um `Exp`

Estou mostrando isso porque ainda que no **Template Haskell** nós não tenhamos
a vantagem que se tem no **LISP** de uma árvore de sintaxe abstrata
extremamente simples e direta, temos essa capacidade de interativamente
inspecionar estruturas.

- - -

## Derivando Show com Template Haskell
Um exemplo prático de **Template Haskell**.

Queremos gerar instâncias de uma _type-class_ `ShowType` ilustrativa, que é
definida como:
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

Se escrevermos o que temos até agora no `ghci`:
{% highlight console %}
ghci> class ShowType a where showType :: a -> String
ghci> data MeuTipo = MeuTipo
{% endhighlight %}

Podemos dar uma olhada na estrutura para a instância que gostaríamos de gerar:
{% highlight console %}
ghci> runQ [d| instance ShowType MeuTipo where showType _ = "MeuTipo" |]
{% endhighlight %}

Com highlighting para ser mais fácil de enxergar:
{% highlight Haskell %}
[ InstanceD []
  (AppT (ConT ShowType) (ConT MeuTipo))
  [ FunD showType [ Clause [ WildP ]
                    -- Nosso gerador só precisa mudar essa parte
                    (NormalB (LitE (StringL "MeuTipo")))
                    []
                  ]
  ]
]
{% endhighlight %}

Vamos usar a função `nameBase` do módulo `Language.Haskell.TH.Syntax`, que
define as estruturas que representam código linkadas acima. Essa função tem
tipo `Name -> String`.

Agora é só escrever:

{% highlight haskell %}
deriveShowType :: Name -> Q [Dec]
deriveShowType name = return [ InstanceD []
                               (AppT (ConT ''MyShow) (ConT name))
                               [ FunD 'myShow [ Clause [ WildP ]
                                                (NormalB (LitE
                                                          (StringL
                                                           (nameBase name))))
                                                []
                                              ]
                               ]
                             ]
{% endhighlight %}

### Usando o gerador
Podemos usar o gerador por meio dos splices:
{% highlight haskell %}
{-# LANGUAGE TemplateHaskell #-}
module Main where

-- O Template Haskell não nos deixa usar splices com funções definidas no
-- módulo atual então botamos tudo escrito até agora em um módulo `TH`
import TH

data MyType = MyType

deriveShowType ''MyType

main :: IO ()
main = print (myShow MyType)
{% endhighlight %}

### Refatorando com pattern-matching
Com pattern matching, poderíamos usar os próprios `QuasiQuotes` para a ajudar a
gerar o código:
{% highlight haskell %}
deriveShowType name = do
    -- Criamos a instância que queremos
    ids <- [d| instance MyShow String where myShow _ = "Mock" |]

    -- Desconstruímos a instância
    let [InstanceD ctx (AppT showt (ConT _)) _] = ids
        name' = nameBase name

    -- Aqui há uma coisa importante, podemos acessar as variáveis em escopo
    -- dentro do QuasiQuoter e elas serão interpoladas.
    fds <- [d| myShow _ = name' |]

    -- Reconstruímos a instância substituindo o "Mock"
    return [InstanceD ctx (AppT showt (ConT name)) fds]
{% endhighlight %}

## QuasiQuotes Take 2: O que é um QuasiQuoter?
Um `QuasiQuoter` não passa de um ADT definido como:
{% highlight haskell %}
data QuasiQuoter = QuasiQuoter { quoteExp  :: String -> Q Exp,
                                 quotePat  :: String -> Q Pat,
                                 quoteType :: String -> Q Type,
                                 quoteDec  :: String -> Q [Dec] }
{% endhighlight %}

Assim para ter uma linguagem interpolada no Haskell basta definir essa
estrutura. Sugiro dar uma olhada no
[manual do GHC](https://downloads.haskell.org/~ghc/6.10.1/docs/html/users_guide/template-haskell.html#th-quasiquotation).
[Um dos projetos da **HaskellBR** está usando um **QuasiQuoter** para ler a declaração de um ADT a partir de arquivos markdown e um header YAML-frontmatter](https://github.com/haskellbr/workhs/blob/75e906454b5c46981e1d4fe7a1571dc62880c2a8/src/Workhs.hs#L70-L99)

(Ele usa o meu módulo [`frontmatter`](https://github.com/yamadapc/haskell-frontmatter))

## O que vimos e todo o código
Esse post tentou introduzir a meta-programação em **Haskell**. Em suma, gostaria de
ter apresentado que:

- Em **Haskell** a meta-programação em tempo de compilação se dá por meio da
  manipulação da **AST**
- Temos segurança de tipos ao gerar código
- Podemos explorar a **AST** usando **QuasiQuoters**
- Podemos definir novas estruturas de sintaxe usando **QuasiQuoters**

[Todo o código está disponível no novo repositório `haskellbr/blog-code` no GitHub](https://github.com/haskellbr/blog-code)

No próximo post, vou me aprofundar nesse último ponto e como é um diferencial
para a comunidade.

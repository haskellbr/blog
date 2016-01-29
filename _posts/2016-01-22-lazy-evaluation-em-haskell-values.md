---
layout: post
title: Um pouco sobre Lazy Evaluation em Haskell - Expressões, Valores e Formas
author: H. T., baseado na obra de Takenobu T.
author_url: "https://github.com/hannelita"
---
Um pouco sobre o significado de Lazy Evaluation e conceitos relacionados envolvidos. Baseado na obra de Takenobu T., [Lazy Evaluation Illustrated for Haskell Divers](https://takenobu-hs.github.io/downloads/haskell_lazy_evaluation.pdf) e com comentários e analogias agregadas livremente pela autora do post.

<!-- more -->

- - -

Na [primeira parte](/2016/01/17/lazy-evaluation-em-haskell.html) desse post foi feito um grande alvoroço perante os termos "NF", "HNF e "WHNF", que faziam menção a forma de uma expressão. Mas afinal, por que é interessante compreender tais conceitos?  Dominar tais assuntos pode ajudá-lo(a) a compreender muito mais sobre o comportamento Lazy do Haskell. Vamos então aplicar o que falamos sobre modelos mentais e sobre as estratégias que o Haskell utiliza para colocar em prática seu comportamento Lazy, exemplificando como é o processo de transformar uma expressão em um valor final. Existem várias categorias de expressões na linguagem - expressões, **do**, **let**, **lambda abstraction**, **construtores**, entre outras. Essa categorias gerais nos servem para identificar o propósito geral da expressão.

Podemos ainda analisar uma expressão de acordo com o valor que ela nos propicia. Por exemplo, veja a imagem abaixo:
![Analisando uma expressão](/assets/images/lazy_evaluation_haskell/conteudo_expressao_valor.jpg)

Note que as expressões que iniciarão algum processo de avaliação contém algum tipo de valor, seja um dado ou uma função. Quando citamos de "NF", "HNF" ou "WHNF", estamos falando justamente sobre esses valores! A próxima imagem ilustra sucintamente a distinção entre esses termos; aprofundaremo-nos mais sobre cada um nos parágrafos a seguir.
![Formas dos valores](/assets/images/lazy_evaluation_haskell/formas.jpg)

É como se o valor a ser avaliado possuísse alguns estágios ou formas. A **Weak Head Normal Form** (**WHNF**) é uma forma que possui o top-level (ou cabeça) (__nível superior__, tradução livre) avaliado. Um top-level consiste em um construtor ou em uma expressão lambda. Veja a esquematização:
![WHNF](/assets/images/lazy_evaluation_haskell/whnf.jpg)

Por exemplo, `Just 7` se apresenta na WHNF - Não há nenhuma expressão top-level que precise ser reduzida. Em outras palavras, não há nenhum **redex**(__reductible expression__) no topo. Tal situação já não ocorre com uma condicional do tipo `if x then True else False` - não temos WHNF aqui; há uma top-level redex aqui.

De modo similar temos a HNF, distinguindo-se da WHNF por solicitar como top-level um construtor ou uma expressão lambda sem top-level redex (note que isso não era requerido para a WHNF). Por exemplo, `\x -> Just 7` está na HNF. Já `\x -> abs 7` possui um top-level redex (`abs 7`), não estando assim na HNF.

Após reduzirmos todas as expressões, obtemos uma NF - um valor que não possui nenhuma redex.
![WHNF](/assets/images/lazy_evaluation_haskell/nf.jpg)

`Just 7`está na NF. Não há nenhum redex interno na expressão. `Just (abs 7)` já não está em NF - há uma expressão a ser reduzida internamente.

Foi possível perceber a diferença entre as três formas? Note que o comportamento Lazy tende a evitar o NF - quanto mais redexes, mais preguiçoso. Por isso o WHNF é um grande aliado - aceita como top-level qualquer expressão lambda ou construtor. 

Esperamos que através desses exemplos e ilustrações tenha sido possível compreender a diferença entre cada uma das formas. 

- - -

#### Amarrando as pontas
Se você chegou até aqui, obrigado. Muito desse post tentou ser o mais simples
possível para pessoas que nunca viram Haskell, então não, isso não é como
Haskell se parece em produção. Mesmo assim, é divertido ver o que é possível
com a linguagem e talvez isso tenha te animado para a estudar.

Dúvidas comentários e posts são aceitos no repositório do GitHub:
[https://github.com/haskellbr/blog](https://github.com/haskellbr/blog). No
momento a meta é ter conteúdo publicado semanalmente.


---
layout: post
title: 24 dias de Hackage, 2015 - dia 24 - Conclusão e agradecimentos
author: Franklin Chen
author_url: "http://conscientiousprogrammer.com/"
translator: Pedro Yamada
translator_url: "https://github.com/yamadapc"
excerpt_separator: "<!-- more -->"
---
_Esse é um artigo escrito por Franklin Chen e traduzido para o português.
[Ler original.](http://conscientiousprogrammer.com/blog/2015/12/24/24-days-of-hackage-2015-day-24-conclusion-and-thanks/)_

## Índice de toda a série
O índice de toda a série está no topo do artigo para o [dia 1](/2015/12/08/aperitivos-de-haskell-24-dias-de-hackage-2015-dia-1-introducao-e-stack.html).

## Encontro HaskellBR São Paulo
[Vamos nos encontrar em São Paulo em Janeiro de 2016. Marque sua presença e comente se não puder vir.](http://www.meetup.com/haskellbr-sp/events/227526368/)

[Vote se o encontro terá workshops, um dojo ou palestras.](https://plus.google.com/+DanielYokomizo/posts/bM1C5HRLcR4)

# Dia 24

([Discussão no Reddit](https://www.reddit.com/r/haskell/comments/3y3zpn/24_days_of_hackage_2015_day_24_conclusion_and/))

Whew, esse projeto "24 dias de Hackage, 2015" finalmente acabou!

Eu nunca esperei fazer algo assim e preciso agradecer todos que me encorajaram
enquanto estava envolvido nisso. Gostei da qualidade dos comentários e
correções que recebi assim como os links para bibliotecas relacionadas para
Haskell.

<!-- more -->

Alguns pensamentos finais:

## Minhas metas com o projeto
Minhas metas iniciais para o projeto eram simples ("mostrar algumas coisas"),
mas rapidamente ficaram mais claros na medida em que fui indo e se tornaram
mais explícitos na minha cabeça. Aqui estão alguns:

- Prover código que rode e seja testado, não apenas _snippets_
- Ter certeza de que o código consegue ser compilado, usando o
  [Stack](http://haskellstack.org/) e o
  [Stackage LTS](https://www.stackage.org/lts) como base
- Antecipar atualizações dos artigos no caso de mudanças factuais ou da API das
  bibliotecas
- Prover links de referência úteis para a documentação e exemplos
- Escrever código de exemplo que, apesar de necessariamente limitado, possa
  inspirar curiosidade suficiente para ir mais longe
- Explicar porque eu acho algo interessante, ao invés de só o mostrar
- Tomar nota de melhorias possíveis, especialmente para a usabilidade
- Discutir somente bibliotecas que eu usei, estou começando a usar ou pretendo
  usar no futuro
- Tentar reutilizar tarefas e código de posts antigos para criar a ideia de
  progressão e continuidade

Espero que tenha sucedido em algumas dessas metas. Eu sei que, por causa de
restrições de tempo, tive que pegar alguns atalhos onde, se tivesse tempo,
poderia ter escrito muito mais texto e código muito mais _"realista"_.

## O ecossistema do Haskell continua crescendo
Há uma centena de outras bibliotecas que poderia ter escolhido mencionar pelo
caminho, algumas das quais eu usei e muitas sobre as quais escutei boas coisas
e (ainda) nunca usei.

Um pouco de cobertura útil sobre o que vem acontecendo no Haskell:

- [State of the Haskell ecosystem](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md)
  por Gabriel Gonzalez
- [Reflections on Haskell in 2015](http://www.stephendiehl.com/posts/haskell_2016.html)
  por Stephen Diehl
- [Intermediate Haskell](https://github.com/commercialhaskell/haskelldocumentation/blob/master/outline/intermediate-haskell.md)
  o material em progresso do Commercial Haskell group

## Outras coisas legais que percebi
Há uma série de blog posts sobre resolver o
[Advent of Code](http://adventofcode.com/)
[usando Haskell](http://gelisam.blogspot.ca/2015/12/a-whirlwind-tour-of-haskell-via-advent.html).
Eu deliberadamente não a li, porque quero tentar o Advent of Code por conta
própria em algum momento.

## Sobre escrever
Não espero fazer um "24 dias de Hackage" de novo.

Antes de mais nada, foi exaustivo, em um tempo do ano quando eu acabei tendo
que cancelar a maior parte da minha vida normal em Dezembro para me dedicar ao
projeto! Eu fiz esse projeto por gratitude e amor pela comunidade que me ajudou
com as ferramentas que fazem da minha vida como um programador mais divertida,
produtiva e simples e plenamente *feliz*, mas estou aliviado que a série acabou
e planejo só relaxar para as festas.

Também decidi que em projetos de escrita futuros (sobre qualquer coisa, não só
coisas relacionadas a Haskell), gostaria de mais tempo para tomar um ritmo
diferente e escrever mais *profundamente* sobre tópicos, talvez artigos com
várias partes que pressuponham uma audiência bem definida. Sabia de cara que para
"24 dias" estava provendo "aperitivos" aqui, e sem saber exatamente qual parte
de uma audiência indefinida acharia quais aperitivos úteis. Gostaria de prover
refeições completas direcionadas a audiências bem definidas, com um prefácio
para cada artigo explicando qual o _background_ que eu assumo, com ponteiros
aos pre-requisitos como apropriado, e construir uma rede de informação
interligada.

Tenho uma sugestão genérica: e se a comunidade de Haskell colaborasse para um
projeto "módulo da semana", talvez até com a ideia de incrementalmente criar um
programa de exemplo gigante demonstrando Haskell que incorporasse muitas
bibliotecas e fosse um projeto do mundo real que seria útil para recem-chegados?

Por ora, gostaria de encorajar qualquer pessoa que queria escrever qualquer
coisa, curta ou pequena, a o fazer. É uma oportunidade de aprendizado tremenda.

## Conclusão
Gostaria de agradecer o Ollie de novo por ter feito os "24 dias" no passado,
espalhado que não poderia fazer a série esse ano e me encorajado a o fazer
quando sugeri que talvez poderia contribuir de alguma forma. Não se esqueça,
sua [série "24 dias"](https://ocharles.org.uk/blog/) ainda está no ar e é
valiosa como referência.

Além disso, fiquei feliz que
[o grupo de usuários de Haskell no Brazil](http://haskellbr.com/)
vem traduzindo, com minha permissão, minha série de artigos para o português
para seu [blog](http://blog.haskellbr.com/) e usando os artigos como base de
discussão e crescimento para a comunidade de Haskell local!

## Todo o código
Todo o código para a série estará
[nesse repositório do GitHub](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).

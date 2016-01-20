---
layout: post
title: Um pouco sobre Lazy Evaluation em Haskell
author: H. T., baseado na obra de Takenobu T.
author_url: "https://github.com/hannelita"
---
Um pouco sobre o significado de Lazy Evaluation e como o Haskell trabalha tal conceito. Baseado na obra de Takenobu T., [Lazy Evaluation Illustrated for Haskell Divers](https://takenobu-hs.github.io/downloads/haskell_lazy_evaluation.pdf) e com comentários e analogias agregadas livremente pela autora do post.

<!-- more -->

Quando você está escrevendo um código, possivelmente elabora algum modelo mental sobre ele. Isso mesmo, seu cérebro inicia um thread ou processo que analisa o que você está escrevendo e eventualmente você executa, ainda que mentalmente, o que está produzindo. De um modo mais formal, como é esse processo? Temos alguns modelos mentais básicos - avaliamos um trecho de código de cima para baixo, aplicamos chamadas de funções da mais interna para a mais externa, avaliamos expressões da esquerda para a direita e damos precedência para operações dentro de parênteses, por exemplo (veja a imagem a seguir). É evidente que cada desenvolvedor(a) possui seu próprio modelo mental e aplica distintos métodos para avaliar seu código.
![Modelo mental de código](/assets/images/lazy_evaluation_haskell/modelo_mental.jpg)

De qualquer forma, o que geralmente mentalizamos são diversos sentenças ordenadas regidas por uma determinada sintaxe. É aí que fica evidente como esses modelos mudam de linguagem para linguagem - principalmente pela sintaxe e pela estruturação e regras da linguagem na qual produzimos o código. Como isso funciona para Haskell? Vejamos:
![Modelo para o Haskell](/assets/images/lazy_evaluation_haskell/modelo_haskell.jpg)

No caso, temos que o programa será uma coleção de expressões - na verdade o programa inteiro é uma única expressão. O que fazemos é avaliar as subexpressões reduzidas de acordo com uma determinada ordem. Vamos substituindo trechos até mentalizarmos um output. Se você já estudou lógica sentencial ou quantitativa, talvez tenha se deparado com diversas proposições e conclusões e exercícios que solicitavam avaliar a saída dos mesmos. Você poderia recorrer a uma análise por partes ou elaborar árvores de decisão. Esses passos de avaliação, fragmentação das partes, análise e substituição é um exemplo de como você pode avaliar um código em Haskell. Esse conjunto de passos também é chamado de "modelo de expressão reduzida".

Retomando o exemplo de analisar proposições de lógica Sentencial ou quantitativa, muitas vezes notamos que podemos ignorar um trecho ou porque notamos que uma parte da proposição já a tornará sempre False/Verdadeira, ou porque já obtivemos um único valor que invalida nosso objetivo, entre outros casos. Note que deixamos de avaliar porque não seria necessário continuar o procedimento. Poupa-nos tempo e recursos - e isso é importante. No momento em que pulamos algumas etapas, somos, de certa forma, "preguiçosos" (ou "lazy"). O Haskell faz isso em muitos casos: você já deve ter ouvido falar do comportamento lazy presente na linguagem.

Quais os benefícios do que chamamos de "lazy evaluation"? É provável que você já tenha ouvido muitos - "evita o gasto desnecessário de recursos computacionais", "modularização", "abstração", "manipula infinitas estruturas de dados", etc. Há vários benefícios! Porém quais deles nos atingem mais diretamente? E o que é de fato um comportamento "lazy"?

Basicamente uma lazy evaluation avalia somente quando necessário, consumindo e analisando cada trecho no máximo uma única vez. Por isso, quando falamos em lazy, além da ideia de postergar quando um trecho será analisado,temos que pensar também na ideia de _evitar_ - a ideia é que o compilador evite ter que analisar os trechos de código com a maior frequência possível. O Haskell utiliza uma série de técnicas para implantar a lazy evaluation, dentre elas, redução de ordem normal ("normal order reduction") - um processo que permite encontrar, caso existe, a forma normal de uma expressão (ou, no inglês, Normal Form ou simplesmente NF). Encontrada tal forma, o compilador secciona-a em partes e armazena o trecho que deve ser avaliado posteriormente no heap. Uma vez no heap, a expressão fica acessível quando ocorrem algum pattern match ou requisição forçada para avaliação por exemplo. Essa explicação rápida nos alerta para alguns problemas: devemos ficar atento ao espaço que as expressões não avaliadas tomam no heap e devemos ter cuidado também com o tempo que elas levarão para ser executadas quando solicitadas. Dê uma olhada na imagem a seguir e ocê verá um esquema simplificado dos ingredientes que o compilador utiliza para propiciar o comportamento lazy:
![Lazy Evaluation no Haskell](/assets/images/lazy_evaluation_haskell/lazy_evaluation_no_haskell.jpg)

Falando em expressões, o que é uma expressão? Na imagem anterior encontramos o termo "expressão" e também o termo "valor". Qual a relação entre eles? Veja o esboço a seguir.
![Expressão e Valor](/assets/images/lazy_evaluation_haskell/expressao_e_valor.jpg)

Note que para obter um valor analisamos uma (ou mais) expressões. Existem várias formas de avaliar um conjunto de expressões - você talvez já tenha ouvido alguns desses termos - "Strict", "Non-strict", "Lazy, "Eager", "Call-by-value", "Call-by-need", "Innermost", "Outermost", entre outros. Esses nomes se referem a estratégias de avaliação de expressões e cada um deles possui um conjunto de benefícios e fraquezas. Devemos mencionar também o nível de avaliação de uma expressão:
![Nível de avaliação das expressões](/assets/images/lazy_evaluation_haskell/expressao_nivel.jpg)

Spoiler: Você se recorda que em uma imagem anterior vimos o termo WHNF? Ele faz menção ao "Weak Head Normal Form", que nada mais é do que uma expressão composta que pode ou não conter partes ainda não avaliadas. Em contrapartida, a NF não contém nenhuma subexpressão que ainda não fora avaliada. Vamos discutir esses conceitos mais detalhadamente em posts futuros aqui no blog.

Podemos encontrar diversos tipos de expressões no Haskell: lambda abstractions, let, do , condicionais, variáveis. Também temos diferentes tipos de valores - valores que representam dados e valores de função. Vamos unir esses conceitos e classificá-los em conjunto de acordo com o formato pelo qual se apresentam:
![Classificando valores e expressões](/assets/images/lazy_evaluation_haskell/categorizacao_valores.jpg)

No próximo post vamos detalhar um pouco sobre essa categorização que acabamos de fazer para os valores: WHNF, HNF ou NF. Até breve!

- - -

#### Amarrando as pontas
Se você chegou até aqui, obrigado. Muito desse post tentou ser o mais simples
possível para pessoas que nunca viram Haskell, então não, isso não é como
Haskell se parece em produção. Mesmo assim, é divertido ver o que é possível com
a linguagem e talvez isso tenha te animado para a estudar.

Dúvidas comentários e posts são
aceitos no repositório do GitHub:
[https://github.com/haskellbr/blog](https://github.com/haskellbr/blog). No
momento a meta é ter conteúdo publicado semanalmente.

<!--Caso te interesse receber e-mails semanais com o estado da União de Haskell no-->
<!--Brasil, assine aqui:-->
<!--<form class="js-digest-signup" style="height: 61px; text-align: center;">-->
  <!--<div class="after-signup" style="top: 17px; position: relative; font-weight: bold;">-->
    <!--Parabéns! Entraremos em contato!-->
  <!--</div>-->
  <!--<div class="before-signup">-->
    <!--<input placeholder="fulano@exemplo.com" name="email" type="email"></input>-->
    <!--<button class="btn" type="submit">Assinar</button>-->
  <!--</div>-->
<!--</form>-->

<!--<script>-->
<!--$(function() {-->
  <!--var $form = $('.js-digest-signup');-->
  <!--$form.on('submit', function(e) {-->
    <!--e.preventDefault();-->
    <!--e.stopPropagation();-->
    <!--$('.before-signup').fadeOut(function() {-->
      <!--$('.after-signup').fadeIn()-->
    <!--})-->
    <!--var email = $form.find('input').val();-->
    <!--console.log(email);-->
  <!--});-->
<!--});-->
<!--</script>-->

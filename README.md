RRTT, um interpretador de pseudocódigo que te permite testar o teu pseudocódigo.
```
Algoritmo caracteristicas_terreno
Variáveis
    ladoA, ladoB, perimetro, area, i: inteiro
Início
    Escrever('Digite o comprimento')
    Ler(ladoA)
    Escrever('Digite a largura')
    Ler(ladoB)
    area <- ladoA * ladoB
    perimetro <- 2 * (ladoA + ladoB)
    Se area > 1000 Então
        Escrever('O terreno é grande.')
    Senão
        Escrever('O terreno é pequeno.')
    FimSe
    Escrever('O perímetro é ', perimetro, ' e a área é ', area, '.')
Fim.
```
Se escreveres o seguinte código num ficheiro, nomeado por exemplo "teste.txt", podes abrí-lo com o executável do interpretador e o teu código irá funcionar!

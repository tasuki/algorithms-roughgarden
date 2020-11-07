Stuff for https://www.coursera.org/learn/algorithms-divide-conquer

Run single spec:

    stack test --test-arguments '--match "/A1Karatsuba/Karatsuba.multiply/multiply 14 23"'

Run a program:

    P="a1-karatsuba" && stack build "algorithms-roughgarden:$P" && stack exec "$P"

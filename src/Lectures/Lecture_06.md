## Lambda Calculus

### **Syntax**
```
E ::= V           -- Variable
    | (E E)       -- Application
    | (\ V -> E)  -- Abstraction
```
```haskell
  type VarName = String

  data Expr
    = Variable String
    | Application Expr Expr
    | Abstraction VarName Expr
```

### **Semantics**
```
(λx . ((pow x) 2))

 λx . ((pow x) 2)

 λx . (pow x) 2
```

### **Lambda Properties**

```
• α-conversion

  One can be expressed in different ways:
  
  (λx . x) z == z
  (λy . y) z == z
          ↓
  (λx . x) == (\y -> y)


• β-reduction 

  (λx . x) z → z                -- result (reduction)

  ((λx . E) a) == E[x := a]     -- все связанные вхождения x
                       ^           заменены на a, где Е - это
                  подстановка      выражение

• η-reduction

  f = λx . x + x
  g = λx . x * 2

  (∀ x. f x == g x) <=> f == g  -- η-reduction
                                   ∀ - это квантор всеобщности

  (λx . x) z == z == (λy . y) z
  (λx . x) z    ==   (λy . y) z
  (λx . x)      ==   (λy . y)   -- η-reduction

  f == g => f x == g x          -- η-expansion

  x == y => f x == f y          -- η-expansion (corollary)
```
```
  (λx . f x) y == f y           -- applied β-reduction
  (λx . f x) y == f y           -- applied η-reduction
```

### **Haskell**

```
  f = \x -> x + x                               -- def 1
  
    f x == (\x -> x + x) x == x + x
    f x = x + x

  f x = x + x                                   -- def 2
```
```
  discriminant a b c = b * b - 4 * a * c        -- def 3 
                                                   - triple equation

    ((discriminant a) b) c  =                    b * b - 4 * a * c
    (discriminant a) b      = \c              -> b * b - 4 * a * c
    discriminant a          = \b -> \c        -> b * b - 4 * a * c
    discriminant            = \a -> \b -> \c  -> b * b - 4 * a * c
  
  discriminant = \a b c -> b * b - 4 * a * c    -- def 4
                                                   - triple abstraction
```
**Free** and **bound** variables (свободные и связанные переменные).
Сокращению подлежат только свободные переменные:

```
  f x = ((+) x) x                 -- we cannot avoid x
```
С точки зрениия `λ`-исчесления, не бывает неполного набора аргументов: набор всегда полный.

```
  discriminant' (a, b, c) = b * b - 4 * a * c
  discriminant' = \(a, b, c) -> b * b - 4 * a * c
  discriminant' = \(t) -> (proj2 t) * (proj2 t) - 4 * (proj1 t) * (proj3 t)

  -- каноническая запись каррирования, где
     proj1..3 t - 1..3 проекции t
```
Две функции `discriminant` и `discriminant'` не эквивалентны с точки зрения `λ`-исчисления, но они изоморфны: то есть они эквиваленты как абстракция уровнем выше.

```
  f (x, y) = e[x, y]
  g x y    = e[x, y]                -- curried

  curry f = g
          = \x y -> e[x, y]
          = \x y -> f (x, y)

  curry = \f x y -> f (x, y)        -- lambda (канонический вид)
  curry f x y = f (x, y)            -- equation
  
  uncurry g = f
            = \x y -> e[x, y]
            = \x y -> g x y
  
  uncurry = \g (x y) -> g x y       -- lambda
  uncurry g (x y) = g x y           -- equation
```

### **Haskell Type**

```haskell
  f1 :: Integer -> Integer
  f1 x = x + x

  f2 x y = x + y
  f2 x = \y -> x + y
  f2 x :: Integer -> Integer
  f2 :: Integer -> (Integer -> Integer)

  -- канонический вид
  discriminant :: Double -> (Double -> (Double -> Double))
  -- общепринятый вид
  discriminant :: Double -> Double -> Double -> Double
```
```haskell
-- some operator
  (?) :: a -> b -> c

  x :: a
  y :: b

  -- left and right section - сечение
  x ? y :: c
  (x ?) = \y -> x ? y == (?) x        -- left section
  (? y) = \x -> x ? y                 -- right section
```
Все операторы в **Haskell** бинарные, кроме `-` -- это унарный оператор -- псевдоним функции `negate`. С другой стороны `(-)` -- это биранрая функция.

```haskell
  (- y) = negate y
  (x -) = (-) y
```
  Соответсвенно, для данного оператора существует исключение при определении частичного примениения:

```haskell
  subtractThisFromX = \y -> x - y == (x -)
  subtractYFromThis = \x -> x - y == subtract y
```

Отличие вызова функции от аппликации: **аппликация** -- это синтаксис, т.е. вид формулы. А вызов функции просходит там, где она применяется.<br />
Применённый в функции параметр (`f x`) становится аргументом (`f 1`) или значением параметра.

```
  id = \x -> x                        -- I (Identität)
                                         - из оснований математики (нем.)
  const = \x y -> x                   -- K (Konstante)
                                         - из оснований математики (нем.)
```

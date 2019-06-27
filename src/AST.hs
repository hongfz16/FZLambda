-- |Abstract Syntax Tree
module AST
  ( Type (..)
  , Pattern (..)
  , Expr (..)
  , ADT (..)
  , Program (..)
  , Result (..) ) where


-- |你的语言里的类型（type），包括基本数据类型、函数类型以及代数数据类型。
data Type
  
  = TBool
  -- ^布尔类型。

  | TInt
  -- ^有限精度整数类型。

  | TChar
  -- ^字符类型。

  | TArrow Type Type
  -- ^箭头类型，这是函数具有的类型。在 @TArrow t1 t2@ 构成的函数类型里，@t1@ 表示函数的参数类型，@t2@ 表示函数的返回值类型。

  | TData String
  -- ^代数数据类型。@TData adtName@ 中的 @adtName@ 是该类型的名称。

  deriving (Show, Eq)


-- |你的语言里的模式（pattern），用于模式匹配（pattern matching）。如果你要实现的语言不支持模式匹配，就不用管这个类型。
data Pattern

  = PBoolLit Bool
  -- ^布尔字面量模式。

  | PIntLit Int
  -- ^有限精度整数字面量模式。

  | PCharLit Char
  -- ^字符字面量模式。

  | PVar String
  -- ^变量模式。
  --
  -- 用 Haskell 语法粗略描述，
  -- 
  -- @
  -- fac n = case n of
  --   0 -> 1
  --   x -> x * fac (x-1)
  -- @
  -- 
  -- 其中 @->@ 左侧的 @x@ 相当于 @PVar "x"@，@->@ 右侧的 @x@ 相当于 @EVar "x"@。@0 -> 1@ 则对应 @PIntLit 0@ 和 @EIntLit 1@。


  | PData String [Pattern]
  -- ^代数数据类型模式，其中 @String@ 是某个代数数据类型的构造函数， @[Pattern]@ 是该构造函数的参数。

  deriving (Show, Eq)


-- |你的语言里的表达式（expression），包括基本数据类型字面量、基本数据类型之间的运算、条件语句、let-In 表达式、lambda 表达式、函数定义、代数数据类型定义。你的语言里每一个合法的表达式都有一个类型和一个值；你的语言里的类型是 Haskell 里 @Type@ 类型的变量。
data Expr
  
  = EBoolLit Bool
  -- ^布尔类型字面量（literal）。
  
  | EIntLit Int
  -- ^有限精度整数类型字面量。
  
  | ECharLit Char
  -- ^字符类型字面量。

  | ENot Expr
  -- ^逻辑取反表达式。@ENot e@ 中的 @e@ 必须是一个类型为 @TBool@ 的表达式，否则不 well-typed。该表达式的类型为 @TBool@，值为 @e@ 的值取反。
  
  | EAnd Expr Expr
  -- ^逻辑“且”表达式。@EAnd e1 e2@ 中的 @e1@ 和 @e2@ 都必须是类型为 @TBool@ 的表达式，否则不 well-typed。
  --
  -- 该表达式的类型为 @TBool@，值为 @e1@ 和 @e2@ 的值的逻辑“且”。
  --
  -- 对该表达式的求值必须具有短路特性，即当 @e1@ 值为 @False@ 时，不求 @e2@ 的值，直接返回 @False@。
  
  | EOr Expr Expr
  -- ^逻辑“或”表达式。@EOr e1 e2@ 中的 @e1@ 和 @e2@ 都必须是类型为 @TBool@ 的表达式，否则不 well-typed。
  --
  -- 该表达式的类型为 @TBool@，值为两个 @Expr@ 的值的逻辑“或”。
  --
  -- 对该表达式的求值必须具有短路特性，即当 @e1@ 值为 @True@ 时，不求 @e2@ 的值，直接返回 @True@。

  | EAdd Expr Expr
  -- ^有限精度整数加法表达式。@EAdd e1 e2@ 中的 @e1@ 和 @e2@ 都必须是类型为 @TInt@ 的表达式，否则不 well-typed。
  --
  -- 该表达式的类型为 @TInt@，值为 @e1@ 的值加上 @e2@ 的值。

  | ESub Expr Expr
  -- ^有限精度整数减法表达式。@ESub e1 e2@ 中的 @e1@ 和 @e2@ 都必须是类型为 @TInt@ 的表达式，否则不 well-typed。
  --
  -- 该表达式的类型为 @TInt@，值为 @e1@ 的值减去 @e2@ 的值。

  | EMul Expr Expr
  -- ^有限精度整数乘法表达式。@EMul e1 e2@ 中的 @e1@ 和 @e2@ 都必须是类型为 @TInt@ 的表达式，否则不 well-typed。
  --
  -- 该表达式的类型为 @TInt@，值为 @e1@ 的值乘以 @e2@ 的值。

  | EDiv Expr Expr
  -- ^有限精度整数除法表达式。@EDiv e1 e2@ 中的 @e1@ 和 @e2@ 都必须是类型为 @TInt@ 的表达式，否则不 well-typed。
  --
  -- 该表达式的类型为 @TInt@，值为 @e1@ 的值除以 @e2@ 的值。
  --
  -- 当 @e2@ 的值为 @0@ 时，你可以任意处理（即 undefined behavior）。

  | EMod Expr Expr
  -- ^有限精度整数取模表达式。@EMod e1 e2@ 中的 @e1@ 和 @e2@ 都必须是类型为 @TInt@ 的表达式，否则不 well-typed。
  --
  -- 该表达式的类型为 @TInt@，值为 @e1@ 的值对 @e2@ 的值取模（modulo）。
  --
  -- 当 @e2@ 的值为 @0@ 时，你可以任意处理（即 undefined behavior）。

  | EEq Expr Expr
  -- ^布尔、有限精度整数、字符判等表达式。@EEq e1 e2@ 中 @e1@ 和 @e2@ 的类型相同，且为 @TBool@ 、@TInt@、@TChar@ 三者之一，否则不 well-typed。
  --
  -- 该表达式的类型为 @TBool@，当 @e1@ 和 @e2@ 的值相等时，值为 @True@ ，否则为 @False@ 。

  | ENeq Expr Expr
  -- ^布尔、有限精度整数、字符“不等”表达式。@ENeq e1 e2@ 中 @e1@ 和 @e2@ 的类型相同，且为 @TBool@ 、@TInt@、@TChar@ 三者之一，否则不 well-typed。
  --
  -- 该表达式的类型为 @TBool@，当 @e1@ 和 @e2@ 的值相等时，值为 @False@ ，否则为 @True@ 。
  
  | ELt Expr Expr
  -- ^小于比较表达式。@ELt e1 e2@ 中 @e1@ 和 @e2@ 的类型相同，且为 @TInt@、@TChar@ 两者之一，否则不 well-typed。
  --
  -- 该表达式的类型为 @TBool@，当 @e1@ 的值小于 @e2@ 的值时，值为 @True@ ，否则为 @False@ 。字符按其 Unicode 来比较（和 Haskell 相同）。
  
  | EGt Expr Expr
  -- ^大于比较表达式。@EGt e1 e2@ 中 @e1@ 和 @e2@ 的类型相同，且为 @TInt@、@TChar@ 两者之一，否则不 well-typed。
  --
  -- 该表达式的类型为 @TBool@，当 @e1@ 的值大于 @e2@ 的值时，值为 @True@ ，否则为 @False@ 。字符按其 Unicode 来比较（和 Haskell 相同）。
  
  | ELe Expr Expr
  -- ^小于等于比较表达式。@ELe e1 e2@ 中 @e1@ 和 @e2@ 的类型相同，且为 @TInt@、@TChar@ 两者之一，否则不 well-typed。
  --
  -- 该表达式的类型为 @TBool@，当 @e1@ 的值小于等于 @e2@ 的值时，值为 @True@ ，否则为 @False@ 。字符按其 Unicode 来比较（和 Haskell 相同）。

  | EGe Expr Expr
  -- ^大于等于比较表达式。@EGe e1 e2@ 中 @e1@ 和 @e2@ 的类型相同，且为 @TInt@、@TChar@ 两者之一，否则不 well-typed。
  --
  -- 该表达式的类型为 @TBool@，当 @e1@ 的值大于等于 @e2@ 的值时，值为 @True@ ，否则为 @False@ 。字符按其 Unicode 来比较（和 Haskell 相同）。

  | EIf Expr Expr Expr
  -- ^条件表达式。@EIf e1 e2 e3@ 中，@e1@ 是条件，@e2@ 是 @then@ 分支，@e3@ 是 @else@ 分支。@e1@ 类型必须为 @TBool@，@e2@ 和 @e3@ 的类型必须相同，否则不 well-typed。
  --
  -- 该表达式的类型与 @e2@ 的类型相同。当 @e1@ 的值为 @True@ 时，该表达式的值为 @e2@ 的值，否则为 @e3@ 的值。
  --
  -- 对该表达式的求值必须是非严格的，即：当条件为 @True@ 时，只对 @then@ 分支求值，当条件为 @False@ 时，只对 @else@ 分支求值。

  | ELambda (String, Type) Expr
  -- ^ @lambda@ 表达式。@ELambda (pn, pt) e@ 中，@(pn, pt)@ 分别表示该 @lambda@ 表达式的参数名和参数类型；@e@ 是函数体。
  --
  -- 该表达式的类型为 @TArrow T0 T1@ ，其中 @T0@ 是参数类型 @pt@， @T1@ 是返回值类型，即 @e@ 的类型。

  | ELet (String, Expr) Expr
  -- ^ @Let In@ 表达式。@ELambda (n, e1) e2@ 中，@(n, e1)@ 表示创建一个从 @n@ 到 @e1@ 的绑定，@e2@ 是该绑定的作用域。用 Haskell 语法粗略表示为 @let n = e1 in e2@。
  --
  -- 该表达式的类型为 @e2@ 的类型，值为 @e2@ 的值。
  --
  -- 与 Haskell 的 @let ... in ...@ 不同的是，此处的 @e1@ 中不可以出现对于该 @n@ 的引用，而 @e2@ 中可以出现对于 @n@ 的引用。这造成不能只用 @ELet@ 和 @ELambda@ 来定义递归函数；要定义递归函数需要 @ELetRec@。

  | ELetRec String (String, Type) (Expr, Type) Expr
  -- ^针对函数定义的 @Let In@ 表达式。@ELetRec f (x, tx) (e1, ty) e2@ 中，其中 @f@ 表示函数名，@(x, tx)@ 表示参数名和参数类型，@(e1, ty)@ 表示函数体和返回值类型。该表达式会创建一个从函数名到函数体的绑定，这个绑定值的类型为 @TArrow tx ty@；@e2@ 是该绑定的作用域。用 Haskell 语法粗略表示为 @let f = ((\x -> e1) :: tx -> ty) in e2@。
  --
  -- 该表达式的类型为 @e2@ 的类型，值为 @e2@ 的值。
  -- 
  -- 使用 @ELetRec@ 与使用 @ELet@ + @ELambda@ 的区别在于：函数体 @e1@ 中允许引用正在被定义的函数 @f@。从而可以定义递归的函数。

  | EVar String
  -- ^变量表达式。@EVar n@ 的类型为 @n@ 所绑定到的表达式的类型，值为 @n@ 所绑定到的表达式的值。 @n@ 到表达式的绑定关系由 @ELambda@、@ELet@、@ELetRec@ 创建。
  -- 
  -- 如果你实现了模式匹配，绑定关系也会由模式匹配创建。

  | EApply Expr Expr
  -- ^函数应用表达式。@EApply e1 e2@ 中，@e1@ 的类型为函数类型，即某个 @TArrow T0 T1@ ，第二个 @Expr@ 的类型为 @T0@ ，否则不 well-typed。
  -- 
  -- 该表达式的类型为 @T1@ ，值为将 @e1@ 的值应用到 @e2@ 的值上得到的值。
  -- 
  -- 如果实现了代数数据类型的支持，@e1@ 也可以是代数数据类型的构造函数（data constructor）。

  | ECase Expr [(Pattern, Expr)]
  -- ^模式匹配表达式。@ECase e0 [(p1, e1),(p2, e2),...]@ 中，@e0@ 为被匹配的表达式，之后的 @[(p1, e1),(p2, e2),...]@ 表示若干个 模式-表达式 对儿。这里 @e1,e2,...@ 的类型必须相同，且 @p1,p2,...@ 必须与 @e0@ 的类型匹配，否则不 well-typed。
  -- 
  -- 该表达式的类型为 @e1@ 的类型，值为第一个匹配上的模式对应的表达式的值。
  --
  -- 如果你要实现的语言不支持模式匹配，就不用管这个构造函数。

  deriving (Show, Eq)


-- |代数数据类型定义。
-- 
-- @ADT T [(D1, [t11, t12, ...]), (D2, [t21, t22, ...]), ...]@ 表示定义了一个名为 @T@（的值）的代数数据类型，具有若干个构造函数 @D1, D2, ...@，且第 i 个构造函数的参数类型为 @ti1, ti2, ...@。
--
-- 如果你要实现的语言不支持代数数据类型，就不用管这个类型。
data ADT = ADT String [(String, [Type])] deriving (Show, Eq)


-- |程序(Program)，一个 @Program@ 由代数数据类型的定义 @[ADT]@ 以及程序体 @Expr@ 构成。
-- 
-- 对于没有 ADT 定义的程序，或未支持 ADT 的语言，将 @[ADT]@ 列表留空即可。
data Program = Program [ADT] Expr deriving (Show, Eq)


-- |求值结果，用于 @evalValue@ 的返回值。只需要实现返回布尔类型、有限精度整数类型、字符类型，其余的求值结果以及发生错误的求值过程都返回 @RInvalid@。
data Result

  = RBool Bool
  -- ^存放布尔类型求值结果。
  
  | RInt Int
  -- ^存放有限精度整数类型求值结果。
  
  | RChar Char
  -- ^存放字符类型求值结果。
  
  | RInvalid
  -- ^不合法的求值结果，包括 1. 求值发生错误；2. 求值结果并非布尔类型、有限精度整数类型、字符类型。

  | RData String

  deriving (Show, Eq)

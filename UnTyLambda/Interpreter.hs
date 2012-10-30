 {-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- В данном задании требуется реализовать интерпретатор для
-- нетипизированной лямбды
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- Какие-то импорты. Заметьте, что в этом задании надо
-- использовать обычную Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- Определение дататайпа для нетипизированной лямбды
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- Дальше всё на ваше усмотрение

-- Если внутри будете использовать именованное представление, то
-- я тут решил немного вам помочь
-- (иначе говоря, код из этого раздела можно совсем выкинуть,
-- если хочется)

-- return a list of free variables in the term
free (Var v) = [ v ]
free (Lam v t) = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

-- alpha-conversion
subst :: Term -> Variable -> Term -> Term
subst t@(Var v) var what = if v == var then what else t
subst t@(Lam v b) var what = if v == var then t else Lam v (subst b var what)
subst (App t t') var what = App (subst t var what) (subst t' var what)

-- add '_' to name maybe
newname fv = head . filter (not . flip elem fv) . iterate ('_':)

--beta-reduction
betaRed :: Variable -> Term -> Term -> Term
betaRed var t1 t2 = subst t1 var (assignUniqueNames t2 (free t1))
	where
	assignUniqueNames :: Term -> [ Variable ] -> Term
	assignUniqueNames t@(Var x) names = t
	assignUniqueNames t@(Lam x tx) names = Lam nx ntx
		where
		nx = if (elem x names) then newname (names ++ free tx) x else x
		ntx = if (elem x names) then subst tx x (Var nx) else tx


-- When no redexes - term is calculated. F-yeah.
hasRedexes (Var x) = False
hasRedexes (Lam x tx) = hasRedexes tx
hasRedexes (App (Lam x tx) ty) = True
hasRedexes (App tx ty) = hasRedexes tx || hasRedexes ty

-- root-left-right. Call this function until the term has redexes.
normalReduce :: Term -> Term
normalReduce t@(Var x) = t
normalReduce (Lam x tx) = Lam x (normalReduce tx)
normalReduce (App (Lam x tx) ty) = betaRed x tx ty
normalReduce (App t1@(App (Lam x tx) ty) t2) = App (normalReduce t1) t2
normalReduce (App t1 t2) = App t1 (normalReduce t2)

-- left-right-root. Call this function until the term has redexes.
appReduce :: Term -> Term
appReduce t@(Var x) = t
appReduce (Lam x tx) = Lam x (appReduce tx)
appReduce (App t1@(App (Lam x tx) ty) t2) = App (appReduce t1) t2
appReduce (App t1 t2@(App (Lam x tx) ty)) = App t1 (appReduce t2)
appReduce (App (Lam x tx) ty) = betaRed x tx ty

-- root is not redex and left is not redex == week head form
inWeakHeadForm :: Term -> Bool
inWeakHeadForm (Var x) = True
inWeakHeadForm (Lam x tx) = True
inWeakHeadForm (App (Lam x tx) ty) = False
inWeakHeadForm (App t1 t2) = inWeakHeadForm t1

-- week head reduce
weakHeadReduce :: Term -> Term
weakHeadReduce (App (Lam x tx) ty) = subst tx x ty
weakHeadReduce t@(App t1 t2) = if not (inWeakHeadForm t1) then App (weakHeadReduce t1) t2 else t

------------------------------------------------------------
-- За исключением того, что требуется реализовать следующие
-- стратегии нормализации (они все принимают максимальное
-- число шагов интерпретатора в качестве первого
-- параметра (n); если за n шагов нормализовать не удаётся,
-- то следует бросать error, тестер его поймает):

wh, no, wa, sa :: Integer -> Term -> Term

-- Редукция аппликативным порядком
sa 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
sa n t = if hasRedexes t then sa (n - 1) (appReduce t) else t

-- Нормализация нормальным порядком
no 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
no n t = if hasRedexes t then no (n - 1) (normalReduce t) else t

-- Редукция в слабую головную нормальную форму
wh 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
wh n t = if not (inWeakHeadForm t) then wh (n - 1) (weakHeadReduce t) else t

-- (*) (не обязательно) Редукция "слабым" аппликативным порядком.
-- Отличается от обычного аппликативного тем, что не лезет внутрь
-- лямбд и правые части аппликаций, когда это возможно.
wa = undefined

-- Замечание: cкорость работы вашего интерпретатора специально не оценивается,
-- потому можно использовать свой изоморфный (с точностью до альфа-конверсии)
-- тип для представления термов и преобразовывать Term в него и обратно.

-- Перечисление всех этих порядков (в порядке отличном от
-- определения, да)
orders =
    [ ("wh", wh)
    , ("no", no)
-- , ("wa", wa) -- Можно раскоментировать, да
    , ("sa", sa) ]

------------------------------------------------------------
-- Игнорируйте это, если выглядит непонятно
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- Сюда можно добавлять тесты
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- Немного теоретических замечаний, если они вас волнуют
--
-- Следует специально отметить, что поскольку в конце теста
-- результат вычисления печатают, то ленивость Haskell не
-- влияет на семантику интерпретируемого исчисления.
--
-- Чтобы это особенно подчеркнуть в тестере выше я написал
-- seq в интересном месте (хотя конкретно это там ничего не
-- гарантирует, на самом-то деле).

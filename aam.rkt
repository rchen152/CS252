#lang racket
(require redex)
(define-language tCESK*
  (e x
     v
     (e e))
  (v (lmd x e))
  (x variable-not-otherwise-mentioned)
  (r any)
  (s any)
  (k mt
     (ar e r a)
     (fn v r a))
  (a number)
  (t number))
(define-metafunction tCESK*
  [(empty) ,(hash)])
(define-metafunction tCESK*
  [(lookup any_key any_map) ,(hash-ref (term any_map) (term any_key))])
(define-metafunction tCESK*
  [(insert any_key any_val any_map) ,(hash-set (term any_map) (term any_key) (term any_val))])
(define-metafunction tCESK*
  [(inj e) (e (empty) (insert 0 mt (empty)) 0 1)])
(define-metafunction tCESK*
  [(tick t) ,(+ (term t) 1)])
(define-metafunction tCESK*
  [(alloc t) t])
(define reduce
  (reduction-relation
   tCESK*
   (--> (x r s a t) ,(append (term (lookup (lookup x r) s)) (term (s a (tick t)))) "1")
   (--> ((e_1 e_2) r s a t)
        ,(let ([t_p (term (tick t))] [a_p (term (alloc t))])
           (term (e_1 r (insert ,a_p (ar e_2 r a) s) ,a_p ,t_p))) "2")
   (--> (v r s a t)
        ,(let ([t_p (term (tick t))] [a_p (term (alloc t))])
           (term (e r_p (insert ,a_p (fn v r a_pp) s) ,a_p ,t_p)))
        (where (ar e r_p a_pp) (lookup a s)) "3")
   (--> (v r s a t)
        ,(let ([t_p (term (tick t))] [a_p (term (alloc t))])
           (term (e (insert x ,a_p r_p) (insert ,a_p (v r) s) a_pp ,t_p)))
        (where (fn (lmd x e) r_p a_pp) (lookup a s)) "4")))
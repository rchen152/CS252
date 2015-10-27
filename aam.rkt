#lang racket
(require redex)
(define-language tCESK*
  (e x
     v
     (e e)
     (op e ...))
  (v (lmd x e)
     number)
  (x variable-not-otherwise-mentioned)
  (op * / + -)
  (c (e r))
  (r ((x a) ...))
  (s ((a vk) ...))
  (k mt
     (ar c a)
     (fn vr a)
     (op (vr ...) (c ...) a))
  (a number)
  (t number)
  (vk vr k)
  (vr (v r)))
(define-metafunction tCESK*
  [(inj e) ((e ()) ((0 mt)) 0 1)])
(define-metafunction tCESK*
  [(dlt * number ...) ,(apply * (term (number ...)))]
  [(dlt / number ...) ,(apply / (term (number ...)))]
  [(dlt + number ...) ,(apply + (term (number ...)))]
  [(dlt - number ...) ,(apply - (term (number ...)))])
(define-metafunction tCESK*
  [(lookup x r) ,(second (assoc (term x) (term r)))]
  [(lookup a s) ,(second (assoc (term a) (term s)))])
(define-metafunction tCESK*
  [(insert x a r) ,(cons (term (x a)) (term r))]
  [(insert a vk s) ,(cons (term (a vk)) (term s))])
(define-metafunction tCESK*
  [(tick t) ,(+ (term t) 1)])
(define-metafunction tCESK*
  [(alloc t) t])
(define reduce
  (reduction-relation
   tCESK*
   (--> ((x r) s a t) ((lookup (lookup x r) s) s a (tick t)))
   (--> (((e_1 e_2) r) s a t)
        ,(let ([t_p (term (tick t))] [a_p (term (alloc t))])
           (term ((e_1 r) (insert ,a_p (ar (e_2 r) a) s) ,a_p ,t_p))))
   (--> (((op e_1 e_2 ...) r) s a t)
        ,(let ([t_p (term (tick t))] [a_p (term (alloc t))] [cs (map (lambda (e) (list e (term r))) (term (e_2 ...)))])
           (term ((e_1 r) (insert ,a_p (op () ,cs a) s) ,a_p ,t_p))))
   (--> (vr s a t)
        ,(let ([t_p (term (tick t))] [a_p (term (alloc t))])
           (term (c (insert ,a_p (fn vr a_pp) s) ,a_p ,t_p)))
        (where (ar c a_pp) (lookup a s)))
   (--> (vr s a t)
        ,(let ([t_p (term (tick t))] [a_p (term (alloc t))])
           (term ((e (insert x ,a_p r)) (insert ,a_p vr s) a_pp ,t_p)))
        (where (fn ((lmd x e) r) a_pp) (lookup a s)))
   (--> (vr s a t)
        ,(let ([t_p (term (tick t))] [a_p (term (alloc t))])
           (term (c_1 (insert ,a_p (op (vr_1 ... vr) (c_2 ...) a_pp) s) ,a_p ,t_p)))
        (where (op (vr_1 ...) (c_1 c_2 ...) a_pp) (lookup a s)))
   (--> ((v r_n) s a t)
        ,(let ([t_p (term (tick t))] [a_p (term (alloc t))])
           (term (((dlt op v_1 ... v) ()) s a_pp ,t_p)))
        (where (op ((v_1 r_n1) ...) () a_pp) (lookup a s)))))
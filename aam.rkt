#lang racket
(require redex)
(define-language CESK
  (x variable-not-otherwise-mentioned)
  (v (lmd x e)
     number)
  (a number)
  (op * / + -)
  (e x
     v
     (e e)
     (op e ...))
  (vp (v p))
  (c (e p))
  (p ((x a) ...))
  (s ((a vp) ...))
  (k mt
     (arg c k)
     (fun vp k)
     (op (vp ...) (c ...) k)))
(define-metafunction CESK
  [(inj e) ((e ()) () mt)])
(define-metafunction CESK
  [(dlt * number ...) ,(apply * (term (number ...)))]
  [(dlt / number ...) ,(apply / (term (number ...)))]
  [(dlt + number ...) ,(apply + (term (number ...)))]
  [(dlt - number ...) ,(apply - (term (number ...)))])
(define-metafunction CESK
  [(lookup x p) ,(second (findf (lambda (obj) (equal? (first obj) (term x))) (term p)))]
  [(lookup a s) ,(second (findf (lambda (obj) (equal? (first obj) (term a))) (term s)))])
(define-metafunction CESK
  [(insert x a p) ,(cons (term (x a)) (term p))]
  [(insert a vp s) ,(cons (term (a vp)) (term s))])
(define reduce
  (reduction-relation
   CESK
   (--> ((x p) s k) ((lookup (lookup x p) s) s k))
   (--> (((e_1 e_2) p) s k) ((e_1 p) s (arg (e_2 p) k)))
   (--> (((lmd x e) p) s (arg c k)) (c s (fun ((lmd x e) p) k)))
   (--> (vp s (fun ((lmd x e) p) k)) ((e (insert x ,(length (term s)) p)) (insert ,(length (term s)) vp s) k))
   (--> (((op e_1 e_2 ...) p) s k) ((e_1 p) s (op () ,(map (lambda (e) (list e (term p))) (term (e_2 ...))) k)))
   (--> (vp_1 s (op (vp_2 ...) (c_1 c_2 ...) k)) (c_1 s (op (vp_2 ... vp_1) (c_2 ...) k)))
   (--> ((v_1 p_1) s (op ((v_2 p_2) ...) () k)) (((dlt op v_2 ... v_1) ()) s k))))
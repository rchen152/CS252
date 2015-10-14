#lang racket
(require redex)
(define-language CEK
  (x variable-not-otherwise-mentioned)
  (v (lmd x e)
     number)
  (op * / + -)
  (e x
     v
     (e e)
     (op e ...))
  (vp (v p))
  (c (e p))
  (p ((x c) ...))
  (k mt
     (arg c k)
     (fun vp k)
     (op (vp ...) (c ...) k)))
(define-metafunction CEK
  [(inj e) ((e ()) mt)])
(define-metafunction CEK
  [(dlt (* number ...)) ,(apply * (term (number ...)))]
  [(dlt (/ number ...)) ,(apply / (term (number ...)))]
  [(dlt (+ number ...)) ,(apply + (term (number ...)))]
  [(dlt (- number ...)) ,(apply - (term (number ...)))])
(define reduce
  (reduction-relation
   CEK
   (--> ((x p) k) (,(second (findf (lambda (c) (equal? (first c) (term x))) (term p))) k))
   (--> (((e_1 e_2) p) k) ((e_1 p) (arg (e_2 p) k)))
   (--> (((lmd x e) p) (arg c k)) (c (fun ((lmd x e) p) k)))
   (--> (vp (fun ((lmd x e) p) k)) ((e ,(cons (term (x vp)) (term p))) k))
   (--> (((op e_1 e_2 ...) p) k) ((e_1 p) (op () ,(map (lambda (e) (list e (term p))) (term (e_2 ...))) k)))
   (--> (vp_1 (op (vp_2 ...) (c_1 c_2 ...) k)) (c_1 (op (vp_2 ... vp_1) (c_2 ...) k)))
   (--> ((v_1 p_1) (op ((v_2 p_2) ...) () k)) (((dlt (op v_2 ... v_1)) ()) k))))
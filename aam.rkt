#lang racket
(require redex)
(define-language CEK
  (x variable-not-otherwise-mentioned)
  (v (lam x e)
     number)
  (e x
     v
     (e e)
     (+ e ...))
  (vp (v p))
  (c (e p))
  (p ((x c) ...))
  (k mt
     (arg c k)
     (fun vp k)
     (+ (vp ...) (c ...) k)))
(define-metafunction CEK
  [(inj e) ((e ()) mt)])
(define reduce
  (reduction-relation
   CEK
   (--> ((x p) k) (,(second (findf (lambda (c) (equal? (first c) (term x))) (term p))) k))
   (--> (((e_1 e_2) p) k) ((e_1 p) (arg (e_2 p) k)))
   (--> (((lam x e) p) (arg c k)) (c (fun ((lam x e) p) k)))
   (--> (vp (fun ((lam x e) p) k)) ((e ,(cons (term (x vp)) (term p))) k))
   (--> (((+ e_1 e_2 ...) p) k) ((e_1 p) (+ () ,(map (lambda (e) (list e (term p))) (term (e_2 ...))) k)))
   (--> (vp_1 (+ (vp_2 ...) (c_1 c_2 ...) k)) (c_1 (+ (vp_2 ... vp_1) (c_2 ...) k)))
   (--> ((v_1 p_1) (+ ((v_2 p_2) ...) () k)) ((,(+ (apply + (term (v_2 ...))) (term v_1)) ()) k))))
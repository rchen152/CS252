#lang racket
(require redex)
(define-language CESK*
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
  (k mt
     (arg c a)
     (fun vp a)
     (op (vp ...) (c ...) a))
  (z vp k)
  (s ((a z) ...)))
(define-metafunction CESK*
  [(inj e) ((e ()) ((0 mt)) 0)])
(define-metafunction CESK*
  [(dlt * number ...) ,(apply * (term (number ...)))]
  [(dlt / number ...) ,(apply / (term (number ...)))]
  [(dlt + number ...) ,(apply + (term (number ...)))]
  [(dlt - number ...) ,(apply - (term (number ...)))])
(define-metafunction CESK*
  [(lookup x p) ,(second (findf (lambda (obj) (equal? (first obj) (term x))) (term p)))]
  [(lookup a s) ,(second (findf (lambda (obj) (equal? (first obj) (term a))) (term s)))])
(define-metafunction CESK*
  [(insert x a p) ,(cons (term (x a)) (term p))]
  [(insert a z s) ,(cons (term (a z)) (term s))])
(define-metafunction CESK*
  [(alloc s) ,(length (term s))])
(define reduce
  (reduction-relation
   CESK*
   (--> ((x p) s a) ((lookup (lookup x p) s) s a))
   (--> (((e_1 e_2) p) s a) ((e_1 p) (insert (alloc s) (arg (e_2 p) a) s) (alloc s)))
   (--> (vp ((a_1 z_1) ... (a_3 (arg c a_4)) (a_2 z_2) ...) a_3)
        ,(let ([s (term ((a_1 z_1) ... (a_3 (arg c a_4)) (a_2 z_2) ...))]) (term (c (insert (alloc ,s) (fun vp a_4) ,s) (alloc ,s)))))
   (--> (vp ((a_1 z_1) ... (a_3 (fun ((lmd x e) p) a_4)) (a_2 z_2) ...) a_3)
        ,(let ([s (term ((a_1 z_1) ... (a_3 (fun ((lmd x e) p) a_4)) (a_2 z_2) ...))]) (term ((e (insert x (alloc ,s) p)) (insert (alloc ,s) vp ,s) a_4))))
   (--> (((op e_1 e_2 ...) p) s a) ((e_1 p) (insert (alloc s) (op () ,(map (lambda (e) (list e (term p))) (term (e_2 ...))) a) s) (alloc s)))
   (--> (vp_1 ((a_1 z_1) ... (a_3 (op (vp_2 ...) (c_1 c_2 ...) a_4)) (a_2 z_2) ...) a_3)
        ,(let ([s (term ((a_1 z_1) ... (a_3 (op (vp_2 ...) (c_1 c_2 ...) a_4)) (a_2 z_2) ...))]) (term (c_1 (insert (alloc ,s) (op (vp_2 ... vp_1) (c_2 ...) a_4) ,s) (alloc ,s)))))
   (--> ((v_1 p_1) ((a_1 z_1) ... (a_3 (op ((v_2 p_2) ...) () a_4)) (a_2 z_2) ...) a_3)
        ,(let ([s (term ((a_1 z_1) ... (a_3 (op ((v_2 p_2) ...) () a_4)) (a_2 z_2) ...))]) (term (((dlt op v_2 ... v_1) ()) ,s a_4))))))
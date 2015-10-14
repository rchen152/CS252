#lang racket
(require redex)
(define-language t-CESK*
  (x variable-not-otherwise-mentioned)
  (v (lmd x e)
     number)
  (op * / + -)
  (a number)
  (t number)
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
(define-metafunction t-CESK*
  [(inj e) ((e ()) ((0 mt)) 0 1)])
(define-metafunction t-CESK*
  [(dlt * number ...) ,(apply * (term (number ...)))]
  [(dlt / number ...) ,(apply / (term (number ...)))]
  [(dlt + number ...) ,(apply + (term (number ...)))]
  [(dlt - number ...) ,(apply - (term (number ...)))])
(define-metafunction t-CESK*
  [(lookup x p) ,(second (findf (lambda (obj) (equal? (first obj) (term x))) (term p)))]
  [(lookup a s) ,(second (findf (lambda (obj) (equal? (first obj) (term a))) (term s)))])
(define-metafunction t-CESK*
  [(insert x a p) ,(cons (term (x a)) (term p))]
  [(insert a z s) ,(cons (term (a z)) (term s))])
(define-metafunction t-CESK*
  [(tick t) ,(+ (term t) 1)])
(define-metafunction t-CESK*
  [(alloc t) t])
(define reduce
  (reduction-relation
   t-CESK*
   (--> ((x p) s a t) ((lookup (lookup x p) s) s a (tick t)))
   (--> (((e_1 e_2) p) s a t) ((e_1 p) (insert (alloc t) (arg (e_2 p) a) s) (alloc t) (tick t)))
   (--> (vp ((a_1 z_1) ... (a_3 (arg c a_4)) (a_2 z_2) ...) a_3 t)
        ,(let ([s (term ((a_1 z_1) ... (a_3 (arg c a_4)) (a_2 z_2) ...))])
           (term (c (insert (alloc t) (fun vp a_4) ,s) (alloc t) (tick t)))))
   (--> (vp ((a_1 z_1) ... (a_3 (fun ((lmd x e) p) a_4)) (a_2 z_2) ...) a_3 t)
        ,(let ([s (term ((a_1 z_1) ... (a_3 (fun ((lmd x e) p) a_4)) (a_2 z_2) ...))])
           (term ((e (insert x (alloc t) p)) (insert (alloc t) vp ,s) a_4 (tick t)))))
   (--> (((op e_1 e_2 ...) p) s a t)
        ,(let ([cs (map (lambda (e) (list e (term p))) (term (e_2 ...)))])
           (term ((e_1 p) (insert (alloc t) (op () ,cs a) s) (alloc t) (tick t)))))
   (--> (vp_1 ((a_1 z_1) ... (a_3 (op (vp_2 ...) (c_1 c_2 ...) a_4)) (a_2 z_2) ...) a_3 t)
        ,(let ([s (term ((a_1 z_1) ... (a_3 (op (vp_2 ...) (c_1 c_2 ...) a_4)) (a_2 z_2) ...))])
           (term (c_1 (insert (alloc t) (op (vp_2 ... vp_1) (c_2 ...) a_4) ,s) (alloc t) (tick t)))))
   (--> ((v_1 p_1) ((a_1 z_1) ... (a_3 (op ((v_2 p_2) ...) () a_4)) (a_2 z_2) ...) a_3 t)
        ,(let ([s (term ((a_1 z_1) ... (a_3 (op ((v_2 p_2) ...) () a_4)) (a_2 z_2) ...))])
           (term (((dlt op v_2 ... v_1) ()) ,s a_4 (tick t)))))))
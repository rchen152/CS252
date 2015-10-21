#lang racket
(require redex)
(define-language et-CESK*
  (e x
     v
     (e e)
     (op e ...))
  (v (lmd x e)
     number)
  (x variable-not-otherwise-mentioned)
  (op * / + -)
  (c (e r a))
  (r ((x a) ...))
  (s ((a vk) ...))
  (k mt
     (ar c aa)
     (fn vr aa)
     (op (vr ...) (c ...) aa))
  (a number)
  (t number)
  (C (c ...))
  (vk vr k)
  (aa (a a))
  (vr (v r a)))
(define-metafunction et-CESK*
  [(inj e) ((e () 0) ((0 mt)) (0 0) 0 ())])
(define-metafunction et-CESK*
  [(dlt * number ...) ,(apply * (term (number ...)))]
  [(dlt / number ...) ,(apply / (term (number ...)))]
  [(dlt + number ...) ,(apply + (term (number ...)))]
  [(dlt - number ...) ,(apply - (term (number ...)))])
(define-metafunction et-CESK*
  [(lookup x r) ,(second (assoc (term x) (term r)))]
  [(lookup a s) ,(second (assoc (term a) (term s)))])
(define-metafunction et-CESK*
  [(insert x a r) ,(cons (term (x a)) (term r))]
  [(insert a vk s) ,(cons (term (a vk)) (term s))]
  [(insert (e r a) s C) ,(if (or (assoc (term a) (term s)) (member (term (e r a)) (term C))) (term C) (cons (term (e r a)) (term C)))]
  [(insert (c ...) s C) ,(foldl (lambda (elem acc) (term (insert ,elem s ,acc))) (term C) (term (c ...)))])
(define-metafunction et-CESK*
  [(tick t) ,(+ (term t) 1)])
(define-metafunction et-CESK*
  [(alloc t) t])
(define reduce
  (reduction-relation
   et-CESK*
   (--> ((x r a_c) s aa t C) ((lookup (lookup x r) s) s aa (tick t) C))
   (--> (((e_1 e_2) r a_c) s (a a_f) t C)
        ,(let* ([t_p (term (tick t))] [a_p (term (alloc ,t_p))])
           (term ((e_1 r a_f) (insert ,a_p (ar (e_2 r a_f) (a a_f)) s) (,a_p a_f) ,t_p (insert ((e_1 e_2) r a_c) s C)))))
   (--> (((op e_1 e_2 ...) r a_c) s (a a_f) t C)
        ,(let* ([t_p (term (tick t))] [a_p (term (alloc ,t_p))] [cs (map (lambda (e) (list e (term r) (term a_f))) (term (e_2 ...)))])
           (term ((e_1 r a_f) (insert ,a_p (op () ,cs (a a_f)) s) (,a_p a_f) ,t_p (insert ((op e_1 e_2 ...) r a_c) s C)))))
   (--> (vr ((a_1 vk_1) ... (a (ar c aa_pp)) (a_2 vk_2) ...) (a a_f) t C)
        ,(let* ([t_p (term (tick t))] [a_p (term (alloc ,t_p))] [s (term ((a_1 vk_1) ... (a (ar c aa_pp)) (a_2 vk_2) ...))])
           (term (c (insert ,a_p (fn vr aa_pp) ,s) (,a_p ,a_p) ,t_p C))))
   (--> (vr ((a_1 vk_1) ... (a (fn ((lmd x e) r a_c) aa_pp)) (a_2 vk_2) ...) (a a_f) t C)
        ,(let* ([t_p (term (tick t))] [a_p (term (alloc ,t_p))] [s (term ((a_1 vk_1) ... (a (fn ((lmd x e) r a_c) aa_pp)) (a_2 vk_2) ...))])
           (term ((e (insert x ,a_p r) a_f) (insert ,a_p vr ((a_1 vk_1) ... (a_2 vk_2) ...)) aa_pp ,t_p (insert ((lmd x e) r a_c) ,s C)))))
   (--> (vr ((a_1 vk_1) ... (a (op (vr_1 ...) (c_1 c_2 ...) aa_pp)) (a_2 vk_2) ...) (a a_f) t C)
        ,(let* ([t_p (term (tick t))] [a_p (term (alloc ,t_p))] [s (term ((a_1 vk_1) ... (a (op (vr_1 ...) (c_1 c_2 ...) aa_pp)) (a_2 vk_2) ...))])
           (term (c_1 (insert ,a_p (op (vr_1 ... vr) (c_2 ...) aa_pp) ,s) (,a_p a_f) ,t_p C))))
   (--> ((v r_n a_n) ((a_1 vk_1) ... (a (op ((v_1 r_n1 a_n1) ...) () aa_pp)) (a_2 vk_2) ...) (a a_f) t C)
        ,(let* ([t_p (term (tick t))] [a_p (term (alloc ,t_p))] [s (term ((a_1 vk_1) ... (a (op ((v_1 r_n1 a_n1) ...) () aa_pp)) (a_2 vk_2) ...))])
           (term (((dlt op v_1 ... v) () a_f) ,s aa_pp ,t_p (insert ((v_1 r_n1 a_n1) ... (v r_n a_n)) ,s C)))))))
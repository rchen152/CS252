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
  (c (e r bs))
  (r any)
  (s any)
  (k mt
     (ar c a)
     (fn vr a)
     (op (vr ...) (c ...) a))
  (a number)
  (t (bs number))
  (vk vr k)
  (vr (v r bs))
  (bs number)
  (C any))
(define-metafunction tCESK*
  [(empty) ,(hash)])
(define-metafunction tCESK*
  [(lookup any_key any_map) ,(hash-ref (term any_map) (term any_key))])
(define-metafunction tCESK*
  [(insert any_key any_val any_map) ,(hash-set (term any_map) (term any_key) (term any_val))])
(define-metafunction tCESK*
  [(inj e) ((e (empty) 1) (insert 0 mt (empty)) 0 (1 1) ,(set))])
(define-metafunction tCESK*
  [(dlt * number ...) ,(apply * (term (number ...)))]
  [(dlt / number ...) ,(apply / (term (number ...)))]
  [(dlt + number ...) ,(apply + (term (number ...)))]
  [(dlt - number ...) ,(apply - (term (number ...)))])
(define-metafunction tCESK*
  [(tick vr s a (bs number) C) (,(arithmetic-shift (term bs) 1) ,(+ (term number) 1)) (where (ar c a_pp) (lookup a s))]
  [(tick vr s a (bs number) C) (,(bitwise-ior (+ (term bs) 1) (term bs)) ,(+ (term number) 1)) (where (fn c a_pp) (lookup a s))]
  [(tick c s a (bs number) C) (bs ,(+ (term number) 1))])
(define-metafunction tCESK*
  [(alloc (bs number)) number])
(define-metafunction tCESK*
  [(check-add (v r bs_c) bs_t C)
   ,(if (< (term bs_c) (arithmetic-shift (term bs_t) (- (integer-length (term bs_c)) (integer-length (term bs_t)))))
        (set-add (term C) (term (v r bs_c))) (term C))]
  [(check-add c bs C) C]
  [(check-add (c ...) bs C) ,(foldl (lambda (elem acc) (term (check-add ,elem bs ,acc))) (term C) (term (c ...)))])
(define reduce
  (reduction-relation
   tCESK*
   (--> ((x r bs) s a t C) ((lookup (lookup x r) s) s a (tick (x r bs) s a t C) C) "1")
   (--> (((e_1 e_2) r bs_c) s a (bs_t number) C)
        ,(let ([t_p (term (tick ((e_1 e_2) r bs_c) s a (bs_t number) C))] [a_p (term (alloc (bs_t number)))])
           (term ((e_1 r bs_t) (insert ,a_p (ar (e_2 r bs_t) a) s) ,a_p ,t_p (check-add ((e_1 e_2) r bs_c) bs_t C)))) "2")
   (--> (((op e_1 e_2 ...) r bs_c) s a (bs_t number) C)
        ,(let ([t_p (term (tick ((op e_1 e_2 ...) r bs_c) s a (bs_t number) C))]
               [a_p (term (alloc (bs_t number)))] [cs (map (lambda (e) (list e (term r) (term bs_t))) (term (e_2 ...)))])
           (term ((e_1 r bs_t) (insert ,a_p (op () ,cs a) s) ,a_p ,t_p (check-add ((op e_1 e_2 ...) r bs_c) bs_t C)))) "3")
   (--> (vr s a t C)
        ,(let ([t_p (term (tick vr s a t C))] [a_p (term (alloc t))])
           (term (c (insert ,a_p (fn vr a_pp) s) ,a_p ,t_p C)))
        (where (ar c a_pp) (lookup a s)) "4")
   (--> (vr s a (bs_t number) C)
        ,(let ([t_p (term (tick vr s a (bs_t number) C))] [a_p (term (alloc (bs_t number)))])
           (term ((e (insert x ,a_p r) bs_t) (insert ,a_p vr s) a_pp ,t_p (check-add ((lmd x e) r bs_fn) bs_t C))))
        (where (fn ((lmd x e) r bs_fn) a_pp) (lookup a s)) "5")
   (--> (vr s a t C)
        ,(let ([t_p (term (tick vr s a t C))] [a_p (term (alloc t))])
           (term (c_1 (insert ,a_p (op (vr_1 ... vr) (c_2 ...) a_pp) s) ,a_p ,t_p C)))
        (where (op (vr_1 ...) (c_1 c_2 ...) a_pp) (lookup a s)) "6")
   (--> ((v r_n bs_c) s a (bs_t number) C)
        ,(let ([t_p (term (tick (v r_n bs_c) s a (bs_t number) C))] [a_p (term (alloc (bs_t number)))])
           (term (((dlt op v_1 ... v) (empty) bs_t) s a_pp ,t_p (check-add ((v_1 r_n1 bs_n1) ... (v r_n bs_c)) bs_t C))))
        (where (op ((v_1 r_n1 bs_n1) ...) () a_pp) (lookup a s)) "7")))

; EXAMPLES
; (traces reduce (term (inj (((lmd x ((lmd y (lmd z (+ x y z))) 12)) -6) -2))))
;  escapes: ((lmd z (+ x y z)) (x -> 4, y -> 7) 110)
; (traces reduce (term (inj (((lmd x (lmd y (+ -6 ((lmd z (+ x z)) y)))) 12) -2))))
;  escapes: ((lmd y (+ -6 ((lmd z (+ x z)) y))) (x -> 4) 10)
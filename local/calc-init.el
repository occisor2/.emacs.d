
;;; Definition stored by Calc on Sat Oct  9 14:29:39 2021
(put 'calc-define 'calc-calorim '(progn
 (defun calc-calorim nil (interactive) (calc-wrapper (calc-enter-result 7 "calo" (cons 'calcFunc-calorim (calc-top-list-n 7)))))
 (put 'calc-calorim 'calc-user-defn 't)
 (defun calcFunc-calorim (m1 c1 t1 m2 c2 t2 Tf) (math-normalize (list
  'calcFunc-eq (list '* m1 (list '* c1 (list '- Tf t1))) (list '* m2
  (list '* c2 (list '- Tf t2))))))
 (put 'calcFunc-calorim 'calc-user-defn '(calcFunc-eq (* (var m1
  var-m1) (* (var c1 var-c1) (- (var Tf var-Tf) (var t1 var-t1)))) (*
  (var m2 var-m2) (* (var c2 var-c2) (- (var Tf var-Tf) (var t2
  var-t2))))))
 (define-key calc-mode-map "zc" 'calc-calorim)
))

;;; Definition stored by Calc on Sun Oct 10 11:35:47 2021
(put 'calc-define 'calc-spheat '(progn
 (defun calc-spheat nil (interactive) (calc-wrapper (calc-enter-result 4 "sphe" (cons 'calcFunc-spheat (calc-top-list-n 4)))))
 (put 'calc-spheat 'calc-user-defn 't)
 (defun calcFunc-spheat (m c arg-t T) (math-normalize (list
  'calcFunc-eq '(var q var-q) (list '* c (list '* m (list '- T
  arg-t))))))
 (put 'calcFunc-spheat 'calc-user-defn '(calcFunc-eq (var q var-q) (*
  (var c var-c) (* (var m var-m) (- (var T var-T) (var t var-t))))))
 (define-key calc-mode-map "zq" 'calc-spheat)
))

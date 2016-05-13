;;; Nome: Felipe Vignoli Mathieu
;;; Nome: Erik Siebert

(defun concatenaLista (lista1 lista2)
	(if lista1
		(cons (car lista1) (concatenaLista (cdr lista1) lista2))
		lista2))

;;; Ex1

(defun maiorIterativo (lista n)
	(let ((m))
		(loop
			(if lista
				(if (> (car lista) n)
					(progn
						(setq m (concatenaLista m (list (car lista))))
						(setq lista (cdr lista)))
					(setq lista (cdr lista)))
				(return m)))))

(defun maiorRecursivo (lista n)
	(if lista
		(if (> (car lista) n)
			(cons (car lista) (maiorRecursivo (cdr lista) n))
			(maiorRecursivo (cdr lista) n))))

;;; Ex2

(defun mediaIterativa (lista)
	(let ((media) (count))
		(setq media 0)
		(setq count 0)
		(dolist (e lista media)
			(setq media (+ media e))
			(setq count (+ count 1)))
		(setq media (/ media count))
		media))

(defun mediaRecursiva (lista &optional &key (count 0) (media 0))
	(if lista
		(mediaRecursiva (cdr lista) :count (setq count (+ count 1)) :media (setq media (+ media (car lista))))
		(setq media (/ media count))))

;;; Ex3

(defun maiorMediaIterativa (lista)
	(let ((media) (count) (aux))
		(setq media 0)
		(setq count 0)
		;;;(setq lista2 lista)
		(dolist (e lista media)
			(setq media (+ media e))
			(setq count (+ count 1)))
		(setq media (/ media count))
		(print media)
		(loop
			(if lista
				(if (> (car lista) media)
					(progn 
						(setq aux (concatenaLista aux (list (car lista))))
						(setq lista (cdr lista)))
					(setq lista (cdr lista)))
				(return aux)))))

(defun maiorM_Recursiva (lista)
	(let ((media))
		(setq media (mediaRecursiva lista))
		(maiorMediaRecursiva lista :media media)))

(defun maiorMediaRecursiva (lista &optional &key (media 0))
	(if lista
		(if (> (car lista) media)
			(cons (car lista) (maiorMediaRecursiva (cdr lista) :media media))
			(maiorMediaRecursiva (cdr lista) :media media))))

;;; Ex4

(defun listaAntIterativo (lista p)
	(let ((count 1) (aux))
		(dolist(e lista aux)
			(if (< count p)
				(progn
					(setq aux (concatenaLista aux (list e)))
					(setq count (+ count 1)))
				(setq count (+ count 1))))))


(defun listaAntRecursiva (lista p &optional (count 1))
	(if lista
		(if (< count p)
			(cons (car lista) (listaAntRecursiva (cdr lista) p (setq count (+ count 1))))
			(listaAntRecursiva (cdr lista) p (setq count (+ count 1))))))

;;; Ex5

(defun listaPostIterativo (lista p)
	(let ((aux) (count 1))
		(dolist (e lista aux)
			(if (or (> count p) (= count p))
				(progn
					(setq aux (concatenaLista aux (list e)))
					(setq count (+ count 1)))
				(setq count (+ count 1))))))

(defun listaPostRecursiva (lista p &optional (count 1))
	(if lista
		(if (or (> count p) (= count p))
			(cons (car lista) (listaPostRecursiva (cdr lista) p (setq count (+ count 1))))
			(listaPostRecursiva (cdr lista) p (setq count (+ count 1))))))

;;; Ex6

(defun 1aN_Iterativo (n)
	(let ((aux) (count 1))
		(loop
			(if (< count n)
				(progn
					(setq aux (concatenaLista aux (list count)))
					(setq count (+ count 1))
				)
				(if (> count n)
					(return aux)
					(progn 
						(setq aux (concatenaLista aux (list n)))
						(return aux)))))))

(defun 1aN_Recursiva (n)
	(if (> n 0)
		(concatenaLista (1aN_Recursiva (- n 1)) (list n))))

;;; Ex7

(defun vizinhosIterativo (lista elemento)
	(let ( (vRight (cadr lista)) (vLeft nil) (aux) (count 1))
		(if lista
			(loop
				(if (and (= (car lista) elemento) (= count 1))
					(return (setq aux (concatenaLista (list vLeft) (list vRight))))
					(if (and (= (car lista) elemento) (> count 1))
						(return aux)))
				(progn
					(setq vLeft (car lista))
					(setq lista (cdr lista))
					(setq vRight (cadr lista))
					(setq count (+ count 1))
					(setq aux (concatenaLista (list vLeft) (list vRight))))))))

(defun vL (lista)
	(setq lista (car lista)))

(defun vR (lista)
	(setq lista (cadr lista)))

(defun vizinhosRecursivo (lista elemento &optional &key (vLeft nil) (vRight (cadr lista)))
	(if lista
		(if (not (= (car lista) elemento))
			(vizinhosRecursivo (cdr lista) elemento :vLeft (vL lista) :vRight (vR (cdr lista)))
			(concatenaLista (list vLeft) (list vRight)))))

;;; Ex8

(defun divideListaIterativa (lista p)
	(if lista
		(let ( (aux) (count 1) (listaAux) )
			(dolist (e lista aux)
				(if (and (= count p) (= count 1))
					(progn
						(setq listaAux (concatenaLista listaAux (list e)))
						(setq count (+ count 1)))
					(if (or (< count p) (= count p))
						(progn 
							(setq listaAux (concatenaLista listaAux (list e)))
							(setq count (+ count 1)))
						(progn
							(setq aux (concatenaLista aux (list e)))
							(setq count (+ count 1))))))
			(list listaAux aux))))

(defun divideListaRecursiva (lista p &optional &key (aux))
	(if (= p 0)
		(list aux lista)
		(divideListaRecursiva (cdr lista) (- p 1) :aux (concatenaLista aux (list (car lista))))))

;;;Ex9

(defun intervaloIterativo (n1 n2)
	(let ((aux))
		(loop 
			(if (or (< n1 n2) (= n1 n2))
				(progn 
					(setq aux (concatenaLista aux (list n1)))
					(setq n1 (+ n1 1)))
				(return aux)))))

(defun intervaloRecursivo (n1 n2)
	(if	(> n2 n1)
		(concatenaLista (intervaloRecursivo n1 (- n2 1)) (list n2))
		(concatenaLista '() (list n2))))

;;; Ex10

(defun listaTamanhoIterativo (lista)
	(let ((aux) (count 0))
		(dolist (e lista aux)
			(dolist (e1 e)
				(setq count (+ count 1)))
			(setq aux (concatenaLista aux (list count)))
			(setq count 0))))

(defun listaT (lista &optional (count 0))
	(if lista
		(listaT (cdr lista) (+ count 1))
		(list count)))

(defun listaTamanhoRecursivo (lista &optional aux)
	(if lista
		(listaTamanhoRecursivo (cdr lista) (concatenaLista aux (listaT (car lista))))
		aux))

;;; Ex11

(defun mergeIterativo (lista1 lista2)
	(let ((aux))
		(loop
			(if (not lista1)
				(return (concatenaLista aux lista2)))
			(if (not lista2)
				(return (concatenaLista aux lista1)))
			(if (< (car lista1) (car lista2))
				(progn
					(setq aux (concatenaLista aux (list (car lista1))))
					(setq lista1 (cdr lista1)))
				(progn
					(setq aux (concatenaLista aux (list (car lista2))))
					(setq lista2 (cdr lista2)))))))

(defun mergeRecursivo (lista1 lista2)
	(cond 
		((not lista1) lista2)
		((not lista2) lista1)
		((< (car lista1) (car lista2))
			(cons (car lista1) (mergeRecursivo (cdr lista1) lista2)))
		((cons (car lista2) (mergeRecursivo (cdr lista2) lista1)))))

;;; Ex12

(defun inverteIterativo (lista)
	(let ((aux))
		(dolist (e lista aux)
			(setq aux (cons e aux)))))

(defun inverteRecursivo (lista)
	(if (cdr lista)
		(concatenaLista  (inverteRecursivo (cdr lista)) (list (car lista)))
		lista))

;;; Ex13
(defun sublista (sublist list)
	(let (result (nelem 0))
		(dolist (esub sublist)
			(dolist (e list)
				(if (eq esub e)
					(setq nelem (+ 1 nelem)))))
		(setq result (eq nelem (length sublist)))))
(defun sublista2 (sublist list)
	(if sublist 
		(if (member (car sublist) list)
			(sublista2 (cdr sublist) list)
			nil)
		T))

;;; Ex14
 (defun sublista1 (sublist list)
	(let ((result NIL) (start NIL) (aux sublist))
		(dolist (e list)
			(cond ((eq (car sublist) e) (setq start T) (setq aux (cdr aux)))
				((and start (null aux)) (setq result T))
				((and start (eq (car aux) e)) (setq aux (cdr aux)))))
		(setq result result)))
;;; Ex15

(defun removeIterativo (lista elemento)
	(let ((aux) (lista2 lista))
		(loop
			(if lista
				(progn
					(if (= elemento (car lista))
						(progn
						(setq aux (concatenaLista aux (cdr lista)))
						(return aux)))
					(setq aux (concatenaLista aux (list (car lista))))
					(setq lista (cdr lista)))
				(return lista2)))))

(defun removeRecursivo (lista elemento &optional (lista2 nil))
	(if (not (equal (car lista) elemento))
		(removeRecursivo (cdr lista) elemento (concatenaLista lista2 (list (car lista))))
		(concatenaLista lista2 (cdr lista))))

;;; Ex16

(defun removeElementoIterativo (lista elemento)
	(let ((aux) (lista2))
		(loop
			(if lista
				(progn
					(if (equal elemento (car lista))
						(progn
							(setq lista2 (concatenaLista aux (cdr lista)))
							(setq lista (cdr lista)))
						(progn
							(setq aux (concatenaLista aux (list (car lista))))
							(setq lista (cdr lista)))))
				(return lista2)))))

(defun removeElementoRecursivo (lista elemento &optional (lista2 nil))
	(if lista
		(if (not (equal (car lista) elemento))
			(removeElementoRecursivo (cdr lista) elemento (concatenaLista lista2 (list (car lista))))
			(concatenaLista lista2 (removeElementoRecursivo (cdr lista) elemento)))
		lista2))

;;; Ex17

(defun removePosIterativo (lista p)
	(let ((aux) (count 1) (lista2))
		(loop
			(if lista
				(if (=  count p)
					(progn
						(setq lista2 (concatenaLista aux (cdr lista)))
						(return lista2))
					(progn
						(setq count (+ count 1))
						(setq aux (concatenaLista aux (list (car lista))))
						(setq lista (cdr lista))))
				(return "Informe posicao existente")))))

(defun removePosRecursivo (lista p &optional lista2)
	(if (= p 1)
		(concatenaLista lista2 (cdr lista))
		(removePosRecursivo (cdr lista) (- p 1) (concatenaLista lista2 (list (car lista))))))

;;; Ex18

(defun expressaoIterativo (e)
	(listp e))

;;; Ex19

(defun nivelaIterativo (lista)
	(let((aux))
		(dolist (e lista aux)
			(if (equal (listp e) nil)
				(setq aux (concatenaLista aux (list e)))))))

(defun nivelaRecursivo (lista &optional lista2)
	(if lista
		(if (equal (listp (car lista)) nil)
			(nivelaRecursivo (cdr lista) (concatenaLista lista2 (list (car lista))))
			(nivelaRecursivo (cdr lista) lista2))
		lista2))

;;; Ex20

(defun intersecaoIterativo (lista lista1)
	(let ((aux) (aux2 lista1))
		(dolist (e lista aux)
			(loop 
				(if lista1
					(if (equal e (car lista1))
						(progn
							(setq aux (concatenaLista aux (list e)))
							(return aux))
						(setq lista1 (cdr lista1)))
					(return aux)))
			(setq lista1 aux2))))
;;; Rever
(defun intersecaoRecursivo (lista lista1 &optional aux)
	(cond
		((null lista)
			aux)
		((null lista1)
			(intersecaoRecursivo (cdr lista) lista1))
		((equal (car lista) (car lista1))
			(setq aux(concatenaLista aux (car lista)))
			(intersecaoRecursivo (cdr lista) lista1))
		((intersecaoRecursivo lista (cdr lista1)))))

;;; Ex21

(defun retornaElemento (lista p)
	(let ((aux) (count 1))
		(loop
			(if lista
				(if (= count p)
					(progn
						(setq aux (concatenaLista aux (list (car lista))))
						(return aux))
					(progn
						(setq lista (cdr lista))
						(setq count (+ count 1))))
				(return aux)))))

(defun encontraElemPosIterativo (lista p)
	(let ((aux))
		(loop
			(if lista
				(if (equal (listp (car lista)) T)
					(progn
						(setq aux (concatenaLista aux (retornaElemento (car lista) p)))
						(setq lista (cdr lista)))
					(setq lista (cdr lista)))
				(return aux)))))

(defun encontraElemPosRecursivo (lista p &optional (aux nil))
	(if lista
		(if (equal (listp (car lista)) T)
			(encontraElemPosRecursivo (cdr lista) p (concatenaLista aux (retornaElemento (car lista) p)))
			(encontraElemPosRecursivo (cdr lista) p aux))
		aux))

;;; Ex22
(defun empacota_copias (list)
	(let (lista (atual (car list)) (times 0) aux)
		(dolist (e list lista)
			(cond ((eq e atual) (setq times (+ 1 times)))
				((not (eq e atual)) (dotimes (i times) (setq aux (apend aux atual))) (setq lista (apend lista aux)) (setq atual e) (setq times 1) (setq aux NIL))))
		(dotimes (i times) (setq aux (apend aux atual))) 
		(setq lista (apend lista aux))))
;;; Ex23
(defun codifica (list)
	(let (lista (atual (car list)) (times 0))
		(dolist (e list lista)
			(cond ((eq e atual) (setq times (+ 1 times)))
				((not (eq e atual)) (setq lista (apend lista (list atual times))) (setq atual e) (setq times 1))))
		(setq lista (apend lista (list atual times)))))
;;; Ex24
(defun decodifica (list)
	(let (lista)
		(dolist (e list lista)
			(dotimes (i (cadr e)) (setq lista (apend lista (car e)))))))
;;; Ex25
(defun replica (list times)
	(let (lista)
		(dolist (e list lista)
			(dotimes (i times) (setq lista (apend lista e))))))
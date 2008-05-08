;; p.8
;; Doc 1: I did enact Julius Caesar: I was killed i' the Capitol; Brutus killed me.
;; Doc 2: So let it be with Caesar. The noble Brutus hath told you Caesar was ambitious:
(use srfi-1)
(use srfi-13)

(define (normalize term) ;; とりあえず "I" 以外を小文字化
  (if (string=? "I" term)
	  term
	  (string-downcase term)))

;;
;; Step 1. Collect the document to be indexed
;;
(define (add-docID docs)
;; ((I did enact ...) (So let it be ...))
;; → ((1 (I did enact ...) (2 (So let it be ...))
  (zip (iota (length docs) 1) docs))

;;
;; Step 2. Tokenize the text, turning each document into a list of tokens
;;
(define (tokenize docs-with-docID)
;; ((1 (I did ...) (2 (So let ...))
;; → ((I . 1) (did . 1)  ... (So . 2) (let . 2) ... )
  (append-map (lambda (id-and-doc) (map (lambda (token)
										  (cons (symbol->string token) (first id-and-doc)))
										(second id-and-doc)))
			  docs-with-docID))
;;
;; Step 3. Do linguistic preprocessing, producing a list of normalized tokens, which
;;         are the indexing terms
;;
(define (sort-terms-alphabetically terms)
;; ((I . 1) (did . 1)  ... (So . 2) (let . 2) ... )
;; → ((ambitious . 2) (be . 2) (brutus . 1) (brutus . 2) ... (was 1) (was 2) (with 2))
  (let1 normalized-terms (map (lambda (p) (cons (normalize (car p)) (cdr p))) terms)
	(stable-sort normalized-terms (lambda (x y) (string-ci<? (car x) (car y))))))
;; sortではなくstable-sortにしている。比較関数を与えた場合現行のGaucheではどちらもmerge sortだが、それが保証されているのはstable-sortのみ。docID順に並んでいることを保証することで、後のpostings生成が楽になる

;;
;; Step 4. Index the documents that each term occurs in by creating an inverted index,
;;         consisting of a dictionary and postings.
;;
(define (make-dictionary-and-postings-list sorted-terms)
;; ((ambitious . 2) (be . 2) (brutus . 1) (brutus . 2) ... (was 1) (was 2) (with 2))
;; → ((ambitious 1 (2)) (be 1 (2)) (brutus 2 (1 2)) ... )
  (let loop ([terms sorted-terms]
			 [term ""] ; 現在処理中のterm
			 [freq -1] ; 現在のtermの出現回数
			 [postings '()] ; postings
			 [result '()])

	; result に 現在処理中のterm を加えたもの。最後にreverseしてね
	(define (add-current-term-to-result)
	  (cons (list term freq (reverse! postings)) result))

	(if (null? terms)
		(cdr (reverse! (add-current-term-to-result)))
		(let1 term-and-docID (car terms)
		  (let ([curr-term (car term-and-docID)]
				[docID (cdr term-and-docID)])
			(if (string=? curr-term term)
				;; same term
				(let1 second-appearance-or-later? (= docID (car postings))
				  (loop (cdr terms)
						term
						;; (+ freq 1) ; これだと出現ドキュメント数じゃなくて出現総数
						(if second-appearance-or-later?
							freq
							(+ freq 1))
						(if second-appearance-or-later?
							postings
							(cons docID postings))
						result))
				;; different term
				(loop (cdr terms)
					  curr-term
					  1
					  (list docID)
					  (add-current-term-to-result)
					  )))))))

(define docs
 '((I did enact julius caesar I was killed |i'| the capitol brutus killed me)
  (So let it be with caesar the noble brutus hath told you caesar was ambitious)))

(map (lambda (r) (format #t "~s x ~d  →  ~a\n" (first r) (second r) (third r)))
 (make-dictionary-and-postings-list
  (sort-terms-alphabetically
   (tokenize
	(add-docID docs)
	))))

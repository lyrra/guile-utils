;;;;
;;;;  20230623 larry valkama: yet another test framework
;;;;    why; primarily because srfi-64 isn't a functional one, but macro based.
;;;;    This suite gives test primitives needed to make dynamically generated tests

;;;;  Example:
;;;;
;;;;    (define (test-softmax)
;;;;      (test-begin "softmax")
;;;;      (test-setup (lambda () 
;;;;                    '(setup-for-subtests)
;;;;                    (set! *random-state* 0)))
;;;;      ; run some sub-test
;;;;      (run-test "softmax-1" (lambda ()
;;;;                              (test-assert (equal? (random 2) 0)
;;;;                                           "randomly fail")))
;;;;      (test-end "softmax"))
;;;;
;;;;    (let ()
;;;;      (test-verbose #f)
;;;;      (test-error-stop #f)
;;;;      (test-record-clear)
;;;;      (test-begin "ml")
;;;;      (test-setup (lambda () 
;;;;                    '(setup-for-tests)
;;;;                    (set! %prandom-seed 0)))
;;;;      ; run some test
;;;;      (run-test "softmax" test-softmax)
;;;;      (test-end "ml")
;;;;      (display (test-report)))

;;;; A thin macro layer could be built ontop of these functional primitives:
;;;;  (define-syntax deftest
;;;;    (syntax-rules ()
;;;;      ((_ name . body)
;;;;       (run-test name (lambda () (begin . body))))))

;;;; Example usage of macro, placing tests into nice subgroups
;;;;
;;;; (deftest 'softmax-tests
;;;;   (deftest 'softmax
;;;;     ...)
;;;;   (deftest 'softmax-loss
;;;;     ...)
;;;;   )

(define-module (guile-utils testsuite)
  #:export (test-verbose
            test-error-stop
            test-record-clear
            test-report
            test-get-current-test
            test-begin
            test-end
            test-assert
            test-setup
            run-test))

(define %test-verbose #f)
(define (test-verbose b)
  (set! %test-verbose b))
(define %test-error-stop #f)
(define (test-error-stop b)
  (set! %test-error-stop b))

(define %test-stack '())
(define (test-begin name)
  (set! %test-stack
        (cons (list name '()) %test-stack)))
(define (test-end . args)
  (set! %test-stack (cdr %test-stack)))

(define %test-record '())
(define (test-record-clear)
  (set! %test-record '()))
(define (test-report)
  %test-record)

(define (test-setup thunk)
  (let* ((test-frame-thunks (cdr (car %test-stack)))
         (new (append (car test-frame-thunks) (list thunk))))
    (set-car! test-frame-thunks new)))

(define (test-record-add msg)
  (set! %test-record (cons (list (caar %test-stack) msg) %test-record)))

(define (test-get-current-test)
  (caar %test-stack))

(define (run-test name fun)
  (let ((test-frame-thunks (cadr (car %test-stack))))
    (for-each (lambda (thunk)
                (thunk))
              test-frame-thunks))
  (test-begin name)
  (fun)
  (test-end name))

(define (test-path-name)
  (let ((str ""))
    (for-each (lambda (frame)
                (let ((frame-name (car frame)))
                  (set! str (string-concatenate (list str "/"
                                                      (format #f "~a" frame-name))))))
              (reverse %test-stack))
    str))

(define (test-assert expr . errmsgexpr)
  (unless expr
    (do ((p errmsgexpr (cdr p)))
        ((null? p))
      (if (not (null? (car p)))
          (let ((msg (format #f "==ERROR (~a): ~s"
                             (test-path-name) (car p))))
            (when (or %test-verbose %test-error-stop) (display msg) (newline))
            (test-record-add msg))))
    (if %test-error-stop
        (error 'test-assert-error))))

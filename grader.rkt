#lang racket

(require racket/sandbox)
(require racket/control)
(require sxml)

(provide
  define-rubric
  all-or-nothing
  weighted-percentage
  assert
  find-rec
  restart
  with-student-module
  with-grade-report
  test-suite
  current-evaluator
  current-module-code
  current-grade-report
  current-rubric-item
  student-function
  gen-integers
  gen-lists
  gen-union
  gen-sorted-integer-lists
  for/cases
  grade-report%
  (all-from-out racket/sandbox)
  (all-from-out racket/control)
  (except-out (all-from-out racket) #%module-begin)
  (rename-out [module-begin #%module-begin]))

(struct assertion-result (success
                          weight
                          desc
                          datum
                          exn))

(define (all-or-nothing results)
  (if (andmap assertion-result-success results)
    1
    0))

(define (weighted-percentage results)
  (define (f result acc)
    (match acc
      [(list correct total)
       (let ([weight (assertion-result-weight result)])
         (list
           (if (assertion-result-success result)
             (+ weight correct)
             correct)
           (+ weight total)))]))
  (apply / (foldl f '(0 0) results)))

(struct rubric-item (id
                     description
                     points
                     grading-function))

(define-syntax-rule (define-rubric rubric-name [id itemspec ...] ...)
  (begin
    (define id (one-rubric-item id itemspec ...)) ...
    (define rubric-name (list id ...))))

(define-syntax one-rubric-item
  (syntax-rules ()
    [(_ id desc points)
     (rubric-item 'id desc points weighted-percentage)]
    [(_ id desc points func)
     (rubric-item 'id desc points func)]))

(define (eval/timeout timeout fail-thunk datum namespace)
  (define channel (make-channel))
  (define th (thread (λ ()
                        (channel-put
                          channel
                          (with-handlers
                            ([exn:fail? (λ (e) `(exn ,e))])
                            `(success ,(eval datum namespace)))))))
  (match (sync/timeout timeout channel)
    [#f (begin
          (break-thread th)
          (fail-thunk))]
    [`(exn ,e) (raise e)]
    [`(success ,r) r]))

(define (total-points-possible rubric)
  (apply + (map rubric-item-points rubric)))

(define grade-report%
  (class object%
    (init in-rubric in-student-info)
    (define rubric in-rubric)
    (define student-info in-student-info)
    (define assertion-bank (make-hash))
    (for ([item rubric])
      (hash-set! assertion-bank item '()))
    (super-new)

    (define/public (add-result item result)
      (hash-set! assertion-bank item
                 (cons result (hash-ref assertion-bank item))))

    (define/public (assert item datum namespace [weight 1] [timeout 0.5])
      (match* ((rubric-item-grading-function item)
               (hash-ref assertion-bank item))
        [(all-or-nothing
          (list-rest (struct assertion-result (#f _ _ _ _)) _))
         ;; special case: all-or-nothing grading and failed assertion exists
         (void)]
        [(_ _)
         (add-result
           item
           (with-handlers
             ([exn:fail? (λ (e)
                            (assertion-result
                              #f weight "Exception encountered!" datum e))])
             (let ([eval-result (eval/timeout
                                  timeout (λ () 'timeout)
                                  datum namespace)])
               (case eval-result
                 [(#f) (assertion-result #f weight "Assertion failure!" datum #f)]
                 [(timeout) (assertion-result
                              #f
                              weight
                              (format "Timeout! Killed after ~A seconds." timeout)
                              datum
                              #f)]
                 [else (assertion-result eval-result weight #f datum #f)]))))]))

    (define/public (points-earned item)
      (let ([results (hash-ref assertion-bank item)])
        (if (null? results)
          (error (rubric-item-id item)
                 "received no assertions")
          (* ((rubric-item-grading-function item) results)
             (rubric-item-points item)))))

    (define/public (total-points-earned)
      (apply + (map (λ (item) (points-earned item)) rubric)))

    (define/public (get-notes)
      (append-map
        (λ (results) (filter assertion-result-desc results))
        (map (curry hash-ref assertion-bank) rubric)))

    (define/public (get-sxml-report)
      (list*
        `(p (b "Student Information:")
            (tt ,(format "~A" student-info)))
        `(table
           (tr (th "Rubric Category")
               (th "Points Earned")
               (th "Points Possible"))
           ,@(for/list ([item rubric])
               `(tr (td ,(rubric-item-description item))
                    (td ,(points-earned item))
                    (td ,(rubric-item-points item))))
           (tr (th "TOTAL")
               (th ,(total-points-earned))
               (th ,(total-points-possible rubric))))
        (let ([notes (get-notes)])
          (if (null? notes)
            '()
            (list '(h2 "Details Available")
                  `(ul ,@(for/list ([result notes])
                           (let ([desc (assertion-result-desc result)]
                                 [datum (assertion-result-datum result)]
                                 [exn (assertion-result-exn result)])
                             `(li
                                ,desc
                                (pre ,(format
                                        "~A~%~A"
                                        (if datum
                                          (format "ASSERTION: ~V" datum)
                                          "")
                                        (if exn
                                          (format "EXCEPTION: ~A"
                                                  (exn-message exn))
                                          ""))))))))))))))

(define current-evaluator (make-parameter
                            (make-evaluator 'racket/base)))
(define current-module-code (make-parameter #f))
(define current-grade-report (make-parameter #f))
(define current-rubric-item (make-parameter #f))

(define-syntax-rule (test-suite item body ...)
  (parameterize ([current-rubric-item item])
    body ...))

(define-syntax assert
  (syntax-rules (for)
    [(_ for rubric-item datum args ...)
     (let ([ns (current-namespace)])
       (send (current-grade-report)
             assert rubric-item datum ns args ...))]
    [(_ datum args ...)
     (assert for (current-rubric-item) datum args ...)]))

(define-syntax-rule (find-rec pattern tree body ...)
  (let ()
    (define (find-pattern elem)
      (match elem
        [pattern body ...]
        [(cons head tail)
         (or (find-pattern head)
             (find-pattern tail))]
        [_ #f]))
    (find-pattern tree)))

(define (continuation-menu try desc exn)
  (eprintf "STOP! An exception occured while ~A:~%~%~A~%~%"
           desc (exn-message exn))
  (let menu ()
    (eprintf "What would you like to do?~%~%")
    (eprintf "1. Drop to a shell to try and resolve the problem~%")
    (eprintf "2. Specify a return value for the situation~%")
    (eprintf "3. Give up and exit the program~%")
    (eprintf "4. Try again~%~%")
    (eprintf "Your choice: ")
    (case (read)
      [(1) (system "bash")
           (menu)]
      [(2) (restart "reading a replacement value"
             (eprintf "eval: ")
             (eval (read)
                   (make-base-namespace)))]
      [(3) (exit 1)]
      [(4) (try)])))

(define-syntax-rule (restart desc body ...)
  (let ()
    (define (try)
      (with-handlers ([exn:fail?
                        (lambda (e)
                          (continuation-menu try desc e))])
        body ...))
    (try)))

(define-syntax-rule (with-student-module filename language body ...)
  (let ([fn filename]
        [lang language])
    (let ([read-code (restart "loading student code"
                       (syntax->datum
                         (call-with-input-file
                           fn
                           (lambda (f)
                             (parameterize ([read-accept-reader #t])
                               (read-syntax fn f))))))])
      (restart "creating student code module"
        (let ([code-body (find-rec (list-rest '#%module-begin cbody)
                                   read-code
                                   cbody)])
          (unless code-body
            (error "Could not locate #%module-begin"))
          (parameterize ([current-evaluator
                           (apply make-evaluator lang code-body)]
                         [current-module-code code-body])
            body ...))))))

(define-syntax-rule (with-grade-report rubric student-info body ...)
  (parameterize ([current-grade-report
                   (make-object grade-report% rubric student-info)])
    body ...))

(define (student-function name)
  (restart (format "loading function ~A" name)
    ((current-evaluator) name)))

(define-syntax-rule (prog1 body1 body ...)
  (let ([result body1])
    body ...
    result))

(define (gen-integers [min -2147483543] [max 2147483543])
  (define state 'init)
  (define (gen)
    (define (try-gen num next)
      (set! state next)
      (if (and (>= num min)
               (< num max))
        num
        (gen)))
    (case state
      [(init) (try-gen 0 'z)]
      [(z) (try-gen 1 'o)]
      [(o) (try-gen -1 'r)]
      [(r) (random min max)]))
  gen)

(define (gen-lists generator [min-length 0] [max-length 300])
  (lambda ()
    (if (> min-length max-length)
      'stop
      (prog1
        (prompt
          (build-list min-length (λ (x)
                                    (let ([elem (generator)])
                                      (case elem
                                        [(stop) (abort 'stop)]
                                        [else elem])))))
        (set! min-length (add1 min-length))))))

(define (gen-union . gens)
  (define next-gens gens)
  (define (gen)
    (cond
      [(null? gens) 'stop]
      [(null? next-gens) (set! next-gens gens)
                         (gen)]
      [else
        (let ([result ((car next-gens))])
          (case result
            [(stop) (set! gens (remq (car next-gens) gens))
                    (set! next-gens (cdr next-gens))
                    (gen)]
            [else (when (< (random) 0.5)
                    (set! next-gens (cdr next-gens)))
                  result]))]))
  gen)

(define (gen-sorted-integer-lists
          [min-start -2147483543] [max-start 2147483543]
          [min-inc 1] [max-inc 2147483543]
          [min-length 0] [max-length 300])
  (lambda ()
    (define val (random min-start max-start))
    (build-list
      (random min-length (add1 max-length))
      (lambda (x)
        (prog1
          val
          (set! val (+ val (random min-inc max-inc))))))))

(define-syntax-rule (for/cases ncases ([name gen ...] ...) body ...)
  (let ([gens (list (begin gen ...) ...)])
    (let next ([n ncases])
      (when (> n 0)
        (apply (lambda (name ...)
                 (unless (member 'stop (list name ...))
                   body ...
                   (next (sub1 n))))
               (map (λ (g) (g)) gens))))))

(define-syntax module-begin
  (syntax-rules (define-rubric using-language)
    [(_ (define-rubric
          rubric-name
          rubric-items ...)
        (using-language language)
        test-cases ...)
     (#%plain-module-begin
      (define-namespace-anchor anchor)
      (current-namespace (namespace-anchor->namespace anchor))
      (define-rubric rubric-name rubric-items ...)
      (define (grade student-code student-info port)
        (with-grade-report rubric-name student-info
          (with-student-module student-code language
            test-cases ...)
          (fprintf port "Grade~%~%")
          (map (λ (e) (fprintf port "~A~%" (srl:sxml->xml e)))
               (send (current-grade-report) get-sxml-report))))
      (module* main #f
        (void
          (command-line
            #:args (student-code student-info)
            (grade student-code student-info (current-output-port))))))]))

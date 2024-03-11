; Handmade Brainf*ck Interpreter w/ Chicken Scheme
(import
  (chicken io)              ;; call-with-input-file
  (chicken process-context) ;; command-line-arguments
  (chicken string)          ;; string-compare3
  (srfi-1)                  ;; take, drop
  (srfi-18)                 ;; thread-sleep!
  (srfi-48))                ;; format

; file name -> list of bf chars
(define file2chars
  (lambda (file_name)
    (call-with-input-file
      file_name
      (lambda (input-file)
        (let loop ([line (read-line input-file)]
                   [chars '()])
          (if (eof-object? line)
              chars
              (loop (read-line input-file)
                    (append chars (string->list line)))))))))

; Usage:
;   -c COMMANDS   read and execute the string as Brainf*ck code
;   -w SECOND(S)  wait and dump per command
;   FILE_NAME     open the source file and execute it

(define arguments (command-line-arguments))

(define bf
  (let loop ([left arguments])
    (if (= (length left) 1)
        (file2chars (car left))
        (if (zero? (string-compare3 "-c" (car left)))
            (string->list (list-ref left 1))
            (loop (cdr left))))))

(define wait
  (let loop ([left arguments])
    (if (null? left)
        0
        (if (zero? (string-compare3 "-w" (car left)))
            (string->number (list-ref left 1))
            (loop (cdr left))))))

; adjust the value to 1 byte
(define overflow
  (lambda (x)
    (cond ((< 255 x) 0)
          ((< x 0) 255)
          (else x))))

; operate "+" "-" "," and return as list which length is 1
(define operate_value
  (lambda (value inst)
    (list (overflow (cond ((char=? inst #\+)
                           (+ value 1))
                          ((char=? inst #\-)
                           (- value 1))
                          ((char=? inst #\,)
                           (char->integer (read-char (current-input-port)))))))))

; assemble new memory state
(define insert_value
  (lambda (old_mem ptr value) ;; "value" expects one-length list
    (let loop ([new_mem '()])
      (if (= (length old_mem) (length new_mem))
          new_mem
          (loop (if (= ptr (length new_mem))
                    (append new_mem value)
                    (append new_mem 
                            (list (list-ref old_mem (length new_mem))))))))))

; return memory for next iteration
(define new_memory
  (lambda (memory data_ptr inst)
    (if (not (or (char=? inst #\+)
                 (or (char=? inst #\-)
                     (char=? inst #\,))))
        memory                            ;; not above, do nothing
        (if (< data_ptr (length memory)) ;; data_ptr MUST be positive
            (insert_value memory
                          data_ptr
                          (operate_value (list-ref memory data_ptr)
                                         inst))
        (new_memory (append memory '(0)) ;; reallocate and retry
                    data_ptr
                    inst)))))

; ">" or "<" -> reference next / previous address
(define new_data_ptr
  (lambda (data_ptr inst)
    (cond ((char=? inst #\>)
           (+ data_ptr 1))
          ((char=? inst #\<)
           (- data_ptr 1))
          (else data_ptr))))

; are []s nesting...?
(define new_nest_count
  (lambda (ptr nest sign)
    (cond
      ((char=? #\[ (list-ref bf ptr))
       (if (= sign 1)
           (+ nest 1)
           (- nest 1))) ;; while don't exit "seek" loop, MUST be >= 1
      ((char=? #\] (list-ref bf ptr))
       (if (= sign -1)
           (+ nest 1)
           (- nest 1)))
      (else nest))))

; where are []s...?
(define seek
  (lambda (inst_ptr toward)
    (let loop ([ptr (+ inst_ptr toward)]
               [nest 0]
               [sign toward]) ;; 1 or -1
      (cond
        ((or (< ptr 0) (<= (length bf) ptr))
         -1) ;; ERROR STOP
        ((and (char=? #\] (list-ref bf ptr))
              (and (= sign 1) (zero? nest)))
         ptr)
        ((and (char=? #\[ (list-ref bf ptr))
              (and (= sign -1) (zero? nest)))
         ptr)
        (else (loop (+ ptr sign)
                    (new_nest_count ptr nest sign)
                    sign))))))

; [ or ] -> seek and jump
; other  -> show next command
(define new_inst_ptr
  (lambda (ptr ref_ed) ;; data pointer, current command
    (cond
      ((char=? #\[ (list-ref bf ptr))
       (if (zero? ref_ed)
           (+ (seek ptr 1) 1)
           (+ ptr 1)))
      ((char=? #\] (list-ref bf ptr))
       (if (zero? ref_ed)
           (+ ptr 1)
           (+ (seek ptr -1) 1)))
      (else (+ ptr 1)))))

; interrupt to running, do something
; "." -> output
(define interrupt
  (lambda (memory data_ptr inst_ptr step)
    (cond
      ((char=? #\. (list-ref bf inst_ptr))
       (display (integer->char (list-ref memory data_ptr))))
      ((not (zero? wait))
       (begin
         (thread-sleep! wait)
         (dump memory data_ptr inst_ptr step)))
      (else 0))))

; dump to STDERR and return exit status
(define dump
  (lambda (memory data_ptr inst_ptr step)
    (begin
      (format (current-error-port)
              "\nStep ~a -- Instructon pointer: ~a, Data pointer: ~a\n~a\n\n"
              step inst_ptr data_ptr memory)
      (cond
        ((<= data_ptr -1) 1)
        (else 0)))))

; tail call version
(define interpreter
  (lambda (memory data_ptr inst_ptr step)
    (cond
      ((<= (length bf) inst_ptr) ;; reach end of the bf code
       (dump memory data_ptr inst_ptr step))
      ((or (<= data_ptr -1) (<= inst_ptr -1)) ;; minus one address...?
       (dump memory data_ptr inst_ptr step))
      (else 
        (begin
          (interrupt memory data_ptr inst_ptr step)
          (interpreter (new_memory memory
                                   data_ptr
                                   (list-ref bf inst_ptr))
                       (new_data_ptr data_ptr
                                     (list-ref bf inst_ptr))
                       (new_inst_ptr inst_ptr
                                     (if (< data_ptr (length memory))
                                         (list-ref memory data_ptr)
                                         0))
                       (+ step 1)))))))

(define main
  (lambda ()
    (interpreter '(0) ;; memory
                 0    ;; Data pointer
                 0    ;; Instructon pointer
                 0))) ;; Step count
(main)



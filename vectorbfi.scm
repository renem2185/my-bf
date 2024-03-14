; Handmade Brainf*ck Interpreter w/ Chicken Scheme

; Usage:
;   -c COMMANDS   read and execute the string as Brainf*ck code
;   -w SECOND(S)  wait and dump per command
;   FILE_NAME     open the source file and execute it

(import
  (chicken io) ;; call-with-input-file -- for Gauche, comment out here
  (srfi-4)     ;; u8vector
  (srfi-13)    ;; string=
  (srfi-18)    ;; thread-sleep!
  (srfi-48)    ;; format
  (srfi-193))  ;; command-args

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

;; list of characters -> vector of 8bit unsigned integer
(define chars2u8vec
  (lambda (chars)
    (list->u8vector (map char->integer chars))))

; Brainf*ck code as u8vector
(define bf
  (delay
    (let loop ([left (command-args)])
      (if (= (length left) 1)
        (chars2u8vec (file2chars (car left)))
        (if (string= "-c" (car left))
          (chars2u8vec (string->list (list-ref left 1)))
          (loop (cdr left)))))))

; wait time [seconds] for debug...?
(define wait
  (delay
    (let loop ([left (command-args)])
      (if (null? left)
        0
        (if (string= "-w" (car left))
          (string->number (list-ref left 1))
          (loop (cdr left)))))))

; adjust the value to 1 byte
(define overflow
  (lambda (x)
    (cond ((< 255 x) 0)
          ((< x 0) 255)
          (else x))))

; operate "+" "-" ","
(define operate2value
  (lambda (value inst)
    (overflow (cond ((char=? inst #\+)
                     (+ value 1))
                    ((char=? inst #\-)
                     (- value 1))
                    ((char=? inst #\,)
                     (char->integer (read-char (current-input-port))))))))

; assemble new memory state (hiding side effect!)
(define insert_value
  (lambda (old_mem   ;; old memory before processing
            ptr      ;; data pointer
            value    ;; processed value by "operate2value"
            new_len) ;; new length of memory 
    (let loop ([new_mem (make-u8vector new_len 0)]
               [count 0])
      (if (<= new_len count)
          new_mem
          (loop (if (<= (u8vector-length old_mem) count)
                    new_mem
                    (if (= count ptr) ;; DANGER!!!
                        (begin
                          (u8vector-set! new_mem count value)
                          new_mem)
                        (begin
                          (u8vector-set! new_mem
                                         count 
                                         (u8vector-ref old_mem
                                                       count))
                          new_mem)))
                (+ count 1))))))

; return memory for next iteration
(define new_memory
  (lambda (memory data_ptr inst)
    (if (not (or (char=? inst #\+)
                 (or (char=? inst #\-)
                     (char=? inst #\,))))
        memory                            ;; not above, do nothing
        (if (< data_ptr (u8vector-length memory)) 
            (insert_value memory
                          data_ptr
                          (operate2value (u8vector-ref memory data_ptr)
                                         inst)
                          (u8vector-length memory))
        (new_memory (insert_value memory          ;; reallocate and retry
                                  data_ptr
                                  0               ;; should be zero
                                  (+ data_ptr 1))
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
  (lambda (inst nest sign)
    (cond
      ((char=? inst #\[)
       (if (= sign 1)
           (+ nest 1)
           (- nest 1))) ;; while don't exit "seek" loop, MUST be >= 1
      ((char=? inst #\])
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
        ((or (< ptr 0) (<= (u8vector-length (force bf)) ptr))
         -1) ;; ERROR STOP
        ((and (= 93 (u8vector-ref (force bf) ptr)) ;; ]
              (and (= sign 1) (zero? nest)))
         ptr)
        ((and (= 91 (u8vector-ref (force bf) ptr)) ;; [
              (and (= sign -1) (zero? nest)))
         ptr)
        (else (loop (+ ptr sign)
                    (new_nest_count (integer->char (u8vector-ref (force bf) ptr))
                                    nest
                                    sign)
                    sign))))))

; [ or ] -> seek and jump
; other  -> show next command
(define new_inst_ptr
  (lambda (ptr ref_ed) ;; data pointer, current command
    (cond
      ((= 91 (u8vector-ref (force bf) ptr)) ;; [
       (if (zero? ref_ed)
           (+ (seek ptr 1) 1)
           (+ ptr 1)))
      ((= 93 (u8vector-ref (force bf) ptr)) ;; ]
       (if (zero? ref_ed)
           (+ ptr 1)
           (+ (seek ptr -1) 1)))
      (else (+ ptr 1)))))

; interrupt to running, do something
; "." -> output
(define interrupt
  (lambda (memory data_ptr inst_ptr step) ;; state of vm
    (cond
      ((= 46 (u8vector-ref (force bf) inst_ptr)) ;; .
       (display (integer->char (u8vector-ref memory data_ptr))))
      ((not (zero? (force wait)))
       (begin
         (thread-sleep! (force wait))
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
      ((<= (u8vector-length (force bf)) inst_ptr) ;; reach end of the bf code
       (dump memory data_ptr inst_ptr step))
      ((or (<= data_ptr -1) (<= inst_ptr -1)) ;; minus one address...?
       (dump memory data_ptr inst_ptr step))
      (else 
        (begin
          (interrupt memory data_ptr inst_ptr step)
          (interpreter (new_memory memory
                                   data_ptr
                                   (integer->char (u8vector-ref (force bf)
                                                                inst_ptr)))
                       (new_data_ptr data_ptr
                                     (integer->char (u8vector-ref (force bf) 
                                                                  inst_ptr)))
                       (new_inst_ptr inst_ptr
                                     (if (< data_ptr (u8vector-length memory))
                                         (u8vector-ref memory data_ptr)
                                         0)) ;; should be zero
                       (+ step 1)))))))

(define main
  (lambda ()
    (interpreter (u8vector 0) ;; memory
                 0            ;; Data pointer
                 0            ;; Instructon pointer
                 0)))         ;; Step count
(main)



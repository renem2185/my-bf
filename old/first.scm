; Handmade Brainf*ck Interpreter w/ Chicken Scheme
(import
  (chicken io)              ; call-with-input-file
  (chicken process-context) ; command-line-arguments
  (chicken string)          ; string-compare3
  (srfi-48)   ; Intermediate Format Strings
  (srfi-133)) ; Vector Library (R7RS-compatible)

; loading...
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

(define arguments (command-line-arguments))
(define bf (if (zero? (string-compare3 "-c" (car arguments)))
               (string->list (list-ref arguments 1))
               (file2chars (car arguments))))

; assemble memory when `+` `-` `,`
(define update_memory
  (lambda (memory ptr rewrite)
    (let ([ref_ed (vector-ref memory ptr)])
      (if (= (vector-length memory) 1) ;; rare case
          (vector rewrite)
          (cond
            ((= ptr 0) ;; edge
             (vector-append (vector rewrite)
                            (vector-copy memory (+ ptr 1))))
            ((= ptr 
                (- (vector-length memory) 1))
             (vector-append (vector-copy memory 0 ptr)
                            (vector rewrite)))
            (else
              (vector-append (vector-copy memory 0 ptr)
                             (vector rewrite)
                             (vector-copy memory (+ ptr 1)))))))))

; where are []...?
(define seek
  (lambda (inst_ptr toward)
    (let loop ([count 1] [ptr (+ inst_ptr toward)] [sign toward])
      (cond
        ((or (and (= sign  1)
                  (char=? #\[ (list-ref bf ptr)))
             (and (= sign -1)
                  (char=? #\] (list-ref bf ptr)))) ;; into nest
         (loop (+ count 1) (+ ptr sign) sign))
        ((or (and (= sign  1)
                  (char=? #\] (list-ref bf ptr)))
             (and (= sign -1)
                  (char=? #\[ (list-ref bf ptr)))) ;; quit nest
         (if (<= count 1)
             ptr
             (loop (- count 1) (+ ptr sign) sign)))
        ((or (zero? ptr) (= (length bf) ptr))
         (loop 1 ptr (* sign -1)))           ;; reach corner of code
        (else (loop count (+ ptr sign) sign))))))

; adjust value to 1byte (= uint8)
(define overflow
  (lambda (x)
    (cond
      ((< 255 x) 0)
      ((< x 0) 255)
      (else x))))

; dump before exit
(define dump
  (lambda (memory data_ptr inst_ptr) ;; mentioned later
    (format (current-error-port)
            "\nData ptr: ~a\nInstructon ptr: ~a\n~a\n\n"
            data_ptr inst_ptr memory)))

; LET'S GOOOOOOOOO
(let main ([memory (vector 0)]
           [data_ptr 0]  ;; Data pointer
           [inst_ptr 0]) ;; Instructon pointer
  (cond 
    ((<= (length bf) inst_ptr)        ;; end of code
     (dump memory data_ptr inst_ptr))
    ((<= data_ptr -1)                 ;; bad reference
     (dump memory data_ptr inst_ptr))
    ((char=? #\> (list-ref bf inst_ptr)) ;; increment the data pointer
     (if (<= (vector-length memory) (+ data_ptr 1)) ;; first try to reference,
         (main (vector-append memory '#(0))         ;; increase length
               (+ data_ptr 1)
               (+ inst_ptr 1))
         (main memory
               (+ data_ptr 1)
               (+ inst_ptr 1))))
    ((char=? #\< (list-ref bf inst_ptr)) ;; decrenent the data pointer
     (main memory
           (- data_ptr 1)
           (+ inst_ptr 1)))
    ((or (char=? #\+ (list-ref bf inst_ptr))  ;; increment / decrement
         (char=? #\- (list-ref bf inst_ptr))) ;; the referenced value
     (let ([update (+ (vector-ref memory data_ptr)
                      (if (char=? #\+ (list-ref bf inst_ptr))
                          1
                          -1))])
       (main (update_memory memory
                            data_ptr
                            (overflow update))
             data_ptr
             (+ inst_ptr 1))))
    ((char=? #\. (list-ref bf inst_ptr)) ;; put the value
     (begin
       (display (integer->char (vector-ref memory data_ptr)))
       (main memory
             data_ptr
             (+ inst_ptr 1))))
    ((char=? #\, (list-ref bf inst_ptr)) ;; get one value from stdin
     (let ([input_value (char->integer (read-char (current-input-port)))])
       (if (integer? input_value) ;; TODO validation
           (main (update_memory memory
                                data_ptr
                                (overflow input_value))
                 data_ptr
                 (+ inst_ptr 1))
           (begin
             (format (current-error-port)
                     "Invalid input: ~a\n"
                     input_value)
             (main memory       ;; retake `,` operation
                   data_ptr
                   inst_ptr)))))
    ((char=? #\[ (list-ref bf inst_ptr)) ;; start of loop
     (if (zero? (vector-ref memory data_ptr))
         (main memory                    ;; jump to [
               data_ptr
               (+ (seek inst_ptr 1) 1)))
         (main memory                    ;; entering [] loop
               data_ptr
               (+ inst_ptr 1)))
    ((char=? #\] (list-ref bf inst_ptr))  ;; end of loop
     (if (zero? (vector-ref memory data_ptr))
         (main memory                     ;; quit
               data_ptr
               (+ inst_ptr 1))
         (main memory                     ;; back to [
               data_ptr
               (+ (seek inst_ptr -1) 1))))
    (else (main memory            ;; discard other characters
                data_ptr
                (+ inst_ptr 1)))))

(newline)



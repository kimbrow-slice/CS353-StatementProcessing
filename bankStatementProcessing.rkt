#lang racket

(require racket/string)
(require racket/hash)

; Reads data from a file and splits it into lines
(define (read-file-lines filepath)
  (with-input-from-file filepath
    (lambda ()
      (let loop ((line (read-line)) (lines '()))
        (if (eof-object? line)
            (reverse lines)
            (loop (read-line) (cons line lines)))))))

; Parses a string into a number, returning false if parsing fails
(define (parse-number str)
  (cond
    [(string->number str) => values]
    [else #f]))

; Parses a string into an account number
(define (parse-acct-num parts)
  (parse-number (hash-ref parts 1)))

; Formats a numerical amount as a string, appending ".00" if needed as edge cases
(define (format-amount amount)
  (define amount-str (number->string amount))
  (define parts (regexp-split #rx"\\.0$" amount-str))
  (define formatted-str
    (if (null? (cdr parts))
        amount-str
        (string-append (car parts) ".00")))
  formatted-str)

; Parses a string representation of an amount into a number, defaulting to 0.00 if the format is incorrect
(define (parse-amount amount-str)
  (define trimmed-str (string-trim amount-str))
  (if (regexp-match? #rx"^-?[0-9]+(?:\\.[0-9]+)?$" trimmed-str)
      (string->number trimmed-str)
      (begin
        0.00)))  ; Default value to parse

; Strips leading and trailing quotation marks from a string, if present
(define (parse-detail detail-str)
  (if (and (> (string-length detail-str) 1)
           (char=? (string-ref detail-str 0) #\")
           (char=? (string-ref detail-str (- (string-length detail-str) 1)) #\"))
      (substring detail-str 1 (- (string-length detail-str) 1))
      detail-str))

; Parses the payment method from a transaction, handling cash, credit, and check methods
(define (parse-payment-method parts)
  (let ((method (string-trim (string-downcase (list-ref parts 3)))))
    (cond
      ((string=? method "cash") (list "Cash" "" "")) ; Cash transactions will not have card/check number
      ((string=? method "credit")
       (let* ((card-number (if (>= (length parts) 5) (string-trim (list-ref parts 4)) ""))
              (amount-str (if (>= (length parts) 6) (string-trim (list-ref parts 5)) "0")))
         (list "Credit" card-number amount-str)))
      ((string=? method "check"); Similar structure for checks, with a check number and an amount
       (let* ((check-number (if (>= (length parts) 5) (string-trim (list-ref parts 4)) ""))
              (amount-str (if (>= (length parts) 6) (string-trim (list-ref parts 5)) "0")))
         (list "Check" check-number amount-str)))
      (else (list "Unknown" "" "0"))))) ; Default case handles unknown types; assumes no card/check number and zero amount

; Parses a single transaction line into a hash map of transaction details
(define (parse-transaction-line line txn-number)
  (let* ((parts (regexp-split #px"\t" line))
         (type (string->symbol (string-downcase (list-ref parts 0))))
         (acct-num (parse-number (list-ref parts 1)))
         (timestamp (if (>= (length parts) 3) (list-ref parts 2) ""))
         (merchant (if (>= (length parts) 4) (parse-detail (list-ref parts 3)) ""))
         (payment-method-info (if (eq? type 'payment) (parse-payment-method parts) (list "Purchase" "" "")))
         (payment-method (first payment-method-info))
         (card-or-check-number (second payment-method-info))
         (amount-str (if (>= (length parts) 6) (list-ref parts 5) (if (>= (length parts) 5) (list-ref parts 4) "")))
         (amount (parse-amount amount-str)))
    (hash 'type type
          'txn-number txn-number
          'acct-num acct-num
          'timestamp timestamp
          'merchant merchant
          'payment-method payment-method
          'card-or-check-number card-or-check-number
          'amount amount)))

; Loads and parses accounts from a file, creating a hash map of account details
(define (load-accounts filepath)
  (foldl (lambda (line acc-hash)
           (let* ((parts (regexp-match #px"([0-9]+)\\s+\"([^\"]+)\"\\s+([0-9]+\\.[0-9]+)" line)))
             (if parts
                 (let ((account-number (string->number (second parts)))  ; Parse the account number
                       (balance (string->number (fourth parts))))  ; Parse the balance
                   (hash-set acc-hash account-number (hash 'acct-num account-number
                                                            'custInfo (third parts) 
                                                            'balance balance 
                                                            'transactions '() 
                                                            'starting-balance balance)))  ; Use balance as starting-balance and include 'acct-num
                 acc-hash)))
         (hash)  ; Start with an empty immutable hash
         (read-file-lines filepath)))

; Processes transactions from a file, grouping them by account number
(define (load-and-process-transactions transactionFile)
  (let* ((lines (read-file-lines transactionFile))
         (transactions (for/list ([line lines] [index (in-naturals)])
                         (parse-transaction-line line index)))
         (grouped-transactions (foldl (lambda (txn acc)
                                        (let ((acct-num (hash-ref txn 'acct-num))) ; Extracts account number from each transaction.
                                          (hash-update acc acct-num (curry cons txn) '()))); Groups transactions by account number in a hash table.
                                      (hash) ; Start with emprty hash
                                      transactions)))
    grouped-transactions))

; Formats transactions for a single account into a string
(define (format-transactions transactions acct-num starting-balance)
  (string-append
   (format "Account Number: ~a, Starting Balance: ~a\n" acct-num starting-balance)  ; Starting balance line
   (apply string-append
          (map (lambda (txn)
                 (format "Transaction - Type: ~s\t, Merchant: ~a\t, Payment Method: ~a\t,Timestamp: ~a\t, Amount: ~a\n"
                         (hash-ref txn 'type)
                         (hash-ref txn 'merchant)
                         (hash-ref txn 'payment-method)
                         (hash-ref txn 'timestamp "No Timestamp found")
                         (hash-ref txn 'amount "No Amount FOund")))
               transactions))))

; Adjusts the balance of an account based on its transactions
(define (adjust-balance-based-on-transactions account transactions-hash)
  (if (hash-has-key? account 'acct-num) ; Validation check for account having correct account number
      (let* ((acct-num (hash-ref account 'acct-num))
             (transactions (hash-ref transactions-hash acct-num '()))
             (starting-balance (hash-ref account 'starting-balance 0.00))
             (new-balance (foldl (lambda (txn acc-balance)
                                   (let ((amount (hash-ref txn 'amount))
                                         (type (hash-ref txn 'type)))
                                     (case type
                                       ('payment (+ acc-balance amount)) ; Increase for payments
                                       ('purchase (- acc-balance amount)) ; Decrease for purchases
                                       (else acc-balance))))
                                 starting-balance
                                 transactions)))
        (hash 'acct-num acct-num
              'custInfo (hash-ref account 'custInfo)
              'balance new-balance
              'transactions transactions
              'starting-balance starting-balance))
      account))

; Displays account details and transactions, including calculating and showing the final balance
(define (display-account acct-num account transactions)
  (let* ((starting-balance (hash-ref account 'starting-balance 0.00)); Calculate totals directly from transactions
         (total-purchases (apply + (map (lambda (txn)
                                          (if (equal? (hash-ref txn 'type) 'purchase)
                                              (hash-ref txn 'amount 0.00)
                                              0.00))
                                        transactions)))
         (total-payments (apply + (map (lambda (txn)
                                         (if (equal? (hash-ref txn 'type) 'payment)
                                             (hash-ref txn 'amount 0.00)
                                             0.00))
                                       transactions)))
         
         ; Calculate the final balance
         (final-balance (+ starting-balance total-payments (- total-purchases))))
    (printf "Monthly Statement for account\n")
    (printf "Account Number: ~a\tCustomer Name: ~a\tStarting Balance: ~a\n\n\n" ; Account Number Customer Nmae
            acct-num
            (hash-ref account 'custInfo "Unknown Customer")
            (format-amount starting-balance))
    
; Display transaction data
    (for-each (lambda (txn)
                (let* ((type (hash-ref txn 'type))
                       (amount (hash-ref txn 'amount 0.00)); Deafult amount to 0.00
                       (payment-method (hash-ref txn 'payment-method ""))  ; defaults values to empty strings if not found
                       (check-number (hash-ref txn 'check-number ""))
                       (card-number (hash-ref txn 'card-number ""))
                       (timestamp (hash-ref txn 'timestamp ""))
                       (merchant (if (equal? type 'purchase) (hash-ref txn 'merchant "") "")))
                  (printf "Timestamp: ~a\t" timestamp)
                  (when (equal? type 'purchase); Conditional printing for purchases, merchant, payment-method and card number
                    (printf "Merchant: ~a\t" merchant))
                  (printf "Payment Type: ~a\t" payment-method)
                  (when (not (equal? check-number ""))
                    (printf "Check Number: ~a\t" check-number))
                  (when (not (equal? card-number ""))
                    (printf "Card Number: ~a\t" card-number))
                  (printf "Amount: ~a\n\n" (format-amount amount))))
              transactions)
    
; Display Totals and final balance
    (printf "Total Purchases: ~a\n"(format-amount total-purchases))
    (printf "Total Payments: ~a\n" (format-amount total-payments))
    (printf "Final Balance: ~a\n\n****************************************************************************************************\n\n" (format-amount final-balance))))

; Writes formatted account transactions to an output file
(define (output-file filename accounts grouped-transactions)
  (with-output-to-file filename
    (lambda ()
      (if (hash? accounts) ; Check if accounts is valid hash map, if so iterate over each account to display the information
          (begin
            (for-each
             (lambda (account-pair)
               (let* ((acct-num (car account-pair))
                      (account (cdr account-pair))
                      (transactions (hash-ref grouped-transactions acct-num '())))
                 (display-account acct-num account transactions)))
             (hash->list accounts)))
          (printf "No accounts found.")))
    #:exists 'replace)) ; Automatically replace file to avoid overwrite prompting

; Updates all accounts with their respective transactions, adjusting balances
(define (update-all-accounts-with-transactions accounts transactions-hash)
  (foldl (lambda (account-pair acc-hash)
           (let ((acct-num (car account-pair))
                 (account (cdr account-pair)))
             (hash-set acc-hash acct-num (adjust-balance-based-on-transactions account transactions-hash))))
         (hash) ; Start with an empty hash
         (hash->list accounts)))

; Main
(define (main)
  (printf "Hello! Welcome to my version of the Bank Statement Processing Program!\n")
  (let* ((accountFile "./ACCOUNTS.txt")
         (transactionFile "./TRANSACTIONS.txt")
         (accounts (load-accounts accountFile))
         (grouped-transactions (load-and-process-transactions transactionFile)) ; Loads and groups transaction data by account number
         (updated-accounts (update-all-accounts-with-transactions accounts grouped-transactions))); Update account balance from the transactions
    (output-file "STATEMENTS.txt" updated-accounts grouped-transactions))
(printf "The program has already ran. You will need to go check the folder where this program is saved to see the statement!\nIt will overwrite if you would like to try editing values in the input files and running again!\n"))

(main)

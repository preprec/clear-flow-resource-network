;; Clear Flow - Tokenized Resource Allocation Network

;; ========== UTILITY FUNCTIONS ==========

(define-private (adjust-system-resources (adjustment int))
  (let (
    (current-total (var-get total-registered-resources))
    (adjusted-total (if (< adjustment 0)
                     (if (>= current-total (to-uint (- 0 adjustment)))
                         (- current-total (to-uint (- 0 adjustment)))
                         u0)
                     (+ current-total (to-uint adjustment))))
  )
    (asserts! (<= adjusted-total (var-get system-capacity-limit)) error-capacity-exceeded)
    (var-set total-registered-resources adjusted-total)
    (ok true)))
(define-private (calculate-commission (value uint))
  (/ (* value (var-get transaction-commission)) u100))
(define-private (calculate-reimbursement (quantity uint))
  (/ (* quantity (var-get base-rate) (var-get reimbursement-rate)) u100))

;; ========== GLOBAL SETTINGS ==========


(define-data-var reimbursement-rate uint u90)
(define-data-var base-rate uint u100)
(define-data-var participant-resource-ceiling uint u10000)
(define-data-var total-registered-resources uint u0)
(define-data-var transaction-commission uint u5)
(define-data-var system-capacity-limit uint u1000000)

;; ========== DATA STRUCTURES ==========

(define-map participant-resource-balances principal uint)
(define-map participant-token-balances principal uint)
(define-map resource-exchange-registry {participant: principal} {quantity: uint, rate: uint})


;; ========== OWNER & ERROR CODES ==========
(define-constant admin-account tx-sender)
(define-constant error-capacity-exceeded (err u208))
(define-constant error-unauthorized (err u200))
(define-constant error-reimbursement-failed (err u206))
(define-constant error-self-transaction (err u207))
(define-constant error-insufficient-resource (err u201))
(define-constant error-transaction-unsuccessful (err u202))
(define-constant error-invalid-rate (err u203))
(define-constant error-invalid-quantity (err u204))
(define-constant error-invalid-commission (err u205))
(define-constant error-invalid-capacity (err u209))



;; ========== TOKEN OPERATIONS ==========

;; Withdraw tokens from the system
(define-public (withdraw-tokens (quantity uint))
  (let (
    (current-balance (default-to u0 (map-get? participant-token-balances tx-sender)))
    (new-balance (if (>= current-balance quantity)
                    (- current-balance quantity)
                    u0))
  )
    ;; Validations
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (>= current-balance quantity) error-insufficient-resource)

    ;; Update participant's token balance
    (map-set participant-token-balances tx-sender new-balance)

    ;; Process token transfer through contract
    (try! (as-contract (stx-transfer? quantity (as-contract tx-sender) tx-sender)))

    (ok new-balance)))

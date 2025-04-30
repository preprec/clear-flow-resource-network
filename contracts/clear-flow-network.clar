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

;; ========== ENHANCED OPERATIONS ==========

;; Enhanced resource exchange with validation safeguards
(define-public (secure-resource-exchange (quantity uint))
  (let (
        (participant-resources (default-to u0 (map-get? participant-resource-balances tx-sender)))
        (token-amount (calculate-reimbursement quantity))
  )
    ;; Additional validation checks
    (asserts! (>= participant-resources quantity) error-insufficient-resource)
    (asserts! (> token-amount u0) error-reimbursement-failed)

    ;; Process the exchange
    (map-set participant-resource-balances tx-sender (- participant-resources quantity))
    (map-set participant-token-balances tx-sender 
             (+ (default-to u0 (map-get? participant-token-balances tx-sender)) token-amount))
    (map-set participant-token-balances admin-account 
             (- (default-to u0 (map-get? participant-token-balances admin-account)) token-amount))

    (ok true)))

;; Performance-optimized resource acquisition
(define-public (optimized-resource-acquisition (provider principal) (quantity uint))
  (let (
        (listing-data (default-to {quantity: u0, rate: u0} 
                      (map-get? resource-exchange-registry {participant: provider})))
        (resource-price (* quantity (get rate listing-data)))
        (acquirer-tokens (default-to u0 (map-get? participant-token-balances tx-sender)))
        (provider-resources (default-to u0 (map-get? participant-resource-balances provider)))
  )
    ;; Streamlined validation
    (asserts! (>= acquirer-tokens resource-price) error-insufficient-resource)
    (asserts! (>= provider-resources quantity) error-insufficient-resource)

    ;; Direct balance updates
    (map-set participant-token-balances tx-sender (- acquirer-tokens resource-price))
    (map-set participant-resource-balances tx-sender 
             (+ (default-to u0 (map-get? participant-resource-balances tx-sender)) quantity))
    (map-set participant-resource-balances provider (- provider-resources quantity))
    (map-set participant-token-balances provider 
             (+ (default-to u0 (map-get? participant-token-balances provider)) resource-price))

    (ok true)))

;; Transfer resources between participants
(define-public (transfer-resources (recipient principal) (quantity uint))
  (let (
    (sender-resources (default-to u0 (map-get? participant-resource-balances tx-sender)))
    (recipient-resources (default-to u0 (map-get? participant-resource-balances recipient)))
    (transfer-fee (calculate-commission (var-get base-rate)))
    (sender-token-balance (default-to u0 (map-get? participant-token-balances tx-sender)))
  )
    ;; Transaction validations
    (asserts! (not (is-eq tx-sender recipient)) error-self-transaction)
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (>= sender-resources quantity) error-insufficient-resource)
    (asserts! (>= sender-token-balance transfer-fee) error-insufficient-resource)
    (asserts! (<= (+ recipient-resources quantity) (var-get participant-resource-ceiling)) 
              error-capacity-exceeded)

    ;; Update resource balances
    (map-set participant-resource-balances tx-sender (- sender-resources quantity))
    (map-set participant-resource-balances recipient (+ recipient-resources quantity))

    ;; Process fee payment
    (map-set participant-token-balances tx-sender (- sender-token-balance transfer-fee))
    (map-set participant-token-balances admin-account 
             (+ (default-to u0 (map-get? participant-token-balances admin-account)) transfer-fee))

    (ok true)
  )
)

;; ========== MARKET OPERATIONS ==========

;; Remove resources from exchange listing
(define-public (delist-resources (quantity uint))
  (let (
    (listing-data (default-to {quantity: u0, rate: u0} 
                  (map-get? resource-exchange-registry {participant: tx-sender})))
    (listed-quantity (get quantity listing-data))
    (listed-rate (get rate listing-data))
  )
    ;; Validations
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (>= listed-quantity quantity) error-insufficient-resource)

    ;; Update the exchange registry
    (map-set resource-exchange-registry 
             {participant: tx-sender} 
             {quantity: (- listed-quantity quantity), rate: listed-rate})

    (ok true)))

;; Cancel all exchange listings
(define-public (cancel-all-listings)
  (let (
    (listing-data (default-to {quantity: u0, rate: u0} 
                  (map-get? resource-exchange-registry {participant: tx-sender})))
    (listed-quantity (get quantity listing-data))
    (system-total (var-get total-registered-resources))
  )
    (asserts! (> listed-quantity u0) error-insufficient-resource)
    (var-set total-registered-resources (- system-total listed-quantity))
    (map-set resource-exchange-registry {participant: tx-sender} {quantity: u0, rate: u0})
    (print {event: "listings-cancelled", participant: tx-sender, quantity: listed-quantity})
    (ok true)))

;; Cancel specific resource listing
(define-public (cancel-specific-listing (quantity uint))
  (let (
    (current-listing (default-to {quantity: u0, rate: u0} 
                     (map-get? resource-exchange-registry {participant: tx-sender})))
    (listing-quantity (get quantity current-listing))
    (listing-rate (get rate current-listing))
  )
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (>= listing-quantity quantity) error-insufficient-resource)
    (if (is-eq listing-quantity quantity)
        (map-delete resource-exchange-registry {participant: tx-sender})
        (map-set resource-exchange-registry {participant: tx-sender} 
                {quantity: (- listing-quantity quantity), rate: listing-rate}))

    (ok true)))

;; ========== PARTICIPANT OPERATIONS ==========

;; Acquire resources from another participant
(define-public (acquire-resources (provider principal) (quantity uint))
  (let (
    (listing-data (default-to {quantity: u0, rate: u0} 
                   (map-get? resource-exchange-registry {participant: provider})))
    (resource-price (* quantity (get rate listing-data)))
    (commission-fee (calculate-commission resource-price))
    (total-price (+ resource-price commission-fee))
    (provider-resources (default-to u0 (map-get? participant-resource-balances provider)))
    (acquirer-tokens (default-to u0 (map-get? participant-token-balances tx-sender)))
    (provider-tokens (default-to u0 (map-get? participant-token-balances provider)))
    (admin-tokens (default-to u0 (map-get? participant-token-balances admin-account)))
  )
    ;; Transaction validations
    (asserts! (not (is-eq tx-sender provider)) error-self-transaction)
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (>= (get quantity listing-data) quantity) error-insufficient-resource)
    (asserts! (>= provider-resources quantity) error-insufficient-resource)
    (asserts! (>= acquirer-tokens total-price) error-insufficient-resource)

    ;; Update provider's resource balance and listing
    (map-set participant-resource-balances provider (- provider-resources quantity))
    (map-set resource-exchange-registry {participant: provider} 
             {quantity: (- (get quantity listing-data) quantity), 
              rate: (get rate listing-data)})

    ;; Update token balances
    (map-set participant-token-balances tx-sender (- acquirer-tokens total-price))
    (map-set participant-token-balances provider (+ provider-tokens resource-price))
    (map-set participant-token-balances admin-account (+ admin-tokens commission-fee))

    ;; Update acquirer's resource balance
    (map-set participant-resource-balances tx-sender 
             (+ (default-to u0 (map-get? participant-resource-balances tx-sender)) quantity))

    (ok true)))

;; Exchange resources for tokens based on base rate
(define-public (exchange-resources-for-tokens (quantity uint))
  (let (
    (participant-resources (default-to u0 (map-get? participant-resource-balances tx-sender)))
    (token-amount (calculate-reimbursement quantity))
    (admin-token-balance (default-to u0 (map-get? participant-token-balances admin-account)))
  )
    ;; Validations
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (>= participant-resources quantity) error-insufficient-resource)
    (asserts! (>= admin-token-balance token-amount) error-reimbursement-failed)

    ;; Update participant's resource balance
    (map-set participant-resource-balances tx-sender (- participant-resources quantity))

    ;; Update token balances
    (map-set participant-token-balances tx-sender 
             (+ (default-to u0 (map-get? participant-token-balances tx-sender)) token-amount))
    (map-set participant-token-balances admin-account (- admin-token-balance token-amount))

    (ok true)))

;; Register new resources in the system
(define-public (register-resources (quantity uint))
  (let (
    (current-balance (default-to u0 (map-get? participant-resource-balances tx-sender)))
    (new-balance (+ current-balance quantity))
    (system-total (var-get total-registered-resources))
    (updated-total (+ system-total quantity))
  )
    ;; Validations
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (<= new-balance (var-get participant-resource-ceiling)) error-capacity-exceeded)
    (asserts! (<= updated-total (var-get system-capacity-limit)) error-capacity-exceeded)

    ;; Update participant's balance
    (map-set participant-resource-balances tx-sender new-balance)

    ;; Update system total
    (var-set total-registered-resources updated-total)

    ;; Return success
    (ok true)))

;; List resources for exchange
(define-public (list-resources-for-exchange (quantity uint) (rate uint))
  (let (
    (current-holdings (default-to u0 (map-get? participant-resource-balances tx-sender)))
    (currently-listed (get quantity (default-to {quantity: u0, rate: u0} 
                           (map-get? resource-exchange-registry {participant: tx-sender}))))
    (new-listing-total (+ quantity currently-listed))
  )
    ;; Validate inputs
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (> rate u0) error-invalid-rate)
    (asserts! (>= current-holdings new-listing-total) error-insufficient-resource)

    ;; Update system resource tracking
    (try! (adjust-system-resources (to-int quantity)))

    ;; Update exchange registry
    (map-set resource-exchange-registry {participant: tx-sender} 
             {quantity: new-listing-total, rate: rate})

    (ok true)))

;; ========== ADMINISTRATIVE FUNCTIONS ==========

;; Allocate resources to a participant (administrative function)
(define-public (allocate-resources (participant principal) (quantity uint))
  (let (
    (current-balance (default-to u0 (map-get? participant-resource-balances participant)))
    (new-balance (+ current-balance quantity))
    (system-total (var-get total-registered-resources))
    (updated-total (+ system-total quantity))
  )
    ;; Admin-only validation
    (asserts! (is-eq tx-sender admin-account) error-unauthorized)
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (<= new-balance (var-get participant-resource-ceiling)) error-capacity-exceeded)
    (asserts! (<= updated-total (var-get system-capacity-limit)) error-capacity-exceeded)

    ;; Update system total
    (var-set total-registered-resources updated-total)

    ;; Log the allocation for audit trail
    (print {event: "resource-allocation", participant: participant, quantity: quantity, new-balance: new-balance})

    (ok new-balance)))



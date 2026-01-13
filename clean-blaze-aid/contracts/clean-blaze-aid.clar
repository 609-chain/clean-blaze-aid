;; CleanBlazeAid - Environmental Impact Verification Platform
;; A blockchain-powered platform for wildfire prevention and disaster response

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-validator (err u101))
(define-constant err-already-validated (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-activity-not-found (err u104))
(define-constant err-invalid-activity (err u105))
(define-constant err-consensus-not-reached (err u106))

;; Data Variables
(define-data-var activity-nonce uint u0)
(define-data-var min-validators uint u3)
(define-data-var ivt-per-acre uint u100)

;; Data Maps
(define-map validators principal bool)
(define-map validator-stakes principal uint)

(define-map activities
  uint
  {
    creator: principal,
    activity-type: (string-ascii 50),
    acres-cleared: uint,
    carbon-sequestered: uint,
    location: (string-ascii 100),
    timestamp: uint,
    verified: bool,
    validation-count: uint
  }
)

(define-map activity-validations
  {activity-id: uint, validator: principal}
  bool
)

(define-map user-balances principal uint)
(define-map user-impact-scores principal uint)

;; SIP-010 Fungible Token Trait (simplified)
(define-fungible-token impact-token)

;; Read-only functions
(define-read-only (get-balance (account principal))
  (ok (ft-get-balance impact-token account))
)

(define-read-only (get-activity (activity-id uint))
  (ok (map-get? activities activity-id))
)

(define-read-only (is-validator (account principal))
  (default-to false (map-get? validators account))
)

(define-read-only (get-validator-stake (validator principal))
  (default-to u0 (map-get? validator-stakes validator))
)

(define-read-only (has-validated (activity-id uint) (validator principal))
  (default-to false (map-get? activity-validations {activity-id: activity-id, validator: validator}))
)

(define-read-only (get-impact-score (user principal))
  (default-to u0 (map-get? user-impact-scores user))
)

;; Public functions

;; Register as validator with stake
(define-public (register-validator (stake-amount uint))
  (begin
    (asserts! (>= stake-amount u1000) err-insufficient-balance)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set validators tx-sender true)
    (map-set validator-stakes tx-sender stake-amount)
    (ok true)
  )
)

;; Submit environmental activity
(define-public (submit-activity 
  (activity-type (string-ascii 50))
  (acres-cleared uint)
  (carbon-sequestered uint)
  (location (string-ascii 100)))
  (let
    (
      (activity-id (+ (var-get activity-nonce) u1))
    )
    (asserts! (> acres-cleared u0) err-invalid-activity)
    (map-set activities activity-id
      {
        creator: tx-sender,
        activity-type: activity-type,
        acres-cleared: acres-cleared,
        carbon-sequestered: carbon-sequestered,
        location: location,
        timestamp: block-height,
        verified: false,
        validation-count: u0
      }
    )
    (var-set activity-nonce activity-id)
    (ok activity-id)
  )
)

;; Validate activity (validators only)
(define-public (validate-activity (activity-id uint))
  (let
    (
      (activity (unwrap! (map-get? activities activity-id) err-activity-not-found))
      (validator tx-sender)
      (current-validations (get validation-count activity))
    )
    (asserts! (is-validator validator) err-not-validator)
    (asserts! (not (has-validated activity-id validator)) err-already-validated)
    
    ;; Record validation
    (map-set activity-validations {activity-id: activity-id, validator: validator} true)
    
    ;; Update validation count
    (let
      (
        (new-validation-count (+ current-validations u1))
        (min-val (var-get min-validators))
      )
      (map-set activities activity-id
        (merge activity {validation-count: new-validation-count})
      )
      
      ;; If consensus reached, mark as verified and mint tokens
      (if (>= new-validation-count min-val)
        (begin
          (map-set activities activity-id
            (merge activity {verified: true, validation-count: new-validation-count})
          )
          (try! (mint-impact-tokens activity-id))
          (ok true)
        )
        (ok true)
      )
    )
  )
)

;; Internal function to mint tokens based on verified activity
(define-private (mint-impact-tokens (activity-id uint))
  (let
    (
      (activity (unwrap! (map-get? activities activity-id) err-activity-not-found))
      (creator (get creator activity))
      (acres (get acres-cleared activity))
      (token-amount (* acres (var-get ivt-per-acre)))
    )
    (asserts! (get verified activity) err-consensus-not-reached)
    
    ;; Mint IVT tokens
    (try! (ft-mint? impact-token token-amount creator))
    
    ;; Update impact score
    (map-set user-impact-scores creator
      (+ (get-impact-score creator) acres)
    )
    (ok token-amount)
  )
)

;; Transfer tokens
(define-public (transfer (amount uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) err-owner-only)
    (try! (ft-transfer? impact-token amount sender recipient))
    (ok true)
  )
)

;; Admin functions

(define-public (set-min-validators (new-min uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-validators new-min)
    (ok true)
  )
)

(define-public (set-ivt-per-acre (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set ivt-per-acre new-rate)
    (ok true)
  )
)

;; Initialize
(begin
  (map-set validators contract-owner true)
  (map-set validator-stakes contract-owner u10000)
)
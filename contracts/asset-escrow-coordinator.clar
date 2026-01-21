;; ============================================================
;; Contract: asset-escrow-coordinator
;; Purpose : Conditional escrow lifecycle with arbitration
;; ============================================================

;; ------------------------------------------------------------
;; Error Codes
;; ------------------------------------------------------------
(define-constant ERR-NOT-BUYER        (err u3000))
(define-constant ERR-NOT-SELLER       (err u3001))
(define-constant ERR-NOT-ARBITER      (err u3002))
(define-constant ERR-INVALID-STATE    (err u3003))
(define-constant ERR-EXPIRED          (err u3004))
(define-constant ERR-NOT-FOUND        (err u3005))
(define-constant ERR-INVALID-PARAMS  (err u3006))

;; ------------------------------------------------------------
;; Escrow States
;; ------------------------------------------------------------
(define-constant STATE-OPEN       u0)
(define-constant STATE-FUNDED     u1)
(define-constant STATE-RELEASED   u2)
(define-constant STATE-REFUNDED   u3)
(define-constant STATE-DISPUTED   u4)

;; ------------------------------------------------------------
;; Data Storage
;; ------------------------------------------------------------
(define-data-var escrow-nonce uint u0)

(define-map escrows
  { id: uint }
  {
    buyer: principal,
    seller: principal,
    arbiter: (optional principal),
    amount: uint,
    deadline: uint,
    state: uint
  }
)

;; ------------------------------------------------------------
;; Read-Only Functions
;; ------------------------------------------------------------
(define-read-only (get-escrow (id uint))
  (map-get? escrows { id: id })
)

;; ------------------------------------------------------------
;; Internal Helpers - Return bool only
;; ------------------------------------------------------------
(define-private (next-id)
  (let ((id (var-get escrow-nonce)))
    (var-set escrow-nonce (+ id u1))
    id
  )
)

(define-private (assert-state (current uint) (expected uint))
  (is-eq current expected)
)

;; ------------------------------------------------------------
;; Public Functions - Escrow Creation
;; ------------------------------------------------------------
(define-public (create-escrow
  (seller principal)
  (arbiter (optional principal))
  (amount uint)
  (deadline uint)
)
  (begin
    (asserts! (> amount u0) ERR-INVALID-PARAMS)
    (asserts! (> deadline burn-block-height) ERR-INVALID-PARAMS)

    (let ((id (next-id)))
      (map-set escrows
        { id: id }
        {
          buyer: tx-sender,
          seller: seller,
          arbiter: arbiter,
          amount: amount,
          deadline: deadline,
          state: STATE-OPEN
        }
      )
      (ok id)
    )
  )
)

;; ------------------------------------------------------------
;; Public Functions - Funding & Release
;; ------------------------------------------------------------
(define-public (mark-funded (id uint))
  (match (map-get? escrows { id: id })
    e
      (begin
        (asserts! (is-eq tx-sender (get buyer e)) ERR-NOT-BUYER)
        (asserts! (assert-state (get state e) STATE-OPEN) ERR-INVALID-STATE)

        (map-set escrows
          { id: id }
          {
            buyer: (get buyer e),
            seller: (get seller e),
            arbiter: (get arbiter e),
            amount: (get amount e),
            deadline: (get deadline e),
            state: STATE-FUNDED
          }
        )
        (ok true)
      )
    ERR-NOT-FOUND
  )
)

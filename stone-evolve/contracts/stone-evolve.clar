;; StoneEvolve - Geological Evolution NFT Platform
;; A geological evolution simulator with authentic metamorphic progression

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-insufficient-time (err u102))
(define-constant err-invalid-evolution (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-not-found (err u105))
(define-constant err-insufficient-balance (err u106))

;; Data Variables
(define-data-var last-token-id uint u0)
(define-data-var geology-token-supply uint u0)
(define-data-var evolution-fee uint u1000000) ;; 1 STX in microSTX

;; Rock Types
(define-constant SEDIMENTARY u1)
(define-constant METAMORPHIC u2)
(define-constant IGNEOUS u3)

;; Evolution Stages
(define-constant STAGE-BASIC u1)
(define-constant STAGE-INTERMEDIATE u2)
(define-constant STAGE-ADVANCED u3)
(define-constant STAGE-LEGENDARY u4)

;; NFT Definition
(define-non-fungible-token stone-nft uint)

;; Fungible Token for GEOLOGY rewards
(define-fungible-token geology-token)

;; Data Maps
(define-map stone-data
  uint
  {
    rock-type: uint,
    stage: uint,
    creation-time: uint,
    last-evolution: uint,
    mineral-composition: (string-ascii 100),
    pressure-applied: uint,
    heat-applied: uint,
    evolution-count: uint,
    rarity-score: uint
  }
)

(define-map user-balances
  principal
  {
    stone-count: uint,
    geology-tokens: uint,
    time-acceleration-tokens: uint
  }
)

(define-map evolution-paths
  {from-type: uint, from-stage: uint}
  {to-type: uint, to-stage: uint, required-time: uint, required-pressure: uint, required-heat: uint}
)

(define-map geologist-council
  principal
  {
    stake-amount: uint,
    validation-count: uint,
    reputation: uint
  }
)

;; Private Functions
(define-private (calculate-rarity (rock-type uint) (stage uint) (evolution-count uint))
  (+ (* rock-type u10) (* stage u20) (* evolution-count u5))
)

;; Public Functions

;; Mint initial sedimentary stone NFT
(define-public (mint-stone (mineral-composition (string-ascii 100)))
  (let
    (
      (token-id (+ (var-get last-token-id) u1))
      (current-time block-height)
    )
    (try! (nft-mint? stone-nft token-id tx-sender))
    (map-set stone-data token-id
      {
        rock-type: SEDIMENTARY,
        stage: STAGE-BASIC,
        creation-time: current-time,
        last-evolution: current-time,
        mineral-composition: mineral-composition,
        pressure-applied: u0,
        heat-applied: u0,
        evolution-count: u0,
        rarity-score: (calculate-rarity SEDIMENTARY STAGE-BASIC u0)
      }
    )
    (var-set last-token-id token-id)
    (ok token-id)
  )
)

;; Apply environmental pressure to stone
(define-public (apply-pressure (token-id uint) (pressure-amount uint))
  (let
    (
      (stone (unwrap! (map-get? stone-data token-id) err-not-found))
      (token-owner (unwrap! (nft-get-owner? stone-nft token-id) err-not-found))
    )
    (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
    (map-set stone-data token-id
      (merge stone {pressure-applied: (+ (get pressure-applied stone) pressure-amount)})
    )
    (ok true)
  )
)

;; Apply heat to stone
(define-public (apply-heat (token-id uint) (heat-amount uint))
  (let
    (
      (stone (unwrap! (map-get? stone-data token-id) err-not-found))
      (token-owner (unwrap! (nft-get-owner? stone-nft token-id) err-not-found))
    )
    (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
    (map-set stone-data token-id
      (merge stone {heat-applied: (+ (get heat-applied stone) heat-amount)})
    )
    (ok true)
  )
)

;; Evolve stone through geological transformation
(define-public (evolve-stone (token-id uint))
  (let
    (
      (stone (unwrap! (map-get? stone-data token-id) err-not-found))
      (token-owner (unwrap! (nft-get-owner? stone-nft token-id) err-not-found))
      (current-time block-height)
      (time-elapsed (- current-time (get last-evolution stone)))
      (new-stage (+ (get stage stone) u1))
      (new-evolution-count (+ (get evolution-count stone) u1))
    )
    (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
    (asserts! (> time-elapsed u100) err-insufficient-time) ;; Require at least 100 blocks
    (asserts! (<= new-stage STAGE-LEGENDARY) err-invalid-evolution)
    
    ;; Evolution conditions based on rock type
    (asserts! (or
      (and (is-eq (get rock-type stone) SEDIMENTARY) (>= (get pressure-applied stone) u50))
      (and (is-eq (get rock-type stone) METAMORPHIC) (>= (get heat-applied stone) u75))
      (and (is-eq (get rock-type stone) IGNEOUS) (>= (get pressure-applied stone) u40))
    ) err-invalid-evolution)
    
    (map-set stone-data token-id
      (merge stone {
        stage: new-stage,
        last-evolution: current-time,
        evolution-count: new-evolution-count,
        rarity-score: (calculate-rarity (get rock-type stone) new-stage new-evolution-count)
      })
    )
    
    ;; Reward GEOLOGY tokens for successful evolution
    (try! (ft-mint? geology-token (* new-stage u100) tx-sender))
    
    (ok true)
  )
)

;; Transform stone type (sedimentary -> metamorphic -> igneous)
(define-public (transform-stone-type (token-id uint) (new-rock-type uint))
  (let
    (
      (stone (unwrap! (map-get? stone-data token-id) err-not-found))
      (token-owner (unwrap! (nft-get-owner? stone-nft token-id) err-not-found))
      (fee (var-get evolution-fee))
    )
    (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
    (asserts! (<= new-rock-type IGNEOUS) err-invalid-evolution)
    (asserts! (>= (get pressure-applied stone) u100) err-invalid-evolution)
    (asserts! (>= (get heat-applied stone) u100) err-invalid-evolution)
    
    ;; Pay evolution fee
    (try! (stx-transfer? fee tx-sender contract-owner))
    
    (map-set stone-data token-id
      (merge stone {
        rock-type: new-rock-type,
        pressure-applied: u0,
        heat-applied: u0,
        rarity-score: (calculate-rarity new-rock-type (get stage stone) (get evolution-count stone))
      })
    )
    
    ;; Reward bonus GEOLOGY tokens for type transformation
    (try! (ft-mint? geology-token u500 tx-sender))
    
    (ok true)
  )
)

;; Transfer stone NFT
(define-public (transfer-stone (token-id uint) (recipient principal))
  (let
    (
      (token-owner (unwrap! (nft-get-owner? stone-nft token-id) err-not-found))
    )
    (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
    (nft-transfer? stone-nft token-id tx-sender recipient)
  )
)

;; Stake for Geologist Council
(define-public (join-geologist-council (stake-amount uint))
  (begin
    (asserts! (>= stake-amount u1000) err-insufficient-balance)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set geologist-council tx-sender
      {
        stake-amount: stake-amount,
        validation-count: u0,
        reputation: u100
      }
    )
    (ok true)
  )
)

;; Validate geological transformation (for council members)
(define-public (validate-transformation (token-id uint) (is-valid bool))
  (let
    (
      (council-member (unwrap! (map-get? geologist-council tx-sender) err-not-found))
      (current-validations (get validation-count council-member))
      (current-reputation (get reputation council-member))
    )
    (map-set geologist-council tx-sender
      (merge council-member {
        validation-count: (+ current-validations u1),
        reputation: (if is-valid (+ current-reputation u10) current-reputation)
      })
    )
    
    ;; Reward validator
    (try! (ft-mint? geology-token u50 tx-sender))
    
    (ok true)
  )
)

;; Burn GEOLOGY tokens for time acceleration
(define-public (accelerate-time (token-id uint) (geology-amount uint))
  (let
    (
      (stone (unwrap! (map-get? stone-data token-id) err-not-found))
      (token-owner (unwrap! (nft-get-owner? stone-nft token-id) err-not-found))
      (time-boost (/ geology-amount u10))
    )
    (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
    (try! (ft-burn? geology-token geology-amount tx-sender))
    
    (map-set stone-data token-id
      (merge stone {
        last-evolution: (- (get last-evolution stone) time-boost)
      })
    )
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-stone-data (token-id uint))
  (map-get? stone-data token-id)
)

(define-read-only (get-stone-owner (token-id uint))
  (nft-get-owner? stone-nft token-id)
)

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id))
)

(define-read-only (get-geology-balance (account principal))
  (ok (ft-get-balance geology-token account))
)

(define-read-only (get-council-member (member principal))
  (map-get? geologist-council member)
)

(define-read-only (get-evolution-fee)
  (ok (var-get evolution-fee))
)

;; Admin functions

(define-public (set-evolution-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set evolution-fee new-fee)
    (ok true)
  )
)

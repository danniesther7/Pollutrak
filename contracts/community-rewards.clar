;; Community Engagement & Rewards System
;; Tracks community participation, implements token rewards, and provides engagement analytics

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u800))
(define-constant ERR-ALREADY-CLAIMED (err u801))
(define-constant ERR-INVALID-PARAMETERS (err u802))
(define-constant ERR-INSUFFICIENT-TOKENS (err u803))
(define-constant ERR-REWARD-NOT-AVAILABLE (err u804))
(define-constant ERR-ACTIVITY-NOT-FOUND (err u805))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant BASE-REWARD-AMOUNT u100000)
(define-constant STREAK-BONUS-MULTIPLIER u125) ;; 25% bonus per streak
(define-constant WEEKLY-PERIOD u1008) ;; blocks per week
(define-constant ENGAGEMENT-THRESHOLD u5) ;; minimum activities per period

;; Activity type rewards
(define-constant REPORT-REWARD u150000)
(define-constant VOTE-REWARD u50000)
(define-constant ZONE-CREATION-REWARD u300000)
(define-constant COMMUNITY-ACTION-REWARD u200000)
(define-constant EDUCATION-REWARD u100000)

;; Data variables
(define-data-var total-rewards-distributed uint u0)
(define-data-var community-token-supply uint u10000000) ;; 10M tokens
(define-data-var engagement-season uint u1)
(define-data-var season-start-block uint u0)

;; User engagement tracking
(define-map user-engagement-stats
  principal
  {
    total-activities: uint,
    current-streak: uint,
    longest-streak: uint,
    last-activity-block: uint,
    total-rewards-earned: uint,
    engagement-level: (string-ascii 15),
    weekly-activities: uint,
    community-rank: uint
  }
)

;; Activity rewards tracking
(define-map activity-rewards
  { user: principal, activity-id: uint }
  {
    activity-type: (string-ascii 20),
    reward-amount: uint,
    timestamp: uint,
    bonus-applied: uint,
    claimed: bool
  }
)

;; Weekly engagement challenges
(define-map weekly-challenges
  { week: uint }
  {
    challenge-type: (string-ascii 30),
    target-value: uint,
    reward-pool: uint,
    participants: uint,
    completed-by: uint,
    active: bool
  }
)

;; Community leaderboards
(define-map community-leaderboard
  { period: uint, rank: uint }
  {
    user: principal,
    score: uint,
    activities-count: uint,
    rewards-earned: uint
  }
)

;; Token balance tracking
(define-map user-token-balances
  principal
  uint
)

;; Educational content completion
(define-map education-progress
  { user: principal, content-id: uint }
  {
    completed-at: uint,
    score: uint,
    reward-claimed: bool
  }
)

;; Record user activity and calculate rewards
(define-public (record-activity (activity-type (string-ascii 20)) (activity-id uint))
  (let
    (
      (user-stats (default-to
        { total-activities: u0, current-streak: u0, longest-streak: u0, last-activity-block: u0, total-rewards-earned: u0, engagement-level: "beginner", weekly-activities: u0, community-rank: u0 }
        (map-get? user-engagement-stats tx-sender)
      ))
      (current-block stacks-block-height)
      (base-reward (get-activity-reward activity-type))
      (streak-multiplier (calculate-streak-bonus (get current-streak user-stats)))
      (final-reward (/ (* base-reward streak-multiplier) u100))
      (new-streak (calculate-new-streak user-stats current-block))
      (new-activities (+ (get total-activities user-stats) u1))
      (new-weekly (if (is-within-week (get last-activity-block user-stats) current-block)
                    (+ (get weekly-activities user-stats) u1)
                    u1))
    )
    
    (asserts! (> final-reward u0) ERR-INVALID-PARAMETERS)
    (asserts! (<= final-reward (var-get community-token-supply)) ERR-INSUFFICIENT-TOKENS)
    
    ;; Update user engagement stats
    (map-set user-engagement-stats tx-sender
      {
        total-activities: new-activities,
        current-streak: new-streak,
        longest-streak: (if (> new-streak (get longest-streak user-stats)) new-streak (get longest-streak user-stats)),
        last-activity-block: current-block,
        total-rewards-earned: (+ (get total-rewards-earned user-stats) final-reward),
        engagement-level: (calculate-engagement-level new-activities),
        weekly-activities: new-weekly,
        community-rank: (get community-rank user-stats)
      }
    )
    
    ;; Record the reward
    (map-set activity-rewards
      { user: tx-sender, activity-id: activity-id }
      {
        activity-type: activity-type,
        reward-amount: final-reward,
        timestamp: current-block,
        bonus-applied: (- streak-multiplier u100),
        claimed: false
      }
    )
    
    ;; Update token balance
    (mint-community-tokens tx-sender final-reward)
    
    ;; Update global stats
    (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) final-reward))
    (var-set community-token-supply (- (var-get community-token-supply) final-reward))
    
    (ok final-reward)
  )
)

;; Get base reward for activity type
(define-private (get-activity-reward (activity-type (string-ascii 20)))
  (if (is-eq activity-type "report")
    REPORT-REWARD
    (if (is-eq activity-type "vote")
      VOTE-REWARD
      (if (is-eq activity-type "zone-creation")
        ZONE-CREATION-REWARD
        (if (is-eq activity-type "community-action")
          COMMUNITY-ACTION-REWARD
          (if (is-eq activity-type "education")
            EDUCATION-REWARD
            BASE-REWARD-AMOUNT
          )
        )
      )
    )
  )
)

;; Calculate streak bonus
(define-private (calculate-streak-bonus (streak uint))
  (if (<= streak u0)
    u100
    (+ u100 (* streak u10)) ;; 10% bonus per streak day
  )
)

;; Calculate new streak
(define-private (calculate-new-streak (user-stats (tuple (total-activities uint) (current-streak uint) (longest-streak uint) (last-activity-block uint) (total-rewards-earned uint) (engagement-level (string-ascii 15)) (weekly-activities uint) (community-rank uint))) (current-block uint))
  (let
    (
      (last-block (get last-activity-block user-stats))
      (block-diff (- current-block last-block))
    )
    (if (<= last-block u0)
      u1
      (if (<= block-diff u288) ;; within 48 hours
        (+ (get current-streak user-stats) u1)
        u1 ;; reset streak
      )
    )
  )
)

;; Check if activity is within current week
(define-private (is-within-week (last-block uint) (current-block uint))
  (<= (- current-block last-block) WEEKLY-PERIOD)
)

;; Calculate engagement level
(define-private (calculate-engagement-level (total-activities uint))
  (if (>= total-activities u100)
    "expert"
    (if (>= total-activities u50)
      "advanced"
      (if (>= total-activities u20)
        "intermediate"
        (if (>= total-activities u5)
          "active"
          "beginner"
        )
      )
    )
  )
)

;; Mint community tokens to user
(define-private (mint-community-tokens (recipient principal) (amount uint))
  (let
    (
      (current-balance (default-to u0 (map-get? user-token-balances recipient)))
    )
    (map-set user-token-balances recipient (+ current-balance amount))
    true
  )
)

;; Create weekly challenge
(define-public (create-weekly-challenge (week uint) (challenge-type (string-ascii 30)) (target-value uint) (reward-pool uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> reward-pool u0) ERR-INVALID-PARAMETERS)
    
    (map-set weekly-challenges
      { week: week }
      {
        challenge-type: challenge-type,
        target-value: target-value,
        reward-pool: reward-pool,
        participants: u0,
        completed-by: u0,
        active: true
      }
    )
    (ok true)
  )
)

;; Complete educational content
(define-public (complete-education-content (content-id uint) (score uint))
  (let
    (
      (existing-progress (map-get? education-progress { user: tx-sender, content-id: content-id }))
    )
    (asserts! (is-none existing-progress) ERR-ALREADY-CLAIMED)
    (asserts! (and (>= score u0) (<= score u100)) ERR-INVALID-PARAMETERS)
    
    (map-set education-progress
      { user: tx-sender, content-id: content-id }
      {
        completed-at: stacks-block-height,
        score: score,
        reward-claimed: false
      }
    )
    
    ;; Award education reward if score is above threshold
    (if (>= score u70)
      (record-activity "education" content-id)
      (ok u0)
    )
  )
)

;; Transfer tokens between users
(define-public (transfer-tokens (recipient principal) (amount uint))
  (let
    (
      (sender-balance (default-to u0 (map-get? user-token-balances tx-sender)))
    )
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-TOKENS)
    (asserts! (not (is-eq tx-sender recipient)) ERR-INVALID-PARAMETERS)
    
    (map-set user-token-balances tx-sender (- sender-balance amount))
    (mint-community-tokens recipient amount)
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-user-engagement (user principal))
  (default-to
    { total-activities: u0, current-streak: u0, longest-streak: u0, last-activity-block: u0, total-rewards-earned: u0, engagement-level: "beginner", weekly-activities: u0, community-rank: u0 }
    (map-get? user-engagement-stats user)
  )
)

(define-read-only (get-user-token-balance (user principal))
  (default-to u0 (map-get? user-token-balances user))
)

(define-read-only (get-activity-reward-info (user principal) (activity-id uint))
  (map-get? activity-rewards { user: user, activity-id: activity-id })
)

(define-read-only (get-weekly-challenge (week uint))
  (map-get? weekly-challenges { week: week })
)

(define-read-only (get-community-stats)
  {
    total-rewards-distributed: (var-get total-rewards-distributed),
    remaining-token-supply: (var-get community-token-supply),
    current-season: (var-get engagement-season),
    season-start: (var-get season-start-block)
  }
)

(define-read-only (get-education-progress (user principal) (content-id uint))
  (map-get? education-progress { user: user, content-id: content-id })
)

(define-read-only (calculate-user-rank (user principal))
  (let
    (
      (user-stats (get-user-engagement user))
      (activities (get total-activities user-stats))
      (rewards (get total-rewards-earned user-stats))
      (streak (get longest-streak user-stats))
    )
    (ok {
      rank-score: (+ activities (* rewards u10) (* streak u50)),
      level: (get engagement-level user-stats),
      next-level-threshold: (get-next-level-threshold (get engagement-level user-stats))
    })
  )
)

;; Get next level threshold
(define-private (get-next-level-threshold (current-level (string-ascii 15)))
  (if (is-eq current-level "beginner")
    u5
    (if (is-eq current-level "active")
      u20
      (if (is-eq current-level "intermediate")
        u50
        (if (is-eq current-level "advanced")
          u100
          u999999 ;; expert max
        )
      )
    )
  )
)

;; Admin functions
(define-public (start-new-season)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set engagement-season (+ (var-get engagement-season) u1))
    (var-set season-start-block stacks-block-height)
    (ok true)
  )
)

(define-public (adjust-token-supply (new-supply uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set community-token-supply new-supply)
    (ok true)
  )
)

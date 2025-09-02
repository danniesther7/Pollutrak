;; Environmental Impact Scoring System
;; Quantifies pollution effects and tracks cumulative environmental damage

;; Error constants
(define-constant err-unauthorized (err u700))
(define-constant err-not-found (err u701))
(define-constant err-invalid-parameters (err u702))
(define-constant err-impact-calculated (err u703))
(define-constant err-insufficient-data (err u704))

;; Configuration constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant IMPACT-CALCULATION-WINDOW u144) ;; 24 hours in blocks
(define-constant CRITICAL-IMPACT-THRESHOLD u80)
(define-constant HIGH-IMPACT-THRESHOLD u60)
(define-constant BASELINE-ENVIRONMENTAL_HEALTH u100)

;; Impact type multipliers for different pollution types
(define-constant WATER-POLLUTION-MULTIPLIER u120)
(define-constant AIR-POLLUTION-MULTIPLIER u110)
(define-constant SOIL-POLLUTION-MULTIPLIER u100)
(define-constant NOISE-POLLUTION-MULTIPLIER u80)
(define-constant CHEMICAL-POLLUTION-MULTIPLIER u150)

;; Data variables
(define-data-var impact-scoring-enabled bool true)
(define-data-var last-impact-id uint u0)
(define-data-var global-impact-score uint u0)

;; Individual pollution impact records
(define-map impact-assessments
  { report-id: uint }
  {
    base-impact-score: uint,
    severity-multiplier: uint,
    area-affected: uint,
    duration-impact: uint,
    cumulative-score: uint,
    pollution-category: (string-ascii 20),
    assessment-timestamp: uint,
    environmental-threat-level: (string-ascii 15)
  }
)

;; Zone cumulative impact tracking
(define-map zone-impact-scores
  { zone-id: uint }
  {
    current-impact-level: uint,
    total-cumulative-impact: uint,
    environmental-health-rating: uint,
    impact-trend: (string-ascii 20),
    last-assessment: uint,
    critical-incidents: uint,
    recovery-progress: uint
  }
)

;; Source environmental impact tracking
(define-map source-impact-records
  { source-id: uint }
  {
    lifetime-impact-score: uint,
    recent-impact-trend: uint,
    environmental-efficiency: uint,
    improvement-rate: uint,
    impact-category: (string-ascii 20),
    last-updated: uint
  }
)

;; Time-based impact trends
(define-map impact-trend-data
  { period: uint }
  {
    average-impact-score: uint,
    incident-count: uint,
    worst-impact-score: uint,
    improvement-indicators: uint,
    environmental-resilience: uint
  }
)

;; Impact calculation certificates
(define-map impact-certificates
  { certificate-id: uint }
  {
    entity-type: (string-ascii 10), ;; "zone" or "source"
    entity-id: uint,
    impact-score: uint,
    certification-period: uint,
    issued-at: uint,
    valid-until: uint,
    certification-level: (string-ascii 20)
  }
)

;; Calculate pollution impact score
(define-public (calculate-pollution-impact (report-id uint) (affected-area-km uint))
  (let
    (
      ;; For demonstration, we'll use a simplified impact calculation
      ;; In real implementation, this would integrate with the main Pollutrak contract
      (base-severity u5) ;; This would come from the actual report
      (pollution-type "Water Pollution") ;; This would come from the actual report
      (multiplier (get-pollution-multiplier pollution-type))
      (area-factor (if (> affected-area-km u10) u150 (+ u100 (* affected-area-km u5))))
      (base-impact (* base-severity u10))
      (adjusted-impact (/ (* base-impact multiplier area-factor) u10000))
      (threat-level (get-threat-level adjusted-impact))
    )
    (asserts! (var-get impact-scoring-enabled) err-unauthorized)
    (asserts! (> affected-area-km u0) err-invalid-parameters)
    
    (map-set impact-assessments
      { report-id: report-id }
      {
        base-impact-score: base-impact,
        severity-multiplier: multiplier,
        area-affected: affected-area-km,
        duration-impact: u100, ;; Default duration factor
        cumulative-score: adjusted-impact,
        pollution-category: pollution-type,
        assessment-timestamp: stacks-block-height,
        environmental-threat-level: threat-level
      }
    )
    
    ;; Update global impact score
    (var-set global-impact-score (+ (var-get global-impact-score) adjusted-impact))
    (ok adjusted-impact)
  )
)

;; Get pollution type multiplier
(define-private (get-pollution-multiplier (pollution-type (string-ascii 20)))
  (if (is-eq pollution-type "Water Pollution")
    WATER-POLLUTION-MULTIPLIER
    (if (is-eq pollution-type "Air Pollution")
      AIR-POLLUTION-MULTIPLIER
      (if (is-eq pollution-type "Soil Pollution")
        SOIL-POLLUTION-MULTIPLIER
        (if (is-eq pollution-type "Chemical Pollution")
          CHEMICAL-POLLUTION-MULTIPLIER
          NOISE-POLLUTION-MULTIPLIER
        )
      )
    )
  )
)

;; Determine environmental threat level
(define-private (get-threat-level (impact-score uint))
  (if (>= impact-score CRITICAL-IMPACT-THRESHOLD)
    "critical"
    (if (>= impact-score HIGH-IMPACT-THRESHOLD)
      "high"
      (if (>= impact-score u30)
        "moderate"
        "low"
      )
    )
  )
)

;; Update zone cumulative impact
(define-public (update-zone-impact (zone-id uint) (report-id uint))
  (let
    (
      (zone-impact (default-to
        { current-impact-level: u0, total-cumulative-impact: u0, environmental-health-rating: BASELINE-ENVIRONMENTAL_HEALTH, impact-trend: "stable", last-assessment: u0, critical-incidents: u0, recovery-progress: u0 }
        (map-get? zone-impact-scores { zone-id: zone-id })
      ))
      (report-impact (map-get? impact-assessments { report-id: report-id }))
    )
    (match report-impact
      impact-data
        (let
          (
            (new-cumulative (+ (get total-cumulative-impact zone-impact) (get cumulative-score impact-data)))
            (new-health-rating (calculate-health-rating (get total-cumulative-impact zone-impact) (get cumulative-score impact-data)))
            (is-critical (>= (get cumulative-score impact-data) CRITICAL-IMPACT-THRESHOLD))
          )
          (map-set zone-impact-scores
            { zone-id: zone-id }
            {
              current-impact-level: (get cumulative-score impact-data),
              total-cumulative-impact: new-cumulative,
              environmental-health-rating: new-health-rating,
              impact-trend: (calculate-trend (get total-cumulative-impact zone-impact) new-cumulative),
              last-assessment: stacks-block-height,
              critical-incidents: (if is-critical (+ (get critical-incidents zone-impact) u1) (get critical-incidents zone-impact)),
              recovery-progress: (calculate-recovery-progress new-health-rating)
            }
          )
          (ok new-cumulative)
        )
      (err err-not-found)
    )
  )
)

;; Calculate environmental health rating
(define-private (calculate-health-rating (old-cumulative uint) (new-impact uint))
  (let
    (
      (impact-decay u5) ;; Natural recovery factor
      (current-health BASELINE-ENVIRONMENTAL_HEALTH)
      (damage-factor (/ new-impact u10))
    )
    (if (> damage-factor impact-decay)
      (if (> current-health damage-factor) (- current-health damage-factor) u0)
      (if (< current-health BASELINE-ENVIRONMENTAL_HEALTH) (+ current-health impact-decay) current-health)
    )
  )
)

;; Calculate impact trend
(define-private (calculate-trend (old-impact uint) (new-impact uint))
  (if (> new-impact (+ old-impact u20))
    "worsening"
    (if (< new-impact (- old-impact u10))
      "improving" 
      "stable"
    )
  )
)

;; Calculate recovery progress
(define-private (calculate-recovery-progress (health-rating uint))
  (if (>= health-rating u90)
    u100 ;; Full recovery
    (/ (* health-rating u100) u90) ;; Proportional recovery
  )
)

;; Update source environmental impact
(define-public (update-source-impact (source-id uint) (emission-amount uint) (efficiency-rating uint))
  (let
    (
      (source-impact (default-to
        { lifetime-impact-score: u0, recent-impact-trend: u50, environmental-efficiency: u50, improvement-rate: u0, impact-category: "moderate", last-updated: u0 }
        (map-get? source-impact-records { source-id: source-id })
      ))
      (impact-addition (/ (* emission-amount u50) u1000)) ;; Simplified impact calculation
      (new-lifetime (+ (get lifetime-impact-score source-impact) impact-addition))
      (efficiency-improvement (if (> efficiency-rating (get environmental-efficiency source-impact)) u10 u0))
    )
    (asserts! (> emission-amount u0) err-invalid-parameters)
    (asserts! (<= efficiency-rating u100) err-invalid-parameters)
    
    (map-set source-impact-records
      { source-id: source-id }
      {
        lifetime-impact-score: new-lifetime,
        recent-impact-trend: efficiency-rating,
        environmental-efficiency: efficiency-rating,
        improvement-rate: efficiency-improvement,
        impact-category: (get-impact-category new-lifetime),
        last-updated: stacks-block-height
      }
    )
    (ok new-lifetime)
  )
)

;; Determine impact category based on lifetime score
(define-private (get-impact-category (lifetime-score uint))
  (if (>= lifetime-score u1000)
    "high-impact"
    (if (>= lifetime-score u500)
      "moderate-impact"
      "low-impact"
    )
  )
)

;; Generate impact certificate
(define-public (generate-impact-certificate (entity-type (string-ascii 10)) (entity-id uint) (certification-period uint))
  (let
    (
      (new-cert-id (+ (var-get last-impact-id) u1))
      (current-block stacks-block-height)
      (impact-score (get-entity-impact-score entity-type entity-id))
      (cert-level (get-certification-level impact-score))
    )
    (asserts! (> certification-period u0) err-invalid-parameters)
    (asserts! (or (is-eq entity-type "zone") (is-eq entity-type "source")) err-invalid-parameters)
    
    (var-set last-impact-id new-cert-id)
    (map-set impact-certificates
      { certificate-id: new-cert-id }
      {
        entity-type: entity-type,
        entity-id: entity-id,
        impact-score: impact-score,
        certification-period: certification-period,
        issued-at: current-block,
        valid-until: (+ current-block certification-period),
        certification-level: cert-level
      }
    )
    (ok new-cert-id)
  )
)

;; Get entity impact score
(define-private (get-entity-impact-score (entity-type (string-ascii 10)) (entity-id uint))
  (if (is-eq entity-type "zone")
    (let
      (
        (zone-data (default-to 
          { current-impact-level: u0, total-cumulative-impact: u0, environmental-health-rating: u100, impact-trend: "stable", last-assessment: u0, critical-incidents: u0, recovery-progress: u0 }
          (map-get? zone-impact-scores { zone-id: entity-id })
        ))
      )
      (get total-cumulative-impact zone-data)
    )
    (let
      (
        (source-data (default-to
          { lifetime-impact-score: u0, recent-impact-trend: u50, environmental-efficiency: u50, improvement-rate: u0, impact-category: "moderate", last-updated: u0 }
          (map-get? source-impact-records { source-id: entity-id })
        ))
      )
      (get lifetime-impact-score source-data)
    )
  )
)

;; Get certification level based on impact score
(define-private (get-certification-level (impact-score uint))
  (if (< impact-score u100)
    "green-certified"
    (if (< impact-score u500)
      "yellow-watch"
      "red-alert"
    )
  )
)

;; Read-only functions
(define-read-only (get-impact-assessment (report-id uint))
  (map-get? impact-assessments { report-id: report-id })
)

(define-read-only (get-zone-impact-score (zone-id uint))
  (map-get? zone-impact-scores { zone-id: zone-id })
)

(define-read-only (get-source-impact-record (source-id uint))
  (map-get? source-impact-records { source-id: source-id })
)

(define-read-only (get-impact-certificate (certificate-id uint))
  (map-get? impact-certificates { certificate-id: certificate-id })
)

(define-read-only (get-global-impact-summary)
  (ok {
    global-impact-score: (var-get global-impact-score),
    total-assessments: (var-get last-impact-id),
    scoring-enabled: (var-get impact-scoring-enabled),
    baseline-health: BASELINE-ENVIRONMENTAL_HEALTH
  })
)

;; Get environmental health status for a zone
(define-read-only (get-environmental-health-status (zone-id uint))
  (let
    (
      (zone-impact (map-get? zone-impact-scores { zone-id: zone-id }))
    )
    (match zone-impact
      impact-data (ok {
        health-rating: (get environmental-health-rating impact-data),
        health-status: (get-health-status (get environmental-health-rating impact-data)),
        recovery-progress: (get recovery-progress impact-data),
        trend: (get impact-trend impact-data),
        last-updated: (get last-assessment impact-data)
      })
      (ok {
        health-rating: BASELINE-ENVIRONMENTAL_HEALTH,
        health-status: "excellent",
        recovery-progress: u100,
        trend: "stable",
        last-updated: u0
      })
    )
  )
)

;; Get health status description
(define-private (get-health-status (rating uint))
  (if (>= rating u90)
    "excellent"
    (if (>= rating u70)
      "good"
      (if (>= rating u50)
        "fair"
        (if (>= rating u30)
          "poor"
          "critical"
        )
      )
    )
  )
)

;; Update impact trend for a time period
(define-public (update-period-impact-trend (period uint))
  (let
    (
      (current-trend (default-to
        { average-impact-score: u0, incident-count: u0, worst-impact-score: u0, improvement-indicators: u0, environmental-resilience: u100 }
        (map-get? impact-trend-data { period: period })
      ))
      (period-incidents (+ (get incident-count current-trend) u1))
    )
    (map-set impact-trend-data
      { period: period }
      {
        average-impact-score: (/ (var-get global-impact-score) (if (> period-incidents u0) period-incidents u1)),
        incident-count: period-incidents,
        worst-impact-score: (get worst-impact-score current-trend),
        improvement-indicators: (get improvement-indicators current-trend),
        environmental-resilience: (calculate-resilience period-incidents)
      }
    )
    (ok true)
  )
)

;; Calculate environmental resilience
(define-private (calculate-resilience (incident-count uint))
  (if (< incident-count u5)
    u100
    (if (< incident-count u15)
      u75
      (if (< incident-count u30)
        u50
        u25
      )
    )
  )
)

;; Admin functions
(define-public (toggle-impact-scoring (enabled bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) err-unauthorized)
    (var-set impact-scoring-enabled enabled)
    (ok true)
  )
)

(define-public (reset-global-impact-score)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) err-unauthorized)
    (var-set global-impact-score u0)
    (ok true)
  )
)

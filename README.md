# 🌍 Pollutrak - Pollution Reporting DAO

A decentralized autonomous organization for crowdsourced pollution reporting with consensus-based validation on the Stacks blockchain.

## 📋 Overview

Pollutrak enables communities to report environmental pollution incidents and validate them through a decentralized consensus mechanism. Users stake STX tokens to participate, vote on report validity, and earn rewards for accurate reporting and validation.

## ✨ Features

- 🏭 **Pollution Reporting**: Submit detailed pollution reports with location, type, and severity
- 🗳️ **Consensus Voting**: Community validates reports through stake-weighted voting
- 💰 **Reward System**: Earn STX rewards for accurate reporting and validation
- 📊 **Reputation Tracking**: Build reputation through consistent accurate participation
- 🔒 **Stake-based Security**: Minimum stake requirements prevent spam and ensure quality

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- STX tokens for staking and participation

### Installation

```bash
clarinet new pollutrak-project
cd pollutrak-project
```

Copy the contract code to `contracts/Pollutrak.clar`

### Testing

```bash
clarinet test
```

### Deployment

```bash
clarinet deploy
```

## 📖 Usage Guide

### 1. Stake Tokens 💎

Before participating, users must stake a minimum of 1 STX:

```clarity
(contract-call? .Pollutrak stake-tokens u1000000)
```

### 2. Submit Pollution Report 📝

Report pollution incidents with detailed information:

```clarity
(contract-call? .Pollutrak submit-report 
  "Downtown River, Block 15" 
  "Water Pollution" 
  u7 
  "Industrial waste discharge observed with visible contamination")
```

**Parameters:**
- `location`: Geographic location (max 100 chars)
- `pollution-type`: Type of pollution (max 50 chars)  
- `severity`: Scale 1-10 (10 being most severe)
- `description`: Detailed description (max 500 chars)

### 3. Vote on Reports 🗳️

Community members vote to validate reports during the voting period:

```clarity
(contract-call? .Pollutrak vote-on-report u1 true)
```

- `report-id`: ID of the report to vote on
- `approve`: `true` to validate, `false` to reject

### 4. Finalize Reports ✅

After voting period ends, anyone can finalize the report:

```clarity
(contract-call? .Pollutrak finalize-report u1)
```

### 5. Claim Rewards 🎁

**Validators** who voted correctly:
```clarity
(contract-call? .Pollutrak claim-validator-reward u1)
```

**Reporters** whose reports were validated:
```clarity
(contract-call? .Pollutrak claim-reporter-reward u1)
```

## 🔍 Read-Only Functions

### Get Report Details
```clarity
(contract-call? .Pollutrak get-report u1)
```

### Check User Reputation
```clarity
(contract-call? .Pollutrak get-user-reputation 'SP1234...)
```

### View Contract Statistics
```clarity
(contract-call? .Pollutrak get-contract-stats)
```

### Check Voting Eligibility
```clarity
(contract-call? .Pollutrak can-vote u1 'SP1234...)
```

## ⚙️ Configuration

| Parameter | Value | Description |
|-----------|-------|-------------|
| `MIN_STAKE` | 1 STX | Minimum stake to participate |
| `VOTING_PERIOD` | 144 blocks | ~24 hours# Pollutrak


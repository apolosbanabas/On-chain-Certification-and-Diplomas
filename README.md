# On-chain Certification and Diplomas
A decentralized solution for issuing and managing academic certificates on the Stacks blockchain.

## 🚀 Features

- Issue tamper-proof academic certificates as NFTs
- Register verified educational institutions
- Transfer certificates between wallets
- Revoke certificates when necessary
- View certificate details and verification status

## 📝 Contract Functions

### For Contract Owner
- `register-institution`: Register a new educational institution
- `set-institution-admin`: Set a new administrator
- `revoke-certificate`: Revoke an issued certificate

### For Institutions
- `issue-certificate`: Issue a new certificate to a recipient

### For Certificate Holders
- `transfer-certificate`: Transfer certificate ownership
- `get-certificate-by-id`: View certificate details

## 🔧 Usage

1. Deploy the contract using Clarinet
2. Register your institution using the contract owner address
3. Issue certificates to student wallet addresses
4. Students can verify and transfer their certificates

## 🔍 Verification

Each certificate contains:
- Recipient wallet address
- Issuing institution
- Course name
- Issue date
- Unique certificate hash
- Verification status

## 💻 Development

Built with Clarity and Clarinet for the Stacks blockchain ecosystem.
```


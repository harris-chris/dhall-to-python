let PhoneNumber = {
    country_code : Natural
    , number : Text
}

let Email = {
    address : Text
}

let ContactDetails = {
    phone_number : PhoneNumber
    , email : Email
}

let ContactDetailsPackage = {
    ContactDetails
} in ContactDetailsPackage


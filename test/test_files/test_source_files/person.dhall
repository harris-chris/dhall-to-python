let PhoneNumberPackage = ./phone_number.dhall

let Person = {
    name : Text
    , phone_number : PhoneNumberPackage.PhoneNumber
}

let PersonPackage = {
    Person
} in PersonPackage


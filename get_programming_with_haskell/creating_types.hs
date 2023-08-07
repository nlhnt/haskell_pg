type FirstName = String
type LastName = String
type Age = Int
type Height = Float
type Weight = Float
type PatientName = (String, String)

patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fn ln age height = name ++ " " ++ ageHeight
    where
        name = ln ++ ", " ++ fn
        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"


firstName :: PatientName -> String
-- firstname patient = fst patient
firstName = fst

lastName :: PatientName -> String
-- lastName patient = snd patient
lastName = snd

patientInfoV2 :: PatientName -> Age -> Height -> String
patientInfoV2 pn age height = name ++ " " ++ ageHeight
    where
        name = lastName pn ++ ", " ++ firstName pn
        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

testPatientName :: PatientName = ("John", "Doe")

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg

data ABOType = A | B | AB | O

-- Data constructor BloodType is made by combining n ABOType and an RhType
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh)  = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --otherwise

-- refactor the names
type MiddleName = String
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

data Patient = Patient Name Sex Age Height Weight BloodType

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeESmith :: Patient
janeESmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith")
                      Female 28 62 140 (BloodType AB Pos)

-- record syntax 
-- no need to create our own getters anymore, cool
data PatientV2 = PatientV2 {
    name :: Name,
    sex :: Sex,
    age :: Age,
    height :: Height,
    weight :: Weight,
    bloodType :: BloodType 
}

jackieSmith :: PatientV2
jackieSmith = PatientV2 {
    name = Name "Jackie" "Smith",
    age = 43,
    sex = Female,
    height = 62,
    weight = 115,
    bloodType = BloodType O Neg
}

-- GHCi> height jackieSmith
-- 62
-- GHCi> showBloodType (bloodType jackieSmith)
-- "O-"

-- ghci> jackieSmithUpdated = jackieSmith { age = 44 }
-- ghci> show (age jackieSmithUpdated)
-- "44"



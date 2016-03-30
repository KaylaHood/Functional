--Kayla Hood and Miguel Reyes, October 10th 2015

import Data.List
import CSCICourses
type CID = String

-- Problem 1 (??? is this it ???)

data Group = Design | Applications | Systems | Standard deriving (Eq, Show)

--is this the real life...

--Problem 2

data Class = Class { ident :: String, hours :: Int, prereqs :: [String] } deriving (Eq, Show) -- the prerequisite is 0 if no prereqs exist

data Course = Course { group_type :: Group, cls :: Class } deriving (Eq, Show)

--is this just fantasy...

--Problem 3

readCourse :: String -> Course
readCourse s = if(group == "None") then Course { group_type = Standard, cls = (Class { ident = id, hours = cred, prereqs = prereq }) }
               else if(group == "Design") then Course { group_type = Design, cls = (Class { ident = id, hours = cred, prereqs = prereq }) }
               else if(group == "Applications") then Course { group_type = Applications, cls = (Class { ident = id, hours = cred, prereqs = prereq }) }
               else if(group == "Systems") then Course { group_type = Systems, cls = (Class { ident = id, hours = cred, prereqs = prereq}) }
               else Course { group_type = Standard, cls = (Class { ident = id, hours = cred, prereqs = prereq }) }
               where 
                   id = ((words s) !! 0)
                   cred = read ((words s) !! 1)
                   group = ((words s) !! 2)
                   prereq = if((length (words s)) > 3) then (drop 3 (words s)) else []

--caught in a landslide...

--Problem 4

data Curriculum = Curriculum { required_courses :: [Course],
                               required_groups :: [Group],
                               hours_needed ::  Integer
                             } deriving (Show)

--no escape from reality...

--Problem 5

_LINES = (lines courses)

getCourses :: [Int] -> [Course]
getCourses [] = []
getCourses xs = map (readCourse) lst
                where sxs = (map show xs) 
                      lst = (filter (\n -> (take 4 n) `elem` sxs) _LINES)

csciBach = Curriculum { required_courses = (getCourses [1120,1320,1323,2320,2321,2322,3320,3321,3322]), required_groups = [Design, Applications, Systems], hours_needed = 49 }

csciC2M = Curriculum { required_courses = (getCourses [1120,1320,1321,1323,2320]), required_groups = [Standard], hours_needed = 34 }

--open your eyes, look up to the skies...

--Problem 6

data Transcript = Transcript { taken_courses :: [Course] } deriving (Show)

frances = Transcript (getCourses [1321,1120])
sally = Transcript ((Course Design (Class "3193" 19 ["1320"]) ) : (getCourses [1120,1320,1321,1323,2320,3345]))

--and seeeeeeeee...

--Problem 7

canTake :: Transcript -> Course -> Bool
canTake (Transcript{ taken_courses = xs })  y = (any ( == y) (xs))

--I'm just a poor boy...

--Problem 8

getHours :: Course -> Int
getHours Course{cls = Class{hours = x}} = fromIntegral (x)

addHours :: [Course] -> Int
addHours [] = 0
addHours (x:xs) = (getHours x) + (addHours xs)

hoursTaken :: Transcript -> Int
hoursTaken (Transcript{ taken_courses = xs }) = (addHours xs)

--I need no sympathy...

--Problem 9

--getGroup :: Course -> Group
--getGroup y = group_type

groupsTaken :: Transcript -> [Group]
groupsTaken Transcript{taken_courses = []} = []
groupsTaken Transcript{taken_courses = ((Course{group_type = x}):xs)} = x:(groupsTaken (Transcript xs))

--'Cause it's easy come, easy go...

--Problem 10

canGraduate :: Curriculum -> Transcript -> Bool
canGraduate Curriculum{required_courses = xs, required_groups = ys} Transcript{taken_courses = zs} =
    ((all (\x -> x `elem` zs) xs) && (all (\p -> any (\Course{group_type =j} -> j == p) zs) ys))

--little high, little low...

--Problem 11

showCourse :: Course -> String
showCourse Course{group_type = x, cls = y} = "Course group: " ++ (show x) ++ " Class: " ++ (show y)

showCourses :: [Course] -> String
showCourses [] = ""
showCourses (x:xs) = (showCourse x) ++ (showCourses xs)

getID :: Course -> String
getID Course{cls = Class{ident = x}} = x

getIDs :: [Course] -> [String]
getIDs xs = map getID xs

diffCourses :: [Course] -> [Course] -> [Course]
diffCourses [] [] = []
diffCourses xs ys = filter (\x -> (getID x) `notElem` (getIDs ys)) xs

missing :: Curriculum -> Transcript -> String
missing Curriculum{required_courses = xs, required_groups = ys, hours_needed = hs} Transcript{taken_courses = zs} = 
    let
        courses_needed = diffCourses xs zs
        groups_fulfilled = groupsTaken (Transcript zs)
        groups_needed = ys \\ (groupsTaken (Transcript zs))
        hours_left = (fromIntegral (hs)) - (hoursTaken (Transcript zs))
    in
        "Remaining courses: " ++ (showCourses (courses_needed)) ++ " remaining groups: " ++ (show (groups_needed)) ++ " hours needed to complete degree: " ++ (show (hours_left))
        


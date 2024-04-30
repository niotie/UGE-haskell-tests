module FileServer where
    
import qualified Data.List as L
import qualified Data.Map.Strict as M

-- File 
type FileName = String

type FileSize = Int

data File = File { fName :: FileName, fSize :: FileSize }

instance Show File where
  show f = fName f ++ " (" ++ show (fSize f) ++ ")"

-- Disk
data DiskStatus = Online | Offline deriving (Eq, Show)

type DiskName = String

type DiskCapacity = Int

data Disk = Disk { dName     :: DiskName
                 , dStatus   :: DiskStatus
                 , dCapacity :: DiskCapacity
                 , dIndex    :: M.Map FileName File }

-- uncomment once dLoad is implemented
-- instance Show Disk where
--   show d = "name="      ++ show (dName     d) ++
--            ",status="   ++ show (dStatus   d) ++
--            ",capacity=" ++ show (dCapacity d) ++
--            ",load="     ++ show (dLoad     d) ++
--            ",files=["   ++ L.intercalate "," (fmap show fs) ++ "]"
--     where
--       fs = M.elems (dIndex d)

instance Eq Disk where
  d1 == d2 = dName d1 == dName d2

-- File server 
data FileServer = FileServer { fsDisks :: M.Map DiskName Disk
                             , fsIndex :: M.Map FileName DiskName }


-- uncomment once required functions are implemented
-- instance Show FileServer where
--   show fs =
--     "file server = ["    ++
--     "number of files="   ++ show (fsCountFilesAPI fs) ++
--     ",total size="       ++ show (fsTotalSizeAPI  fs) ++
--     ",average load="     ++ show (fsLoadAPI       fs) ++
--     ",disks=[\n"                                    ++
--     L.intercalate ",\n" d ++ "\n]]"
--     where
--       d = ["  " ++ show dn ++ "=[" ++ show d ++ "]" | (dn, d) <- M.assocs (fsDisks fs)]

-- functions

-- file

-- smart file constructor
fMk :: FileName -> FileSize -> File
fMk n s = File { fName = n, fSize = s }

-- disk

-- smart disk constructor (disks are created offline)
dMk :: DiskName -> DiskCapacity -> Disk
dMk n c = Disk { dName = n, dStatus = Offline, dCapacity = c, dIndex = M.empty } 

dFiles :: Disk -> [File]
dFiles = M.elems . dIndex

-- file server

-- empty file server (no disk and no file)
fsEmpty :: FileServer
fsEmpty = FileServer { fsIndex = M.empty, fsDisks = M.empty }

-- return the number of disks
fsCountDisksAPI :: FileServer -> Int
fsCountDisksAPI = M.size . fsDisks

-- return all files
fsFiles :: FileServer -> [File]
fsFiles = concatMap dFiles . M.elems . fsDisks

-- return all filenames
fsFileNamesAPI :: FileServer -> [FileName]
fsFileNamesAPI = map fName . fsFiles

-- data

-- unsafefs

f01 = fMk "f01" 10
f02 = fMk "f02" 80
f03 = fMk "f03" 20
f04 = fMk "f04" 10
f05 = fMk "f05" 50
f06 = fMk "f06" 80

fsDisk01 = fmap (\f -> (fName f, f)) [f01, f02, f04]
d01 = Disk { dName = "disk01", dStatus = Online, dCapacity = 100, dIndex = M.fromList fsDisk01 }

fsDisk02 = fmap (\f -> (fName f, f)) [f03]
d02 = Disk { dName = "disk02", dStatus = Online, dCapacity =  50, dIndex = M.fromList fsDisk02 }

fsDisk03 = fmap (\f -> (fName f, f)) [f05]
d03 = Disk { dName = "disk03", dStatus = Online, dCapacity =  80, dIndex = M.fromList fsDisk03 }

fsDisk04 = fmap (\f -> (fName f, f)) [f06]
d04 = Disk { dName = "disk04", dStatus = Online, dCapacity = 100, dIndex = M.fromList fsDisk04 }

unsafefs :: FileServer
unsafefs = FileServer { fsIndex = M.fromList (fsDisk01' ++ fsDisk02' ++ fsDisk03' ++ fsDisk04')
                , fsDisks = M.fromList [("disk01", d01), ("disk02", d02), ("disk03", d03), ("disk04", d04)] }
  where
    fsDisk01' = fmap (\f -> (fName f, "disk01")) [f01, f02, f04]
    fsDisk02' = fmap (\f -> (fName f, "disk01")) [f03]
    fsDisk03' = fmap (\f -> (fName f, "disk01")) [f05]
    fsDisk04' = fmap (\f -> (fName f, "disk01")) [f06]

-- uncomment once fsAddDisk and fsSetAllDisksOnline are implemented

-- somes files
-- f01 = fMk "f01" 10
-- f02 = fMk "f02" 80
-- f03 = fMk "f03" 20
-- f04 = fMk "f04" 10
-- f05 = fMk "f05" 50
-- f06 = fMk "f06" 80
-- f07 = fMk "f07" 20
-- f08 = fMk "f08"  5
-- f09 = fMk "f09" 10
-- f10 = fMk "f10" 40

-- some disks
-- d01 = dMk "disk01" 100
-- d02 = dMk "disk02"  50
-- d03 = dMk "disk03"  80
-- d04 = dMk "disk04" 100

-- fs :: Maybe FileServer
-- fs = fsAddDisk d01 fsEmpty      >>= 
--      fsAddDisk d02              >>= 
--      fsAddDisk d03              >>= 
--      fsAddDisk d04              >>=
--      Just . fsSetAllDisksOnline >>=
--      fsAddFileAPI f01           >>=
--      fsAddFileAPI f02           >>=
--      fsAddFileAPI f03           >>=
--      fsAddFileAPI f04           >>=
--      fsAddFileAPI f05           >>= 
--      fsAddFileAPI f06



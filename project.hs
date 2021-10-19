data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)

convertBinToDec x = (convertBinToDec2 x 0)
convertBinToDec2 0 _ = 0
convertBinToDec2 x c = 2 ^ c * (mod x 10) + convertBinToDec2 (div x 10) (c + 1)

replaceIthItem _ [] _ = []
replaceIthItem item l index = replaceIthItem2 item l index 0
replaceIthItem2 item (x:xs) index c | c == index = item:xs		
								    | otherwise = x:(replaceIthItem2 item xs index (c+1))
						
splitEvery n l = splitEvery2 n l [] 0
splitEvery2 n [] y c = [y]
splitEvery2 n (x:xs) y c | c < n = splitEvery2 n xs (y ++ [x]) (c+1)
						 | otherwise = [y] ++ (splitEvery2 n (x:xs) [] 0)
						
logBase2 x = logBase 2 x

getNumBits numOfSets "fullyAssoc" l = 0
getNumBits numOfSets "setAssoc" l = ceiling (logBase2 numOfSets)
getNumBits numOfSets "directMap" l = ceiling (logBase2 (len l)) 

fillZeros s 0 = s
fillZeros s n = "0" ++ fillZeros s (n-1) 

len [] = 0
len (x:xs) = 1 + len xs

flatten [] = []
flatten (x:xs) = x ++ flatten xs

getDataFromCache stringAddress cache "directMap" bitsNum 
	= output (cache !! i) (div (read stringAddress) (10^bitsNum)) 0 
	where i = convertBinToDec (mod (read stringAddress) (10^bitsNum))

getDataFromCache stringAddress cache "setAssoc" bitsNum 
	= outt ((splitCache cache bitsNum) !! i) (div (read stringAddress) (10^bitsNum)) 0 
	where i = convertBinToDec (mod (read stringAddress) (10^bitsNum))
	
getDataFromCache stringAddress cache "fullyAssoc" bitsNum = fullyAssoc_helper (read stringAddress) cache 0

fullyAssoc_helper _ [] _ = NoOutput
fullyAssoc_helper tag ((It (T t) (D d) f _):xs) cur | t == tag && f = Out (d,cur)
								 | otherwise = fullyAssoc_helper tag xs (cur+1)

convertAddress x n "fullyAssoc" = (x,0)
convertAddress binAddress bitsNum "setAssoc" = (div binAddress (10^bitsNum),mod binAddress (10^bitsNum))
convertAddress binAddress bitsNum "directMap" = (div binAddress (10^bitsNum),mod binAddress (10^bitsNum))

replaceInCache tag idx memory oldCache "directMap" bitsNum 
	= (memory !! x, replace_direct tag (convertBinToDec idx) 0 (memory !! x) oldCache) 
	where x = convertBinToDec (read ((fillZeros (show tag) (6-bitsNum-(len (show tag)))) ++ (fillZeros (show idx) (bitsNum-(len (show idx)))))) 

replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum 
	= (memory !! (convertBinToDec tag),replace_fully oldCache (It (T tag) (D (memory !! (convertBinToDec tag))) True (-1)))

replaceInCache tag idx memory oldCache "setAssoc" bitsNum 
	= ((memory !! x),flatten (replace_set (splitCache oldCache bitsNum) (convertBinToDec idx) 0 (It (T tag) (D (memory !! x)) True (-1))))
	where x = convertBinToDec (read ((fillZeros (show tag) (6-bitsNum-(len (show tag)))) ++ (fillZeros (show idx) (bitsNum-(len (show idx)))))) 

replace_set (x:xs) dec cur item | cur == dec = (replace_fully x item):xs
								| otherwise = x:(replace_set xs dec (cur+1) item)	

replace_direct _ _ _ _ [] = []
replace_direct tag dec cur dat (x:xs) | cur == dec = (It (T tag) (D dat) True 0):xs
									  | otherwise = x:(replace_direct tag dec (cur+1) dat xs)

replace_fully cache item | chk_all_valid cache = inc_order (update_valid cache item)
						 | otherwise = inc_order (update_invalid cache item)

chk_all_valid [] = True
chk_all_valid ((It _ _ valid _):xs) = valid && chk_all_valid xs

inc_order [] = []
inc_order ((It (T t) (D d) valid order):xs) | valid = (It (T t) (D d) valid (order+1)):inc_order xs
											| otherwise = (It (T t) (D d) valid order):inc_order xs
	
update_valid ((It (T t) (D d) valid order):xs) item | order == (maxx (x:xs) 0) = item:xs		
										| otherwise = x:(update_valid xs item)
										where x = (It (T t) (D d) valid order)
maxx [] cur = cur
maxx ((It _ _ _ order):xs) cur | order > cur = maxx xs order
							   | otherwise = maxx xs cur
							   
							   
update_invalid ((It (T t) (D d) valid order):xs) item | valid = (It (T t) (D d) valid order):(update_invalid xs item)
						   | otherwise = item:xs					
											
outt [] _ _ = NoOutput
outt ((It (T t) (D d) f _):xs) stringAddress c | f && t==stringAddress = Out(d,c)
											   | otherwise = outt xs stringAddress (c+1)

output (It (T t) (D d) f _) tag c | f && t == tag = Out (d,c)
								  |otherwise = NoOutput

splitCache cache n = splitEvery (div (len cache) (2^n)) cache

getData stringAddress cache memory cacheType bitsNum
	| x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
	| otherwise = (getX x, cache)
	where
		x = getDataFromCache stringAddress cache cacheType bitsNum
		address = read stringAddress:: Int
		(tag, index) = convertAddress address bitsNum cacheType

getX (Out (d, _)) = d

runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numOfSets =
	((d:prevData), finalCache)
	where
	bitsNum = round(logBase2 numOfSets)
	(d, updatedCache) = getData addr cache memory cacheType bitsNum
	(prevData, finalCache) = runProgram xs updatedCache memory cacheType numOfSets
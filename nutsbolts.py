#http://redd.it/1sob1e

def takeOldInput(nameList, priceList):
	numItems = int(input("Number of items: "))
	print("Current Price:")
	for i in range (1, numItems+1):
		item = input("Name Price: ")
		name, price = item.split()
		price = int(price)
		nameList.append(name)
		priceList.append(price)
	return numItems
		
		
def takeNewInput(numItems, fixNameList, fixPriceList):
	print("New Price:")
	for i in range (1, numItems+1):
		item = input("Name Price: ")
		name, price = item.split()
		price = int(price)
		fixNameList.append(name)
		fixPriceList.append(price)


def priceFix(numItems, nameList, priceList, fixNameList, fixPriceList):
	for i in range (0, numItems):
		for j in range (0, numItems):
			if nameList[i] == fixNameList[j]:
				dif = fixPriceList[j] - priceList[i]
				if dif != 0:
					print(fixNameList[j], dif)
		
def main():
	nameList = [] 
	priceList = []
	fixNameList = []
	fixPriceList = []
	numItems = takeOldInput(nameList, priceList)
	takeNewInput(numItems, fixNameList, fixPriceList)
	priceFix(numItems, nameList, priceList, fixNameList, fixPriceList)
	
	
	
	
main()
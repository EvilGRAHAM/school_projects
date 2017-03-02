class Interest:
	"""This is used to calculate compound interest"""
	
	def __init__(self, period, rate, time):
		self.period = period
		self.rate = rate
		self.time = time
		
	def __iter__(self):
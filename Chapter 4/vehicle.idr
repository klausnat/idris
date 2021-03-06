||| Exercise 1. Extend the Vehicle data type so that it supports unicycles and motorcycles, and update whells and refuel accordingly

data PowerSource = Petrol | Pedal | Electricity

data Vehicle : PowerSource -> Type where -- we define two types in one declaration Vehicle Pedal and Vehicle Petrol
     Unicycle : Vehicle Pedal
     Bicycle : Vehicle Pedal
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
     Motorcycle : (fuel : Nat) -> Vehicle Petrol
     Tram : (power : Nat) -> Vehicle Electricity
     ElectricCar : (power : Nat) -> Vehicle Electricity
     
wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Motorcycle fuel) = 2
wheels (Tram power) = 8
wheels (ElectricCar power) = 8

refuel : Vehicle Petrol -> Vehicle Petrol         
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50
refuel Bicycle impossible
refuel Unicycle impossible
refuel (Tram power) impossible
refuel (ElectricCar power) impossible

recharge : Vehicle Electricity -> Vehicle Electricity
recharge (Tram power) = Tram 100
recharge (ElectricCar power) = ElectricCar 50

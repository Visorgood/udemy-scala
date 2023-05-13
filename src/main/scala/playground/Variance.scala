package playground

object Variance {

  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle

  class IParking[T](things: List[T]) {
    def park(vehicle: T): IParking[T] = ???
    def impound(vehicles: List[T]): IParking[T] = ???
    def checkVehicles(conditions: String): List[T] = ???
    def flatMap[S](f: T => IParking[S]): IParking[S] = ???
  }

  class CParking[+T](things: List[T]) {
    def park[S >: T](vehicle: S): CParking[S] = ???
    def impound[S >: T](vehicles: List[S]): CParking[S] = ???
    def checkVehicles(conditions: String): List[T] = ???
    def flatMap[S](f: T => CParking[S]): CParking[S] = ???
  }

  class XParking[-T](things: List[T]) {
    def park(vehicle: T): XParking[T] = ???
    def impound(vehicles: List[T]): XParking[T] = ???
    def checkVehicles[S <: T](conditions: String): List[S] = ???
    def flatMap[R <: T, S](f: R => XParking[S]): XParking[S] = ???
  }

  /*
    Rule of thumb:
    - Use COVARIANCE if you need a COLLECTION OF THINGS
    - Use CONTRAVARIANCE if you need a GROUP OF ACTIONS
   */
}

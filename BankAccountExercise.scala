package part1Actor

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part1Actor.BankAccountExercise.Person.LiveTheLife
import part1Actor.BankAccountExercise.system

object BankAccountExercise extends App {

  //  Bank Account
  object BankAccount {
    case class Deposit(amount : Int)
    case class Withdraw (amount : Int)
    case object Statement

    case class TransactionSuccessful(message : String)
    case class TransactionFailure(reason: String)
  }
  class BankAccount extends Actor{
    var funds =0

    import BankAccount._

    def receive : Receive={
      case Deposit(amount)=>
        if(amount < 0) sender() ! TransactionFailure ("Invaild deposit amount")
        else {
          funds += amount
          sender() ! TransactionSuccessful(s"Deposited amount is $amount")
        }
      case Withdraw(amount) =>
        if(amount < 0) sender() ! TransactionFailure(" invalid amount")
        else if (amount > funds) sender() ! TransactionFailure(" Insufficient fund")
        else{
          funds -= amount
          sender() ! TransactionSuccessful("successfully withdrew")
        }
      case Statement => sender() ! s"your balance is : $funds"
    }
  }
  object Person {
    case class LiveTheLife(account : ActorRef)
  }
  class Person extends Actor{
    import Person._
    import BankAccount._

    override def receive: Receive ={
      case LiveTheLife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(90000)
        account ! Withdraw(500)
        account ! Statement
      case message =>println(message.toString)
    }
  }
  val system = ActorSystem("BankAccountExercies")
  val account =system.actorOf(Props[BankAccount],"bankAccount")
  val person = system.actorOf(Props[Person],"Billionare")

  person ! LiveTheLife(account)

}

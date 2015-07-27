package com.github.fgeller

import scala.util._

object XMLGenerator extends App {
  val rand = new Random

  def randomInt(length: Int): Int = {
    rand.nextInt(Math.pow(10, length).toInt)
  }

  def randomString(length: Int): String = {
    rand.nextString(length)
  }

  def randomPerson = {
    <Person>
      <Name>
        <FirstName>{ randomString(10) }</FirstName>
        <MiddleName>{ randomString(1) }</MiddleName>
        <LastName>{ randomString(10) }</LastName>
      </Name>
      <Addresses>
        {
          (1 to (rand.nextInt(4))).map(_ ⇒
            <Address>
              <Street>{ randomInt(4) } { randomString(15) }</Street>
              <StreetSecondary>{ randomString(15) }</StreetSecondary>
              <City>{ randomString(15) }</City>
              <State>{ randomString(15) }</State>
              <PostCode>{ randomInt(5) }</PostCode>
              <Country>{ randomString(15) }</Country>
            </Address>)
        }
      </Addresses>
    </Person>

  }

  1 to 2 foreach { _ ⇒
    println(randomPerson)
  }
}

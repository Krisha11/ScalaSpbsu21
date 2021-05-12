package org.spbsu.scala

import org.scalatest.funsuite.AnyFunSuite
import sttp.client3._
import cats.effect.IO
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
import cats.effect.unsafe.implicits.global

import java.io.File
import sttp.client3._
import cats.effect.IO
import org.scalatest.matchers.should.Matchers
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend

import java.nio.file.Files

class HttpClientSpec extends AnyFunSuite with Matchers {
  test("http request from ya.ru returns content") {
    val request = basicRequest
      .get(uri"https://httpbin.org/get")

    val value  = AsyncHttpClientCatsBackend[IO]()
      .flatMap(request.send(_))
      .map(resp => println(resp.body))
    value.unsafeRunSync()
  }

  test("download ical from emkn") {
    val targetFile = new File("classes.ics")
    val request = basicRequest
      .get(uri"https://emkn.ru/users/446/classes.ics")
      .response(asFile(targetFile))

    val value  = AsyncHttpClientCatsBackend[IO]()
      .flatMap(request.send(_))
      .map(resp => println(resp.headers))
    value.unsafeRunSync()
    targetFile.exists() shouldBe true
  }

}

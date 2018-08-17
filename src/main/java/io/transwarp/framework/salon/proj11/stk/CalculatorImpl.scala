package io.transwarp.framework.salon.proj11.stk

import io.transwarp.framework.salon.proj11.stk.TokenType.TokenType
import io.transwarp.framework.salon.proj11.{Calculator, Result}

class CalculatorImpl extends Calculator {
  private var exprChars: Array[Char] = _
  private var exprLen: Int = _
  private var pos: Int = 0
  private var curToken: Token = _

  override def calculate(exp: String): Result = {
    exprChars = exp.toCharArray
    exprLen = exprChars.length
    pos = 0
    curToken = null
    nextToken
    val result = expr
    if (curToken.tag != TokenType.EOF) new CompileError
    else result
  }

  private def nextToken: Unit = {
    while (pos < exprLen && exprChars(pos) == ' ') pos += 1
    if (pos >= exprLen) curToken = Token(TokenType.EOF)
    else {
      val startPos = pos
      pos += 1
      curToken = exprChars(startPos) match {
        case char if char == '+'
          || char == '*'
          || char == '('
          || char == ')' => Token(TokenType.OPERATOR, char)
        case char if '0' <= char && char <= '9' =>
          while (pos < exprLen && '0' <= exprChars(pos) && exprChars(pos) <= '9') pos += 1
          Token(TokenType.NUM, exprChars.slice(startPos, pos).mkString.toInt)
        case _ => Token(TokenType.ERROR)
      }
    }
  }

  private def expr: Result = {
    val termResult = term
    if (termResult.hasCompileError) return new CompileError
    var result = termResult.getValue
    while (curToken.tag == TokenType.OPERATOR && curToken.value.asInstanceOf[Char] == '+') {
      nextToken
      val nextResult = term
      if (nextResult.hasCompileError) return new CompileError
      result += nextResult.getValue
    }
    new Result(result, false)
  }

  private def term: Result = {
    val factorResult = factor
    if (factorResult.hasCompileError) return new CompileError
    var result = factorResult.getValue
    while (curToken.tag == TokenType.OPERATOR && curToken.value.asInstanceOf[Char] == '*') {
      nextToken
      val nextResult = factor
      if (nextResult.hasCompileError) return new CompileError
      result *= nextResult.getValue
    }
    new Result(result, false)
  }

  private def factor: Result = {
    if (curToken.tag == TokenType.OPERATOR && curToken.value.asInstanceOf[Char] == '(') {
      nextToken
      val result = expr
      if (!result.hasCompileError && curToken.tag == TokenType.OPERATOR && curToken.value.asInstanceOf[Char] == ')') {
        nextToken
        new Result(result.getValue, false)
      } else new CompileError
    } else if (curToken.tag == TokenType.NUM) {
      val result = new Result(curToken.value.asInstanceOf[Int], false)
      nextToken
      result
    } else new CompileError
  }
}

case class Token(tag: TokenType, value: Any = null)

class CompileError extends Result(-1, true)

object TokenType extends Enumeration {
  type TokenType = Value
  val NUM = Value
  val OPERATOR = Value
  val EOF = Value
  val ERROR = Value
}

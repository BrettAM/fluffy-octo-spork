package com
import scala.util.parsing.combinator._

object XrandrParser extends JavaTokenParsers {
    def main: Parser[List[Monitor]] = line ~> rep(monitor) ^^ {
        case monitors => monitors
    }
    def monitor: Parser[Monitor] = statusLine ~ rep(resLine) ^^ {
        case monitor~resolutions => monitor.resList = resolutions; monitor
    }
    def statusLine: Parser[Monitor] =
            word ~ conState ~ (resBlock | "".r) ~ rotation ~ reflection <~ line ^^ {
        case name~cntd~resPos~rot~ref => {
            val (res, pos, offstate) = resPos match {
                case rp: (Res @unchecked,Res @unchecked, Boolean @unchecked) => rp
                case _ => (new Res(0,0), new Res(0,0), false)
            }
            new Monitor(name, cntd, res, pos, rot, ref, offstate)
        }
    }
    def conState: Parser[Boolean] = """\s*(connected|disconnected)""".r ^^ {
        case "connected" => true
        case _ => false
    }
    def resBlock: Parser[(Res,Res,Boolean)] =
        """\s*""".r ~> integer ~ "x" ~ integer ~ "+" ~ integer ~ "+" ~ integer ^^ {
            case rx~_~ry~_~px~_~py => (new Res(rx,ry),new Res(px, py),true)
        }
    def rotation: Parser[Rotation] = """\s*(right|left|inverted)?""".r ^^ {
        case "right" => Rotation.right
        case "left" => Rotation.left
        case "inverted" => Rotation.inverted
        case _ => Rotation.normal
    }
    def reflection: Parser[Reflection] = """\s*(X axis|Y axis|X and Y axis)?""".r ^^ {
        case "X axis" => Reflection.x
        case "Y axis" => Reflection.y
        case "X and Y axis" => Reflection.xy
        case _ => Reflection.normal
    }
    def resLine: Parser[Res] = resPair <~ line ^^ {
        case res => res
    }
    def resPair: Parser[Res] = integer ~ """x""" ~ integer ^^ {
        case x~_~y => new Res(x, y)
    }
    def word: Parser[String] = """\S*""".r ^^ { _.toString }
    def integer: Parser[Int] = wholeNumber ^^ {
        case x => x.toInt
    }
    def line: Parser[String] = """[^\n]*""".r ^^ {
        case line => line.toString
    }
    def apply(x: String) :List[Monitor] = {
        val res = parse(main, x)
        res match {
            case Success(matched, _) => matched
            case _ => Nil
        }
    }
}

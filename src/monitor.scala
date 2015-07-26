package com
import com.XrandrParser
import scala.util.parsing.combinator._
import scala.language.postfixOps
import sys.process._
class Res(val x: Int, val y: Int){
    def transpose: Res = new Res(y,x)
    override
    def toString() : String = x + "x" + y
}
sealed trait Rotation
package object Rotation{
    case object normal extends Rotation
    case object left extends Rotation
    case object right extends Rotation
    case object inverted extends Rotation
    val all = List(normal, left, right, inverted)
}
sealed trait Reflection
package object Reflection{
    case object normal extends Reflection
    case object x extends Reflection
    case object y extends Reflection
    case object xy extends Reflection
    val all = List(normal, x, y, xy)
}
object Monitor{
    def get(): List[Monitor] = {
        XrandrParser("xrandr" !!)
    }
}
class Monitor(
        val name: String,
        val connected: Boolean,
        activeRes: Res,
        var position: Res,
        var rotation: Rotation,
        var reflection: Reflection
            ){
    var resList: List[Res] = Nil
    var brightness = 1.0
    private[this] var rawRes = rotateRes(activeRes)
    def rotateRes(r: Res) = if(rotation == Rotation.right ||
                               rotation == Rotation.left) r.transpose
                            else r
    def resolution = rotateRes(rawRes)
    def resolution_= (r: Res) {
        rawRes = r
    }
    def publish(){
        val cmd = "xrandr --output " + name +
                    " --brightness " + brightness +
                    " --mode " + rawRes +
                    " --pos " + position +
                    " --rotate " + rotation +
                    " --reflect " + reflection
        cmd !;
        println(cmd);
    }
    override
    def toString() : String = name + " : " + activeRes + " " +
                                rotation + " " + reflection + " " +
                                resList.head + " ... (" + resList.length + " more)"
}
